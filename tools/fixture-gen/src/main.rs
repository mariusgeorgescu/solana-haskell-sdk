use serde_json::json;
use solana_sdk::{
    compute_budget::ComputeBudgetInstruction,
    hash::Hash,
    instruction::{AccountMeta, Instruction},
    message::Message,
    program_option::COption,
    pubkey::Pubkey,
    signature::keypair_from_seed,
    signer::Signer,
    system_instruction::{self, SystemInstruction},
    transaction::Transaction,
};
use solana_sdk::stake::instruction::{LockupArgs, StakeInstruction};
use solana_sdk::stake::state::{Authorized, Lockup, StakeAuthorize};
use solana_sdk::vote::instruction::VoteInstruction;
use solana_sdk::vote::state::{VoteAuthorize, VoteInit};
use solana_sdk::stake::{self};
use solana_sdk::vote::{self};
use solana_sdk::loader_upgradeable_instruction::UpgradeableLoaderInstruction;
use solana_sdk::secp256k1_instruction::new_secp256k1_instruction;
use spl_token::instruction::{AuthorityType, TokenInstruction};
use std::fs;
use std::str::FromStr;

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn main() {
    fs::create_dir_all("../../test/fixtures").unwrap();

    let base = Pubkey::new_from_array([3u8; 32]);
    let owner = Pubkey::new_from_array([5u8; 32]);
    let authority = Pubkey::new_from_array([6u8; 32]);
    let seed = "hello-seed".to_string();
    let lamports: u64 = 1_000_000;
    let space: u64 = 165;

    let variants: Vec<(&str, SystemInstruction)> = vec![
        ("CreateAccount", SystemInstruction::CreateAccount { lamports, space, owner }),
        ("Assign", SystemInstruction::Assign { owner }),
        ("Transfer", SystemInstruction::Transfer { lamports }),
        (
            "CreateAccountWithSeed",
            SystemInstruction::CreateAccountWithSeed {
                base,
                seed: seed.clone(),
                lamports,
                space,
                owner,
            },
        ),
        ("AdvanceNonceAccount", SystemInstruction::AdvanceNonceAccount),
        ("WithdrawNonceAccount", SystemInstruction::WithdrawNonceAccount(lamports)),
        ("InitializeNonceAccount", SystemInstruction::InitializeNonceAccount(authority)),
        ("AuthorizeNonceAccount", SystemInstruction::AuthorizeNonceAccount(authority)),
        ("Allocate", SystemInstruction::Allocate { space }),
        (
            "AllocateWithSeed",
            SystemInstruction::AllocateWithSeed { base, seed: seed.clone(), space, owner },
        ),
        (
            "AssignWithSeed",
            SystemInstruction::AssignWithSeed { base, seed: seed.clone(), owner },
        ),
        (
            "TransferWithSeed",
            SystemInstruction::TransferWithSeed {
                lamports,
                from_seed: seed.clone(),
                from_owner: owner,
            },
        ),
        ("UpgradeNonceAccount", SystemInstruction::UpgradeNonceAccount),
    ];

    let instr_fixtures: Vec<_> = variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&bincode::serialize(v).unwrap()) }))
        .collect();
    fs::write(
        "../../test/fixtures/system_instruction_data.json",
        serde_json::to_string_pretty(&instr_fixtures).unwrap(),
    )
    .unwrap();

    let payer = keypair_from_seed(&[1u8; 32]).unwrap();
    let recipient = Pubkey::new_from_array([2u8; 32]);
    let blockhash = Hash::new_from_array([9u8; 32]);

    let transfer_ix = system_instruction::transfer(&payer.pubkey(), &recipient, 1_000_000_000);
    let msg_transfer =
        Message::new_with_blockhash(&[transfer_ix.clone()], Some(&payer.pubkey()), &blockhash);

    let allocate_ix = system_instruction::allocate(&payer.pubkey(), 200);
    let msg_two = Message::new_with_blockhash(
        &[transfer_ix, allocate_ix],
        Some(&payer.pubkey()),
        &blockhash,
    );

    let msg_fixtures = vec![
        json!({ "name": "transfer-message", "hex": hex(&msg_transfer.serialize()) }),
        json!({ "name": "transfer-allocate-message", "hex": hex(&msg_two.serialize()) }),
    ];
    fs::write(
        "../../test/fixtures/messages.json",
        serde_json::to_string_pretty(&msg_fixtures).unwrap(),
    )
    .unwrap();

    let cb_variants: Vec<(&str, Instruction)> = vec![
        ("RequestHeapFrame", ComputeBudgetInstruction::request_heap_frame(32768)),
        ("SetComputeUnitLimit", ComputeBudgetInstruction::set_compute_unit_limit(200_000)),
        ("SetComputeUnitPrice", ComputeBudgetInstruction::set_compute_unit_price(1_000)),
        (
            "SetLoadedAccountsDataSizeLimit",
            ComputeBudgetInstruction::set_loaded_accounts_data_size_limit(65_536),
        ),
    ];
    let cb_fixtures: Vec<_> = cb_variants
        .iter()
        .map(|(name, ix)| json!({ "name": name, "hex": hex(&ix.data) }))
        .collect();
    fs::write(
        "../../test/fixtures/compute_budget_instruction_data.json",
        serde_json::to_string_pretty(&cb_fixtures).unwrap(),
    )
    .unwrap();

    let memo_program = Pubkey::from_str("MemoSq4gqABAXKb96qnH8TysNcWxMyWCqXgDLGmfcHr").unwrap();
    let memo_ix = Instruction {
        program_id: memo_program,
        accounts: vec![AccountMeta::new_readonly(payer.pubkey(), true)],
        data: b"hello-memo".to_vec(),
    };
    let memo_fixtures = vec![json!({ "name": "memo-data", "hex": hex(&memo_ix.data) })];
    fs::write(
        "../../test/fixtures/memo_instruction_data.json",
        serde_json::to_string_pretty(&memo_fixtures).unwrap(),
    )
    .unwrap();

    let priority_msg = Message::new_with_blockhash(
        &[
            ComputeBudgetInstruction::set_compute_unit_limit(200_000),
            ComputeBudgetInstruction::set_compute_unit_price(1_000),
            system_instruction::transfer(&payer.pubkey(), &recipient, 1_000_000_000),
            memo_ix.clone(),
        ],
        Some(&payer.pubkey()),
        &blockhash,
    );
    let priority_tx = Transaction::new(&[&payer], priority_msg, blockhash);

    let new_account = keypair_from_seed(&[10u8; 32]).unwrap();
    assert!(
        new_account.pubkey().to_bytes() < payer.pubkey().to_bytes(),
        "seed must yield a pubkey that sorts BEFORE the payer, so the fixture distinguishes pin-payer from sort-everything"
    );
    let create_ix = system_instruction::create_account(
        &payer.pubkey(),
        &new_account.pubkey(),
        1_000_000,
        165,
        &owner,
    );
    let two_signer_msg =
        Message::new_with_blockhash(&[create_ix], Some(&payer.pubkey()), &blockhash);
    let two_signer_tx = Transaction::new(&[&payer, &new_account], two_signer_msg, blockhash);

    let tx = Transaction::new(&[&payer], msg_transfer, blockhash);

    let wallet = Pubkey::new_from_array([11u8; 32]);
    let mint = Pubkey::new_from_array([12u8; 32]);
    let delegate = Pubkey::new_from_array([13u8; 32]);
    let amount: u64 = 1_000_000;
    let decimals: u8 = 6;

    let token_variants: Vec<(&str, TokenInstruction)> = vec![
        ("InitializeMint-some", TokenInstruction::InitializeMint { decimals, mint_authority: wallet, freeze_authority: COption::Some(delegate) }),
        ("InitializeMint-none", TokenInstruction::InitializeMint { decimals, mint_authority: wallet, freeze_authority: COption::None }),
        ("InitializeAccount", TokenInstruction::InitializeAccount),
        ("InitializeMultisig", TokenInstruction::InitializeMultisig { m: 2 }),
        ("Transfer", TokenInstruction::Transfer { amount }),
        ("Approve", TokenInstruction::Approve { amount }),
        ("Revoke", TokenInstruction::Revoke),
        ("SetAuthority-some", TokenInstruction::SetAuthority { authority_type: AuthorityType::AccountOwner, new_authority: COption::Some(delegate) }),
        ("SetAuthority-none", TokenInstruction::SetAuthority { authority_type: AuthorityType::CloseAccount, new_authority: COption::None }),
        ("SetAuthority-mint-tokens", TokenInstruction::SetAuthority { authority_type: AuthorityType::MintTokens, new_authority: COption::Some(delegate) }),
        ("SetAuthority-freeze", TokenInstruction::SetAuthority { authority_type: AuthorityType::FreezeAccount, new_authority: COption::None }),
        ("MintTo", TokenInstruction::MintTo { amount }),
        ("Burn", TokenInstruction::Burn { amount }),
        ("CloseAccount", TokenInstruction::CloseAccount),
        ("FreezeAccount", TokenInstruction::FreezeAccount),
        ("ThawAccount", TokenInstruction::ThawAccount),
        ("TransferChecked", TokenInstruction::TransferChecked { amount, decimals }),
        ("ApproveChecked", TokenInstruction::ApproveChecked { amount, decimals }),
        ("MintToChecked", TokenInstruction::MintToChecked { amount, decimals }),
        ("BurnChecked", TokenInstruction::BurnChecked { amount, decimals }),
        ("InitializeAccount2", TokenInstruction::InitializeAccount2 { owner: wallet }),
        ("SyncNative", TokenInstruction::SyncNative),
        ("InitializeAccount3", TokenInstruction::InitializeAccount3 { owner: wallet }),
        ("InitializeMultisig2", TokenInstruction::InitializeMultisig2 { m: 2 }),
        ("InitializeMint2-some", TokenInstruction::InitializeMint2 { decimals, mint_authority: wallet, freeze_authority: COption::Some(delegate) }),
    ];
    let token_fixtures: Vec<_> = token_variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&v.pack()) }))
        .collect();
    fs::write(
        "../../test/fixtures/token_instruction_data.json",
        serde_json::to_string_pretty(&token_fixtures).unwrap(),
    )
    .unwrap();

    let token_program = spl_token::id();
    let ata_program = spl_associated_token_account::id();
    let (generic_pda, generic_bump) =
        Pubkey::find_program_address(&[b"solana-haskell-sdk/1"], &token_program);
    assert!(generic_bump < 255, "generic PDA bump must be < 255 to prove the search runs");
    let ata = spl_associated_token_account::get_associated_token_address(&payer.pubkey(), &mint);
    let (_ata_check, ata_bump) = Pubkey::find_program_address(
        &[payer.pubkey().as_ref(), token_program.as_ref(), mint.as_ref()],
        &ata_program,
    );
    assert!(ata_bump < 255, "ATA bump must be < 255 to prove the search runs");
    let dest_ata = spl_associated_token_account::get_associated_token_address(&wallet, &mint);
    let program_id_fixed = Pubkey::new_from_array([23u8; 32]);
    let (programdata, _pd_bump) = Pubkey::find_program_address(
        &[program_id_fixed.as_ref()],
        &solana_sdk::bpf_loader_upgradeable::id(),
    );
    let pda_fixtures = vec![
        json!({ "name": "generic-pda-address", "hex": hex(&generic_pda.to_bytes()) }),
        json!({ "name": "generic-pda-bump", "hex": hex(&[generic_bump]) }),
        json!({ "name": "ata-address", "hex": hex(&ata.to_bytes()) }),
        json!({ "name": "ata-bump", "hex": hex(&[ata_bump]) }),
        json!({ "name": "dest-ata-address", "hex": hex(&dest_ata.to_bytes()) }),
        json!({ "name": "programdata-address", "hex": hex(&programdata.to_bytes()) }),
    ];
    fs::write(
        "../../test/fixtures/pda.json",
        serde_json::to_string_pretty(&pda_fixtures).unwrap(),
    )
    .unwrap();

    let create_dest_ata_ix =
        spl_associated_token_account::instruction::create_associated_token_account_idempotent(
            &payer.pubkey(),
            &wallet,
            &mint,
            &token_program,
        );
    let transfer_checked_ix = spl_token::instruction::transfer_checked(
        &token_program,
        &ata,
        &mint,
        &dest_ata,
        &payer.pubkey(),
        &[],
        amount,
        decimals,
    )
    .unwrap();
    let ata_msg = Message::new_with_blockhash(
        &[create_dest_ata_ix, transfer_checked_ix],
        Some(&payer.pubkey()),
        &blockhash,
    );
    let ata_tx = Transaction::new(&[&payer], ata_msg, blockhash);

    let stake_acct = keypair_from_seed(&[10u8; 32]).unwrap();
    let vote_acct = Pubkey::new_from_array([17u8; 32]);
    let custodian = Pubkey::new_from_array([18u8; 32]);
    let node = Pubkey::new_from_array([19u8; 32]);
    let new_auth = Pubkey::new_from_array([20u8; 32]);
    let authorized = Authorized { staker: payer.pubkey(), withdrawer: payer.pubkey() };
    let lockup = Lockup { unix_timestamp: 1_700_000_000, epoch: 300, custodian };

    let stake_variants: Vec<(&str, StakeInstruction)> = vec![
        ("Initialize", StakeInstruction::Initialize(authorized, lockup)),
        ("Authorize-staker", StakeInstruction::Authorize(new_auth, StakeAuthorize::Staker)),
        ("Authorize-withdrawer", StakeInstruction::Authorize(new_auth, StakeAuthorize::Withdrawer)),
        ("DelegateStake", StakeInstruction::DelegateStake),
        ("Split", StakeInstruction::Split(250_000)),
        ("Withdraw", StakeInstruction::Withdraw(500_000)),
        ("Deactivate", StakeInstruction::Deactivate),
        ("SetLockup-some", StakeInstruction::SetLockup(LockupArgs { unix_timestamp: Some(1_700_000_000), epoch: Some(300), custodian: Some(custodian) })),
        ("SetLockup-none", StakeInstruction::SetLockup(LockupArgs { unix_timestamp: None, epoch: None, custodian: None })),
        ("Merge", StakeInstruction::Merge),
        ("InitializeChecked", StakeInstruction::InitializeChecked),
        ("AuthorizeChecked", StakeInstruction::AuthorizeChecked(StakeAuthorize::Withdrawer)),
        ("GetMinimumDelegation", StakeInstruction::GetMinimumDelegation),
    ];
    let stake_fixtures: Vec<_> = stake_variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&bincode::serialize(v).unwrap()) }))
        .collect();
    fs::write(
        "../../test/fixtures/stake_instruction_data.json",
        serde_json::to_string_pretty(&stake_fixtures).unwrap(),
    )
    .unwrap();

    let vote_init = VoteInit {
        node_pubkey: node,
        authorized_voter: payer.pubkey(),
        authorized_withdrawer: payer.pubkey(),
        commission: 5,
    };
    let vote_variants: Vec<(&str, VoteInstruction)> = vec![
        ("InitializeAccount", VoteInstruction::InitializeAccount(vote_init.clone())),
        ("Authorize-voter", VoteInstruction::Authorize(new_auth, VoteAuthorize::Voter)),
        ("Authorize-withdrawer", VoteInstruction::Authorize(new_auth, VoteAuthorize::Withdrawer)),
        ("Withdraw", VoteInstruction::Withdraw(500_000)),
        ("UpdateValidatorIdentity", VoteInstruction::UpdateValidatorIdentity),
        ("UpdateCommission", VoteInstruction::UpdateCommission(5)),
        ("AuthorizeChecked", VoteInstruction::AuthorizeChecked(VoteAuthorize::Withdrawer)),
    ];
    let vote_fixtures: Vec<_> = vote_variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&bincode::serialize(v).unwrap()) }))
        .collect();
    fs::write(
        "../../test/fixtures/vote_instruction_data.json",
        serde_json::to_string_pretty(&vote_fixtures).unwrap(),
    )
    .unwrap();

    let create_stake_ix = system_instruction::create_account(
        &payer.pubkey(),
        &stake_acct.pubkey(),
        1_000_000,
        200,
        &stake::program::id(),
    );
    let init_stake_ix = stake::instruction::initialize(&stake_acct.pubkey(), &authorized, &lockup);
    let delegate_ix = stake::instruction::delegate_stake(&stake_acct.pubkey(), &payer.pubkey(), &vote_acct);
    let stake_msg = Message::new_with_blockhash(
        &[create_stake_ix, init_stake_ix, delegate_ix],
        Some(&payer.pubkey()),
        &blockhash,
    );
    let stake_tx = Transaction::new(&[&payer, &stake_acct], stake_msg, blockhash);

    let tx_fixtures = vec![
        json!({ "name": "transfer-transaction", "hex": hex(&bincode::serialize(&tx).unwrap()) }),
        json!({ "name": "payer-pubkey", "hex": hex(&payer.pubkey().to_bytes()) }),
        json!({ "name": "priority-transfer-transaction", "hex": hex(&bincode::serialize(&priority_tx).unwrap()) }),
        json!({ "name": "two-signer-create-transaction", "hex": hex(&bincode::serialize(&two_signer_tx).unwrap()) }),
        json!({ "name": "new-account-pubkey", "hex": hex(&new_account.pubkey().to_bytes()) }),
        json!({ "name": "ata-transfer-transaction", "hex": hex(&bincode::serialize(&ata_tx).unwrap()) }),
        json!({ "name": "stake-setup-transaction", "hex": hex(&bincode::serialize(&stake_tx).unwrap()) }),
    ];
    fs::write(
        "../../test/fixtures/transactions.json",
        serde_json::to_string_pretty(&tx_fixtures).unwrap(),
    )
    .unwrap();

    let loader_variants: Vec<(&str, UpgradeableLoaderInstruction)> = vec![
        ("InitializeBuffer", UpgradeableLoaderInstruction::InitializeBuffer),
        ("Write", UpgradeableLoaderInstruction::Write { offset: 128, bytes: vec![0xAB; 64] }),
        ("DeployWithMaxDataLen", UpgradeableLoaderInstruction::DeployWithMaxDataLen { max_data_len: 1_048_576 }),
        ("Upgrade", UpgradeableLoaderInstruction::Upgrade),
        ("SetAuthority", UpgradeableLoaderInstruction::SetAuthority),
        ("Close", UpgradeableLoaderInstruction::Close),
        ("ExtendProgram", UpgradeableLoaderInstruction::ExtendProgram { additional_bytes: 4096 }),
        ("SetAuthorityChecked", UpgradeableLoaderInstruction::SetAuthorityChecked),
    ];
    let loader_fixtures: Vec<_> = loader_variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&bincode::serialize(v).unwrap()) }))
        .collect();
    fs::write(
        "../../test/fixtures/loader_instruction_data.json",
        serde_json::to_string_pretty(&loader_fixtures).unwrap(),
    )
    .unwrap();

    let secp_secret = libsecp256k1::SecretKey::parse(&[42u8; 32]).unwrap();
    let secp_ix = new_secp256k1_instruction(&secp_secret, b"hello-secp");
    let data = &secp_ix.data;
    // layout: [0]=count, [1..12]=offsets, [12..32]=eth, [32..96]=sig, [96]=recovery, [97..]=msg
    let secp_fixtures = vec![
        json!({ "name": "secp-full-instruction-data", "hex": hex(data) }),
        json!({ "name": "secp-eth-address", "hex": hex(&data[12..32]) }),
        json!({ "name": "secp-signature", "hex": hex(&data[32..96]) }),
        json!({ "name": "secp-recovery-id", "hex": hex(&data[96..97]) }),
        json!({ "name": "secp-message", "hex": hex(&data[97..]) }),
    ];
    fs::write(
        "../../test/fixtures/secp256k1.json",
        serde_json::to_string_pretty(&secp_fixtures).unwrap(),
    )
    .unwrap();

    println!("fixtures written to test/fixtures/");
}
