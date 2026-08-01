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
use solana_sdk::stake::state::{Authorized, Delegation, Lockup, Meta, Stake, StakeAuthorize, StakeStateV2};
use solana_sdk::stake::stake_flags::StakeFlags;
use solana_sdk::vote::instruction::VoteInstruction;
use solana_sdk::vote::state::{VoteAuthorize, VoteInit};
use solana_sdk::stake::{self};
use solana_sdk::loader_upgradeable_instruction::UpgradeableLoaderInstruction;
use solana_sdk::secp256k1_instruction::new_secp256k1_instruction;
use solana_sdk::address_lookup_table::{self, instruction::ProgramInstruction, AddressLookupTableAccount};
use solana_sdk::address_lookup_table::state::{AddressLookupTable, LookupTableMeta};
use borsh::BorshSerialize;
use solana_sdk::message::{v0, VersionedMessage};
use solana_sdk::nonce::state::{Data as NonceData, DurableNonce, State as NonceState, Versions as NonceVersions};
use solana_sdk::program_pack::Pack;
use solana_sdk::transaction::VersionedTransaction;
use spl_token::instruction::{AuthorityType, TokenInstruction};
use spl_token::state::{Account as TokenAccount, AccountState, Mint};
use mpl_token_metadata::accounts::{MasterEdition, Metadata};
use mpl_token_metadata::instructions::{
    CreateMasterEditionV3Builder, CreateMetadataAccountV3Builder, UpdateMetadataAccountV2Builder,
};
use mpl_token_metadata::types::{Collection, CollectionDetails, Creator, DataV2, Key, TokenStandard, UseMethod, Uses};
use std::borrow::Cow;
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

    let (lut_addr, lut_bump) = Pubkey::find_program_address(
        &[payer.pubkey().as_ref(), &12345u64.to_le_bytes()],
        &address_lookup_table::program::id(),
    );
    let alt_addr_a = Pubkey::new_from_array([29u8; 32]);
    let alt_addr_b = Pubkey::new_from_array([30u8; 32]);
    let alt_variants: Vec<(&str, ProgramInstruction)> = vec![
        ("CreateLookupTable", ProgramInstruction::CreateLookupTable { recent_slot: 12345, bump_seed: lut_bump }),
        ("FreezeLookupTable", ProgramInstruction::FreezeLookupTable),
        ("ExtendLookupTable", ProgramInstruction::ExtendLookupTable { new_addresses: vec![alt_addr_a, alt_addr_b] }),
        ("DeactivateLookupTable", ProgramInstruction::DeactivateLookupTable),
        ("CloseLookupTable", ProgramInstruction::CloseLookupTable),
    ];
    let alt_fixtures: Vec<_> = alt_variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&bincode::serialize(v).unwrap()) }))
        .collect();
    fs::write(
        "../../test/fixtures/alt_instruction_data.json",
        serde_json::to_string_pretty(&alt_fixtures).unwrap(),
    )
    .unwrap();

    let (metadata_pda, _metadata_bump) = Metadata::find_pda(&mint);
    let (master_edition_pda, _master_edition_bump) = MasterEdition::find_pda(&mint);

    let pda_fixtures = vec![
        json!({ "name": "generic-pda-address", "hex": hex(&generic_pda.to_bytes()) }),
        json!({ "name": "generic-pda-bump", "hex": hex(&[generic_bump]) }),
        json!({ "name": "ata-address", "hex": hex(&ata.to_bytes()) }),
        json!({ "name": "ata-bump", "hex": hex(&[ata_bump]) }),
        json!({ "name": "dest-ata-address", "hex": hex(&dest_ata.to_bytes()) }),
        json!({ "name": "programdata-address", "hex": hex(&programdata.to_bytes()) }),
        json!({ "name": "lookup-table-address", "hex": hex(&lut_addr.to_bytes()) }),
        json!({ "name": "lookup-table-bump", "hex": hex(&[lut_bump]) }),
        json!({ "name": "metadata-address", "hex": hex(&metadata_pda.to_bytes()) }),
        json!({ "name": "master-edition-address", "hex": hex(&master_edition_pda.to_bytes()) }),
    ];
    fs::write(
        "../../test/fixtures/pda.json",
        serde_json::to_string_pretty(&pda_fixtures).unwrap(),
    )
    .unwrap();

    let second_creator = Pubkey::new_from_array([33u8; 32]);
    let collection_key = Pubkey::new_from_array([34u8; 32]);

    let data_v2_full = DataV2 {
        name: "Solana Haskell NFT".to_string(),
        symbol: "SHSDK".to_string(),
        uri: "https://example.com/nft.json".to_string(),
        seller_fee_basis_points: 550,
        creators: Some(vec![
            Creator { address: payer.pubkey(), verified: true, share: 60 },
            Creator { address: second_creator, verified: false, share: 40 },
        ]),
        collection: Some(Collection { verified: false, key: collection_key }),
        uses: Some(Uses { use_method: UseMethod::Multiple, remaining: 10, total: 10 }),
    };
    let data_v2_minimal = DataV2 {
        name: "Solana Haskell NFT".to_string(),
        symbol: "SHSDK".to_string(),
        uri: "https://example.com/nft.json".to_string(),
        seller_fee_basis_points: 550,
        creators: None,
        collection: None,
        uses: None,
    };

    let create_v3_full_ix = CreateMetadataAccountV3Builder::new()
        .metadata(metadata_pda)
        .mint(mint)
        .mint_authority(payer.pubkey())
        .payer(payer.pubkey())
        .update_authority(payer.pubkey(), true)
        .data(data_v2_full.clone())
        .is_mutable(true)
        .collection_details(CollectionDetails::V1 { size: 0 })
        .instruction();

    let create_v3_minimal_ix = CreateMetadataAccountV3Builder::new()
        .metadata(metadata_pda)
        .mint(mint)
        .mint_authority(payer.pubkey())
        .payer(payer.pubkey())
        .update_authority(payer.pubkey(), true)
        .data(data_v2_minimal.clone())
        .is_mutable(true)
        .instruction();

    let update_some_ix = UpdateMetadataAccountV2Builder::new()
        .metadata(metadata_pda)
        .update_authority(payer.pubkey())
        .data(data_v2_full.clone())
        .new_update_authority(payer.pubkey())
        .primary_sale_happened(true)
        .is_mutable(true)
        .instruction();

    let update_none_ix = UpdateMetadataAccountV2Builder::new()
        .metadata(metadata_pda)
        .update_authority(payer.pubkey())
        .instruction();

    let master_edition_some_ix = CreateMasterEditionV3Builder::new()
        .edition(master_edition_pda)
        .mint(mint)
        .update_authority(payer.pubkey())
        .mint_authority(payer.pubkey())
        .payer(payer.pubkey())
        .metadata(metadata_pda)
        .max_supply(100)
        .instruction();

    let master_edition_none_ix = CreateMasterEditionV3Builder::new()
        .edition(master_edition_pda)
        .mint(mint)
        .update_authority(payer.pubkey())
        .mint_authority(payer.pubkey())
        .payer(payer.pubkey())
        .metadata(metadata_pda)
        .instruction();

    let metadata_fixtures = vec![
        json!({ "name": "CreateMetadataAccountV3-full", "hex": hex(&create_v3_full_ix.data) }),
        json!({ "name": "CreateMetadataAccountV3-minimal", "hex": hex(&create_v3_minimal_ix.data) }),
        json!({ "name": "UpdateMetadataAccountV2-some", "hex": hex(&update_some_ix.data) }),
        json!({ "name": "UpdateMetadataAccountV2-none", "hex": hex(&update_none_ix.data) }),
        json!({ "name": "CreateMasterEditionV3-some", "hex": hex(&master_edition_some_ix.data) }),
        json!({ "name": "CreateMasterEditionV3-none", "hex": hex(&master_edition_none_ix.data) }),
        json!({
            "name": "create-v3-full-accounts",
            "accounts": create_v3_full_ix.accounts.iter().map(|m| json!({
                "pubkey": hex(&m.pubkey.to_bytes()),
                "signer": m.is_signer,
                "writable": m.is_writable,
            })).collect::<Vec<_>>(),
        }),
    ];
    fs::write(
        "../../test/fixtures/metadata_instruction_data.json",
        serde_json::to_string_pretty(&metadata_fixtures).unwrap(),
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

    let lut_account = AddressLookupTableAccount {
        key: Pubkey::new_from_array([31u8; 32]),
        addresses: vec![recipient, alt_addr_a, alt_addr_b],
    };
    let v0_msg = v0::Message::try_compile(
        &payer.pubkey(),
        &[
            system_instruction::transfer(&payer.pubkey(), &recipient, 1_000_000_000),
            system_instruction::transfer(&payer.pubkey(), &alt_addr_a, 500_000),
        ],
        &[lut_account],
        blockhash,
    )
    .unwrap();
    let versioned = VersionedMessage::V0(v0_msg);
    let v0_tx = VersionedTransaction::try_new(versioned.clone(), &[&payer]).unwrap();

    let nonce_account_pk = Pubkey::new_from_array([37u8; 32]);
    // Computed here (rather than down by the nonce-account state fixture) so the
    // same `DurableNonce` value is reused for both: the durable-nonce transaction
    // below signs with it as the "recent blockhash", and the nonce-account state
    // fixture further down stores it as the account's current nonce.
    let nonce_blockhash = Hash::new_from_array([35u8; 32]);
    let durable_nonce = DurableNonce::from_blockhash(&nonce_blockhash);

    // Sponsored-fee transfer: `stake_acct` is the fee payer (signs first) even
    // though it is not referenced by the transfer instruction itself; `payer` is
    // the second signer, as the transfer's `from` account. Message::new_with_blockhash
    // pins the fee payer as account 0 regardless of whether it appears in any
    // instruction, so this yields a 2-signature transaction.
    let sponsored_msg = Message::new_with_blockhash(
        &[system_instruction::transfer(&payer.pubkey(), &recipient, 1_000_000_000)],
        Some(&stake_acct.pubkey()),
        &blockhash,
    );
    let sponsored_tx = Transaction::new(&[&stake_acct, &payer], sponsored_msg, blockhash);

    // Durable-nonce transfer: `Message::new_with_nonce` prepends a system
    // `AdvanceNonceAccount(nonce_account_pk, payer)` instruction ahead of the
    // transfer, but does not set `recent_blockhash` (it stays the default Hash).
    // Durable-nonce transactions instead sign with the nonce account's current
    // durable-nonce value in place of a recent blockhash, so we build an
    // unsigned transaction and `try_sign` it with `*durable_nonce.as_hash()` --
    // matching how Rust nonce-aware clients construct these offline.
    let nonce_msg = Message::new_with_nonce(
        vec![system_instruction::transfer(&payer.pubkey(), &recipient, 1_000_000_000)],
        Some(&payer.pubkey()),
        &nonce_account_pk,
        &payer.pubkey(),
    );
    let mut nonce_tx = Transaction::new_unsigned(nonce_msg);
    nonce_tx
        .try_sign(&[&payer], *durable_nonce.as_hash())
        .unwrap();

    let tx_fixtures = vec![
        json!({ "name": "transfer-transaction", "hex": hex(&bincode::serialize(&tx).unwrap()) }),
        json!({ "name": "payer-pubkey", "hex": hex(&payer.pubkey().to_bytes()) }),
        json!({ "name": "priority-transfer-transaction", "hex": hex(&bincode::serialize(&priority_tx).unwrap()) }),
        json!({ "name": "two-signer-create-transaction", "hex": hex(&bincode::serialize(&two_signer_tx).unwrap()) }),
        json!({ "name": "new-account-pubkey", "hex": hex(&new_account.pubkey().to_bytes()) }),
        json!({ "name": "ata-transfer-transaction", "hex": hex(&bincode::serialize(&ata_tx).unwrap()) }),
        json!({ "name": "stake-setup-transaction", "hex": hex(&bincode::serialize(&stake_tx).unwrap()) }),
        json!({ "name": "v0-message", "hex": hex(&versioned.serialize()) }),
        json!({ "name": "v0-transfer-transaction", "hex": hex(&bincode::serialize(&v0_tx).unwrap()) }),
        json!({ "name": "sponsored-transfer-transaction", "hex": hex(&bincode::serialize(&sponsored_tx).unwrap()) }),
        json!({ "name": "nonce-transfer-transaction", "hex": hex(&bincode::serialize(&nonce_tx).unwrap()) }),
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

    // --- On-chain account state fixtures (SPL token, ALT, nonce, stake, metadata) ---

    let token_account_full = TokenAccount {
        mint,
        owner: payer.pubkey(),
        amount: 5_000_000,
        delegate: COption::Some(delegate),
        state: AccountState::Frozen,
        is_native: COption::Some(2_039_280),
        delegated_amount: 1_000,
        close_authority: COption::Some(new_auth),
    };
    let mut token_account_full_buf = [0u8; TokenAccount::LEN];
    token_account_full.pack_into_slice(&mut token_account_full_buf);

    let token_account_minimal = TokenAccount {
        mint,
        owner: payer.pubkey(),
        amount: 5_000_000,
        delegate: COption::None,
        state: AccountState::Initialized,
        is_native: COption::None,
        delegated_amount: 0,
        close_authority: COption::None,
    };
    let mut token_account_minimal_buf = [0u8; TokenAccount::LEN];
    token_account_minimal.pack_into_slice(&mut token_account_minimal_buf);

    let mint_full = Mint {
        mint_authority: COption::Some(payer.pubkey()),
        supply: 1_000_000_000,
        decimals: 6,
        is_initialized: true,
        freeze_authority: COption::Some(new_auth),
    };
    let mut mint_full_buf = [0u8; Mint::LEN];
    mint_full.pack_into_slice(&mut mint_full_buf);

    let mint_minimal = Mint {
        mint_authority: COption::None,
        supply: 1_000_000_000,
        decimals: 6,
        is_initialized: true,
        freeze_authority: COption::None,
    };
    let mut mint_minimal_buf = [0u8; Mint::LEN];
    mint_minimal.pack_into_slice(&mut mint_minimal_buf);

    // ALT: `serialize_for_tests` is a public (non-cfg-test-gated) method on
    // `AddressLookupTable`, so no manual bincode/zero-buffer construction is needed.
    let lut_meta = LookupTableMeta {
        deactivation_slot: u64::MAX,
        last_extended_slot: 12345,
        last_extended_slot_start_index: 1,
        authority: Some(payer.pubkey()),
        _padding: 0,
    };
    let lut_table = AddressLookupTable {
        meta: lut_meta,
        addresses: Cow::Owned(vec![recipient, alt_addr_a, alt_addr_b]),
    };
    let lut_bytes = lut_table.serialize_for_tests().unwrap();

    // Nonce: `DurableNonce::from_blockhash` hashes its input (sha256 with a
    // "DURABLE_NONCE" domain prefix), so the durable-nonce bytes stored on-chain are
    // NOT the raw 32x35 blockhash. Emit the actual hashed bytes separately so the
    // Haskell test asserts against ground truth rather than the seed blockhash.
    // (`nonce_blockhash`/`durable_nonce` were computed earlier, alongside the
    // durable-nonce transaction fixture, and are reused here.)
    let nonce_data = NonceData::new(payer.pubkey(), durable_nonce, 5000);
    let nonce_versions = NonceVersions::new(NonceState::Initialized(nonce_data));
    let nonce_bytes = bincode::serialize(&nonce_versions).unwrap();
    let durable_nonce_bytes = durable_nonce.as_hash().to_bytes();

    let stake_meta = Meta {
        rent_exempt_reserve: 2_282_880,
        authorized,
        lockup,
    };
    let stake_delegation = Delegation {
        voter_pubkey: vote_acct,
        stake: 1_000_000,
        activation_epoch: 250,
        deactivation_epoch: u64::MAX,
        warmup_cooldown_rate: 0.25,
    };
    let stake_stake = Stake {
        delegation: stake_delegation,
        credits_observed: 42,
    };
    let stake_state = StakeStateV2::Stake(stake_meta, stake_stake, StakeFlags::empty());
    let stake_initialized_state = StakeStateV2::Initialized(stake_meta);
    let stake_bytes = bincode::serialize(&stake_state).unwrap();
    let stake_initialized_bytes = bincode::serialize(&stake_initialized_state).unwrap();

    let metadata_full = Metadata {
        key: Key::MetadataV1,
        update_authority: payer.pubkey(),
        mint,
        name: format!("{:\0<32}", "Solana Haskell NFT"),
        symbol: format!("{:\0<10}", "SHSDK"),
        uri: format!("{:\0<200}", "https://example.com/nft.json"),
        seller_fee_basis_points: 550,
        creators: Some(vec![
            Creator { address: payer.pubkey(), verified: true, share: 60 },
            Creator { address: second_creator, verified: false, share: 40 },
        ]),
        primary_sale_happened: false,
        is_mutable: true,
        edition_nonce: Some(253),
        token_standard: Some(TokenStandard::NonFungible),
        collection: Some(Collection { verified: false, key: collection_key }),
        uses: Some(Uses { use_method: UseMethod::Multiple, remaining: 10, total: 10 }),
        collection_details: None,
        programmable_config: None,
    };
    // metadata-account-legacy: truncate right after the is_mutable byte. Rather than
    // trusting a hand-computed offset, serialize a variant with every tail field set to
    // None (identical prefix, 6 trailing 1-byte Option::None tags for edition_nonce,
    // token_standard, collection, uses, collection_details, programmable_config) and
    // strip those 6 known tag bytes to find where the shared prefix ends.
    let metadata_tail_none = Metadata {
        edition_nonce: None,
        token_standard: None,
        collection: None,
        uses: None,
        collection_details: None,
        programmable_config: None,
        ..metadata_full.clone()
    };
    let metadata_full_bytes = metadata_full.try_to_vec().unwrap();
    let metadata_tail_none_bytes = metadata_tail_none.try_to_vec().unwrap();
    let metadata_legacy_len = metadata_tail_none_bytes.len() - 6;
    assert_eq!(
        metadata_full_bytes[..metadata_legacy_len],
        metadata_tail_none_bytes[..metadata_legacy_len],
    );
    // Cross-check against the hand-computed offset from the task brief:
    // key(1) + update_authority(32) + mint(32) + name(4+32) + symbol(4+10) + uri(4+200)
    // + seller_fee_basis_points(2) + creators(1 + 4 + 2*(32+1+1)) + primary_sale_happened(1)
    // + is_mutable(1) = 396.
    assert_eq!(metadata_legacy_len, 396);
    let metadata_legacy_bytes = &metadata_full_bytes[..metadata_legacy_len];

    let state_fixtures = vec![
        json!({ "name": "token-account", "hex": hex(&token_account_full_buf) }),
        json!({ "name": "token-account-minimal", "hex": hex(&token_account_minimal_buf) }),
        json!({ "name": "mint", "hex": hex(&mint_full_buf) }),
        json!({ "name": "mint-minimal", "hex": hex(&mint_minimal_buf) }),
        json!({ "name": "lookup-table", "hex": hex(&lut_bytes) }),
        json!({ "name": "nonce-account", "hex": hex(&nonce_bytes) }),
        json!({ "name": "nonce-durable-hash", "hex": hex(&durable_nonce_bytes) }),
        json!({ "name": "stake-account", "hex": hex(&stake_bytes) }),
        json!({ "name": "stake-account-initialized", "hex": hex(&stake_initialized_bytes) }),
        json!({ "name": "metadata-account", "hex": hex(&metadata_full_bytes) }),
        json!({ "name": "metadata-account-legacy", "hex": hex(metadata_legacy_bytes) }),
    ];
    fs::write(
        "../../test/fixtures/state_fixtures.json",
        serde_json::to_string_pretty(&state_fixtures).unwrap(),
    )
    .unwrap();

    println!("fixtures written to test/fixtures/");
}
