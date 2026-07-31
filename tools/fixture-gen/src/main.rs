use serde_json::json;
use solana_sdk::{
    compute_budget::ComputeBudgetInstruction,
    hash::Hash,
    instruction::{AccountMeta, Instruction},
    message::Message,
    pubkey::Pubkey,
    signature::keypair_from_seed,
    signer::Signer,
    system_instruction::{self, SystemInstruction},
    transaction::Transaction,
};
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
    let tx_fixtures = vec![
        json!({ "name": "transfer-transaction", "hex": hex(&bincode::serialize(&tx).unwrap()) }),
        json!({ "name": "payer-pubkey", "hex": hex(&payer.pubkey().to_bytes()) }),
        json!({ "name": "priority-transfer-transaction", "hex": hex(&bincode::serialize(&priority_tx).unwrap()) }),
        json!({ "name": "two-signer-create-transaction", "hex": hex(&bincode::serialize(&two_signer_tx).unwrap()) }),
        json!({ "name": "new-account-pubkey", "hex": hex(&new_account.pubkey().to_bytes()) }),
    ];
    fs::write(
        "../../test/fixtures/transactions.json",
        serde_json::to_string_pretty(&tx_fixtures).unwrap(),
    )
    .unwrap();

    println!("fixtures written to test/fixtures/");
}
