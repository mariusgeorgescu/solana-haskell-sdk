use serde_json::json;
use solana_sdk::{
    hash::Hash,
    message::Message,
    pubkey::Pubkey,
    signature::keypair_from_seed,
    signer::Signer,
    system_instruction::{self, SystemInstruction},
    transaction::Transaction,
};
use std::fs;

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

    let tx = Transaction::new(&[&payer], msg_transfer, blockhash);
    let tx_fixtures = vec![
        json!({ "name": "transfer-transaction", "hex": hex(&bincode::serialize(&tx).unwrap()) }),
        json!({ "name": "payer-pubkey", "hex": hex(&payer.pubkey().to_bytes()) }),
    ];
    fs::write(
        "../../test/fixtures/transactions.json",
        serde_json::to_string_pretty(&tx_fixtures).unwrap(),
    )
    .unwrap();

    println!("fixtures written to test/fixtures/");
}
