#!/usr/bin/env python3
"""Record real Solana JSON-RPC responses as golden fixtures.

The SDK's response parsers are the one layer that byte-level golden vectors
cannot cover: their input is a node's JSON, not a Rust struct's serialization.
This script drives a local validator into a known state (a funded account, a
transfer, an SPL mint with a token account holding a balance and a delegate),
calls every RPC method the SDK binds except requestAirdrop and sendTransaction
(they mutate the ledger) and simulateTransaction (it needs a signed transaction
as input and returns an untyped Object), and writes the raw responses to
test/fixtures/rpc_responses.json.

Recording is additive and deliberate: run it, inspect the diff, commit. The
test suite then replays the fixtures offline, so CI never needs a validator.
A method the node rejects aborts the run instead of being skipped, so the
fixture set cannot silently drift from this description.

Usage:
    solana-test-validator --reset --slots-per-epoch 32   # in another terminal
    python3 tools/rpc-record/record.py

--slots-per-epoch 32 is what makes getInflationReward recordable: on the
default 432,000-slot epoch the node cannot answer it until the first epoch
ends, so the script refuses to run on such a validator, waits for epoch 2, and
queries the bootstrap stake account's reward for the previous epoch.

Requires: solana and spl-token CLIs on PATH (they set up the on-chain state),
and a reachable validator (SOLANA_RPC_URL, default http://127.0.0.1:8899).
"""

import json
import os
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request

RPC_URL = os.environ.get("SOLANA_RPC_URL", "http://127.0.0.1:8899")
OUTPUT = os.path.join(os.path.dirname(__file__), "..", "..", "test", "fixtures", "rpc_responses.json")


def rpc(method, params=None):
    """Call one JSON-RPC method and return the full response envelope."""
    payload = {"jsonrpc": "2.0", "id": 1, "method": method}
    if params is not None:
        payload["params"] = params
    request = urllib.request.Request(
        RPC_URL,
        data=json.dumps(payload).encode(),
        headers={"Content-Type": "application/json"},
    )
    with urllib.request.urlopen(request, timeout=30) as response:
        return json.loads(response.read())


def cli(*args, check=True):
    """Run a solana-family CLI command, returning its stdout."""
    result = subprocess.run(args, capture_output=True, text=True)
    if check and result.returncode != 0:
        sys.exit("command failed: {}\n{}{}".format(" ".join(args), result.stdout, result.stderr))
    return result.stdout.strip()


def setup(workdir):
    """Create the on-chain state the token and transaction methods need."""
    payer_path = os.path.join(workdir, "payer.json")
    cli("solana-keygen", "new", "--no-bip39-passphrase", "-s", "-o", payer_path, "--force")
    payer = cli("solana-keygen", "pubkey", payer_path)

    recipient_path = os.path.join(workdir, "recipient.json")
    cli("solana-keygen", "new", "--no-bip39-passphrase", "-s", "-o", recipient_path, "--force")
    recipient = cli("solana-keygen", "pubkey", recipient_path)

    common = ["--url", RPC_URL, "--keypair", payer_path, "--commitment", "confirmed"]
    # The spl-token CLI takes the payer as --fee-payer/--owner rather than --keypair.
    token_common = ["--url", RPC_URL, "--fee-payer", payer_path, "--owner", payer_path]
    cli("solana", "airdrop", "10", payer, *common)
    print("airdropped 10 SOL to", payer)

    transfer = cli(
        "solana", "transfer", recipient, "1", "--allow-unfunded-recipient",
        "--output", "json", *common,
    )
    signature = json.loads(transfer)["signature"]
    print("transfer signature", signature)

    mint = json.loads(cli("spl-token", "create-token", "--decimals", "6", "--output", "json", *token_common))["commandOutput"]["address"]
    # create-account reports only a signature, so the node is the source of
    # truth for the associated token account's address.
    cli("spl-token", "create-account", mint, "--output", "json", *token_common)
    token_account = associated_token_account(payer, mint)
    # Name the recipient account explicitly: without it the CLI targets the
    # associated account of its configured wallet, not our payer.
    mint_output = cli("spl-token", "mint", mint, "42", token_account, "--output", "json", *token_common)
    print("mint", mint, "token account", token_account)

    # A delegate approval gives getTokenAccountsByDelegate a non-empty answer.
    approve_output = cli("spl-token", "approve", token_account, "1", recipient, "--output", "json", *token_common)

    # The token methods are queried at the node's default (finalized)
    # commitment, so the minted balance and the approval must be finalized
    # before recording -- otherwise the fixtures capture a supply of 0.
    wait_for_finalized(json.loads(mint_output)["signature"])
    wait_for_finalized(json.loads(approve_output)["signature"])

    # The transfer must be finalized before getTransaction and getBlock will
    # report it at the default commitment.
    wait_for_finalized(signature)
    return {
        "payer": payer,
        "payer_path": payer_path,
        "recipient": recipient,
        "signature": signature,
        "mint": mint,
        "token_account": token_account,
    }


def associated_token_account(owner, mint, attempts=60):
    """Ask the node for the token account just created for owner and mint.

    Queried at finalized commitment: the spl-token CLI reads at finalized too,
    so returning earlier would have the next command fail on an account it
    cannot see yet.
    """
    for _ in range(attempts):
        response = rpc("getTokenAccountsByOwner", [owner, {"mint": mint}, {"encoding": "base64", "commitment": "finalized"}])
        accounts = response.get("result", {}).get("value", [])
        if accounts:
            return accounts[0]["pubkey"]
        time.sleep(1)
    sys.exit("no token account for {} appeared under {}".format(mint, owner))


def wait_for_finalized(signature, attempts=60):
    for _ in range(attempts):
        statuses = rpc("getSignatureStatuses", [[signature], {"searchTransactionHistory": True}])
        value = statuses.get("result", {}).get("value", [None])[0]
        if value and value.get("confirmationStatus") == "finalized":
            return
        time.sleep(1)
    sys.exit("timed out waiting for {} to finalize".format(signature))


def wait_for_epoch(target, attempts=600):
    """Block until the node's epoch reaches target (about 13 s per 32-slot epoch)."""
    for _ in range(attempts):
        if rpc("getEpochInfo")["result"]["epoch"] >= target:
            return
        time.sleep(1)
    sys.exit("timed out waiting for epoch {}".format(target))


def wait_for_snapshot(attempts=300):
    """Block until the node has taken its first full snapshot; before that
    getHighestSnapshotSlot answers with error -32008."""
    for _ in range(attempts):
        if "result" in rpc("getHighestSnapshotSlot"):
            return
        time.sleep(1)
    sys.exit("timed out waiting for the first snapshot")


def methods(state):
    """Every method/params pair to record, in the order they are written."""
    payer, recipient = state["payer"], state["recipient"]
    signature, mint, token_account = state["signature"], state["mint"], state["token_account"]
    base64_config = {"encoding": "base64"}

    slot = rpc("getSlot", [{"commitment": "finalized"}])["result"]
    transaction = rpc("getTransaction", [signature, {"encoding": "json", "maxSupportedTransactionVersion": 0}])
    transaction_slot = transaction.get("result", {}).get("slot", slot)
    blockhash = rpc("getLatestBlockhash")["result"]["value"]["blockhash"]
    # The bootstrap validator's stake account: the one account that earns an
    # inflation reward on a fresh test validator.
    stake_program = "Stake11111111111111111111111111111111111111"
    stake_account = rpc(
        "getProgramAccounts",
        [stake_program, {"encoding": "base64", "dataSlice": {"offset": 0, "length": 0}}],
    )["result"][0]["pubkey"]
    # A signed-only transfer message (never submitted) gives getFeeForMessage
    # a real, script-controlled input: one signature at the default fee.
    fee_message = json.loads(cli(
        "solana", "transfer", recipient, "0.001", "--allow-unfunded-recipient",
        "--url", RPC_URL, "--keypair", state["payer_path"], "--sign-only", "--blockhash", blockhash,
        "--dump-transaction-message", "--output", "json",
    ))["message"]

    return [
        # Cluster and node
        ("getVersion", "getVersion", None),
        ("getIdentity", "getIdentity", None),
        ("getClusterNodes", "getClusterNodes", None),
        ("getHealth", "getHealth", None),
        ("getHighestSnapshotSlot", "getHighestSnapshotSlot", None),
        ("getRecentPrioritizationFees", "getRecentPrioritizationFees", None),
        ("getRecentPrioritizationFees-filtered", "getRecentPrioritizationFees", [[payer]]),
        # Ledger and epoch
        ("getEpochInfo", "getEpochInfo", None),
        ("getEpochSchedule", "getEpochSchedule", None),
        ("getGenesisHash", "getGenesisHash", None),
        ("getSlot", "getSlot", None),
        ("getBlockHeight", "getBlockHeight", None),
        ("getTransactionCount", "getTransactionCount", None),
        ("getFirstAvailableBlock", "getFirstAvailableBlock", None),
        ("getRecentPerformanceSamples", "getRecentPerformanceSamples", [2]),
        ("getStakeMinimumDelegation", "getStakeMinimumDelegation", None),
        ("getLeaderSchedule", "getLeaderSchedule", None),
        ("getSlotLeader", "getSlotLeader", None),
        ("getSlotLeaders", "getSlotLeaders", [transaction_slot, 3]),
        ("getMaxRetransmitSlot", "getMaxRetransmitSlot", None),
        ("getMaxShredInsertSlot", "getMaxShredInsertSlot", None),
        ("minimumLedgerSlot", "minimumLedgerSlot", None),
        # Blocks
        ("getLatestBlockhash", "getLatestBlockhash", None),
        ("isBlockhashValid", "isBlockhashValid", [blockhash, {"commitment": "processed"}]),
        ("getBlock", "getBlock", [transaction_slot, {"encoding": "json", "maxSupportedTransactionVersion": 0, "transactionDetails": "full", "rewards": False}]),
        ("getBlockCommitment", "getBlockCommitment", [transaction_slot]),
        ("getBlockProduction", "getBlockProduction", None),
        ("getBlocks", "getBlocks", [max(1, transaction_slot - 2), transaction_slot]),
        ("getBlocksWithLimit", "getBlocksWithLimit", [max(1, transaction_slot - 2), 3]),
        ("getBlockTime", "getBlockTime", [transaction_slot]),
        # Tokenomics
        ("getInflationGovernor", "getInflationGovernor", None),
        ("getInflationRate", "getInflationRate", None),
        ("getInflationReward", "getInflationReward", [[stake_account]]),
        ("getSupply", "getSupply", None),
        ("getVoteAccounts", "getVoteAccounts", None),
        ("getMinimumBalanceForRentExemption", "getMinimumBalanceForRentExemption", [165]),
        # Accounts
        ("getBalance", "getBalance", [payer]),
        ("getAccountInfo", "getAccountInfo", [payer, base64_config]),
        ("getAccountInfo-missing", "getAccountInfo", ["11111111111111111111111111111112", base64_config]),
        # No configuration: the node's default (bare base58) encoding, offered
        # only for accounts of at most 128 bytes -- the mint is 82.
        ("getAccountInfo-base58", "getAccountInfo", [mint]),
        # jsonParsed: the node's parsed view where it has a parser (SPL Token),
        # and the base64 pair where it has none (a plain system account).
        ("getAccountInfo-jsonParsed", "getAccountInfo", [token_account, {"encoding": "jsonParsed"}]),
        ("getAccountInfo-jsonParsed-fallback", "getAccountInfo", [payer, {"encoding": "jsonParsed"}]),
        ("getMultipleAccounts", "getMultipleAccounts", [[payer, recipient], base64_config]),
        ("getLargestAccounts", "getLargestAccounts", None),
        ("getProgramAccounts", "getProgramAccounts", ["TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", base64_config]),
        # Transactions
        ("getSignaturesForAddress", "getSignaturesForAddress", [recipient, {"limit": 3}]),
        ("getSignatureStatuses", "getSignatureStatuses", [[signature], {"searchTransactionHistory": True}]),
        ("getTransaction", "getTransaction", [signature, {"encoding": "json", "maxSupportedTransactionVersion": 0}]),
        ("getFeeForMessage", "getFeeForMessage", [fee_message, {"commitment": "processed"}]),
        # SPL Token
        ("getTokenSupply", "getTokenSupply", [mint]),
        ("getTokenAccountBalance", "getTokenAccountBalance", [token_account]),
        ("getTokenLargestAccounts", "getTokenLargestAccounts", [mint]),
        ("getTokenAccountsByOwner", "getTokenAccountsByOwner", [payer, {"mint": mint}, base64_config]),
        ("getTokenAccountsByDelegate", "getTokenAccountsByDelegate", [recipient, {"mint": mint}, base64_config]),
    ]


def main():
    try:
        health = rpc("getHealth")
    except urllib.error.URLError as err:
        sys.exit("no validator at {}: {}".format(RPC_URL, err))
    if health.get("result") != "ok":
        sys.exit("validator at {} is not healthy: {}".format(RPC_URL, health))
    if rpc("getEpochInfo")["result"]["slotsInEpoch"] > 1000:
        sys.exit("start the validator with --slots-per-epoch 32: getInflationReward needs a completed epoch")

    with tempfile.TemporaryDirectory() as workdir:
        state = setup(workdir)
        wait_for_epoch(2)
        wait_for_snapshot()
        recorded = []
        for name, method, params in methods(state):
            response = rpc(method, params)
            if "error" in response:
                sys.exit("{} failed: {}".format(name, response["error"]))
            entry = {"name": name, "method": method, "response": response}
            if params is not None:
                entry["params"] = params
            recorded.append(entry)

    with open(OUTPUT, "w") as handle:
        json.dump(recorded, handle, indent=2, sort_keys=False)
        handle.write("\n")

    print("wrote {} responses to {}".format(len(recorded), os.path.normpath(OUTPUT)))


if __name__ == "__main__":
    main()
