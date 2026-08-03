#!/usr/bin/env python3
"""Record real Solana JSON-RPC responses as golden fixtures.

The SDK's response parsers are the one layer that byte-level golden vectors
cannot cover: their input is a node's JSON, not a Rust struct's serialization.
This script drives a local validator into a known state (a funded account, a
transfer, an SPL mint with a token account holding a balance), calls every RPC
method the SDK exposes a parser for, and writes the raw responses to
test/fixtures/rpc_responses.json.

Recording is additive and deliberate: run it, inspect the diff, commit. The
test suite then replays the fixtures offline, so CI never needs a validator.

Usage:
    solana-test-validator --reset            # in another terminal
    python3 tools/rpc-record/record.py

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

    # The token methods are queried at the node's default (finalized)
    # commitment, so the minted balance must be finalized before recording --
    # otherwise the fixtures capture a supply of 0.
    wait_for_finalized(json.loads(mint_output)["signature"])

    # The transfer must be finalized before getTransaction and getBlock will
    # report it at the default commitment.
    wait_for_finalized(signature)
    return {
        "payer": payer,
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


def methods(state):
    """Every method/params pair to record, in the order they are written."""
    payer, recipient = state["payer"], state["recipient"]
    signature, mint, token_account = state["signature"], state["mint"], state["token_account"]
    base64_config = {"encoding": "base64"}

    slot = rpc("getSlot", [{"commitment": "finalized"}])["result"]
    transaction = rpc("getTransaction", [signature, {"encoding": "json", "maxSupportedTransactionVersion": 0}])
    transaction_slot = transaction.get("result", {}).get("slot", slot)
    blockhash = rpc("getLatestBlockhash")["result"]["value"]["blockhash"]

    return [
        # Cluster and node
        ("getVersion", "getVersion", None),
        ("getIdentity", "getIdentity", None),
        ("getClusterNodes", "getClusterNodes", None),
        ("getHealth", "getHealth", None),
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
        # Blocks
        ("getLatestBlockhash", "getLatestBlockhash", None),
        ("isBlockhashValid", "isBlockhashValid", [blockhash, {"commitment": "processed"}]),
        ("getBlock", "getBlock", [transaction_slot, {"encoding": "json", "maxSupportedTransactionVersion": 0, "transactionDetails": "full", "rewards": False}]),
        ("getBlockCommitment", "getBlockCommitment", [transaction_slot]),
        ("getBlockProduction", "getBlockProduction", None),
        ("getBlocks", "getBlocks", [max(1, transaction_slot - 2), transaction_slot]),
        # Tokenomics
        ("getInflationGovernor", "getInflationGovernor", None),
        ("getInflationRate", "getInflationRate", None),
        ("getInflationReward", "getInflationReward", [[payer], {"commitment": "finalized"}]),
        ("getSupply", "getSupply", None),
        ("getVoteAccounts", "getVoteAccounts", None),
        ("getMinimumBalanceForRentExemption", "getMinimumBalanceForRentExemption", [165]),
        # Accounts
        ("getBalance", "getBalance", [payer]),
        ("getAccountInfo", "getAccountInfo", [payer, base64_config]),
        ("getAccountInfo-missing", "getAccountInfo", ["11111111111111111111111111111112", base64_config]),
        ("getMultipleAccounts", "getMultipleAccounts", [[payer, recipient], base64_config]),
        ("getLargestAccounts", "getLargestAccounts", None),
        ("getProgramAccounts", "getProgramAccounts", ["TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", base64_config]),
        # Transactions
        ("getSignaturesForAddress", "getSignaturesForAddress", [recipient, {"limit": 3}]),
        ("getSignatureStatuses", "getSignatureStatuses", [[signature], {"searchTransactionHistory": True}]),
        ("getTransaction", "getTransaction", [signature, {"encoding": "json", "maxSupportedTransactionVersion": 0}]),
        # SPL Token
        ("getTokenSupply", "getTokenSupply", [mint]),
        ("getTokenAccountBalance", "getTokenAccountBalance", [token_account]),
        ("getTokenLargestAccounts", "getTokenLargestAccounts", [mint]),
        ("getTokenAccountsByOwner", "getTokenAccountsByOwner", [payer, {"mint": mint}, base64_config]),
    ]


def main():
    try:
        health = rpc("getHealth")
    except urllib.error.URLError as err:
        sys.exit("no validator at {}: {}".format(RPC_URL, err))
    if health.get("result") != "ok":
        sys.exit("validator at {} is not healthy: {}".format(RPC_URL, health))

    with tempfile.TemporaryDirectory() as workdir:
        state = setup(workdir)
        recorded, skipped = [], []
        for name, method, params in methods(state):
            response = rpc(method, params)
            if "error" in response:
                skipped.append("{} ({})".format(name, response["error"].get("message", "error")))
                continue
            entry = {"name": name, "method": method, "response": response}
            if params is not None:
                entry["params"] = params
            recorded.append(entry)

    with open(OUTPUT, "w") as handle:
        json.dump(recorded, handle, indent=2, sort_keys=False)
        handle.write("\n")

    print("wrote {} responses to {}".format(len(recorded), os.path.normpath(OUTPUT)))
    for entry in skipped:
        print("skipped:", entry)


if __name__ == "__main__":
    main()
