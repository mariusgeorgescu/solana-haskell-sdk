
<p align="center">
  <a href="https://solana.com/docs">
    <img src="https://raw.githubusercontent.com/mariusgeorgescu/solana-haskell-sdk/main/solana-haskell-logo.jpeg" alt="Solana Haskell SDK Logo" width="425" />
  </a>
  <h1 align="center">Solana SDK library for Haskellers</h1>
  <p align="center">
    <a href="https://hackage.haskell.org/package/solana-haskell-sdk">
      <img src="https://img.shields.io/hackage/v/solana-haskell-sdk.svg?style=flat-square&logo=haskell&logoColor=white&label=Hackage" />
    </a>
    <a href="https://github.com/mariusgeorgescu/solana-haskell-sdk/actions/workflows/ci.yml">
      <img src="https://github.com/mariusgeorgescu/solana-haskell-sdk/actions/workflows/ci.yml/badge.svg?branch=main" />
    </a>
    <a href="https://github.com/mariusgeorgescu/solana-haskell-sdk/blob/main/CONTRIBUTING.md">
      <img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square" />
    </a>
  </p>
</p>

## Table of contents

- 📋 [Documentation](#documentation)
- 🧪 [Integration tests](#integration-tests)
- 🔌 [WebSocket subscriptions](#websocket-subscriptions)
- 🚀 [Features](#features)
- 🧑‍💻 [Usage Examples](#usage-examples)
- 📝 [Contributing](#contributing)
- ✨ [Credits](#credits)
- ⚖️ [License](#license)

## Documentation

API documentation is generated with Haddock (`cabal haddock`). Serialization correctness methodology: every instruction, message, and transaction encoder is asserted byte-identical to vectors generated from the official Rust crates (solana-sdk, spl-token, mpl-token-metadata) — see [tools/README.md](https://github.com/mariusgeorgescu/solana-haskell-sdk/blob/main/tools/README.md).

## Integration tests

An opt-in test suite runs the SDK against a local validator. It sits behind the `integration` cabal flag, so a plain `cabal test all` neither builds it nor pulls its extra dependencies:

    solana-test-validator --reset
    cabal test integration-tests --flags=integration

Without a validator listening on `127.0.0.1:8899` the suite skips itself and exits 0, so it stays green in CI even when enabled. Set `SOLANA_INTEGRATION=1` to make an unreachable validator a failure instead, or `SOLANA_RPC_URL` to target a different endpoint.

## WebSocket subscriptions

`Network.Solana.RPC.WebSocket` speaks Solana's PubSub protocol: signature and account subscriptions, and `awaitSignature`, which waits for a transaction to be *pushed* to you instead of polling for it.

The module is transport-agnostic, so the SDK carries no WebSocket dependency and the same code serves plain `ws://` and TLS `wss://` endpoints — you supply the connection. With the [`websockets`](https://hackage.haskell.org/package/websockets) package:

```haskell
import Network.Solana.RPC.WebSocket
import Network.WebSockets qualified as WS

WS.runClient "127.0.0.1" 8900 "/" $ \conn -> do
  let transport = WsTransport (WS.sendTextData conn) (WS.receiveData conn)
  result <- awaitSignature transport (RequestId 1) (Just "confirmed") 30 signature
  print result   -- Right <slot>, or Left with the on-chain or protocol error
```

For `wss://` endpoints use [`wuss`](https://hackage.haskell.org/package/wuss)'s `runSecureClient` in place of `runClient`; nothing else changes.

## Features

- [x] Full JSON-RPC API client (accounts, blocks, chain, ledger, tokens, tokenomics, transactions)
- [x] Wallet, account and keys management (ed25519 keypairs, base58 addresses)
- [x] Program Derived Addresses (`findProgramAddress` / `createProgramAddress`)
- [x] Transaction building, signing and submission
  - [x] Priority fees via Compute Budget (`setComputeUnitLimit`, `setComputeUnitPrice`)
  - [x] Versioned (v0) transactions and Address Lookup Tables
  - [x] Clients for native programs
    - [x] Address Lookup Table
    - [x] BPF Loader (upgradeable)
    - [x] Compute Budget
    - [x] Secp256k1 (instruction construction; signing out of scope)
    - [x] Stake (delegation lifecycle; seed-authority variants out of scope)
    - [x] System Program (all 13 instructions)
    - [x] Vote (account management)
  - [x] Clients for Solana Program Library (SPL)
    - [x] Memo
    - [x] SPL Token (instructions 0-20, incl. `transferChecked`)
    - [x] Associated Token Account (derive + create, idempotent variant)
  - [x] Metaplex Token Metadata (create/update metadata, master edition)
- [x] On-chain account state decoders (SPL token accounts and mints, lookup tables, stake and nonce accounts, Metaplex metadata)
- [x] PubSub (WebSocket) signature and account subscriptions, with push-based confirmation
- [x] Serialization verified byte-for-byte against the official Rust crates (solana-sdk, spl-token, mpl-token-metadata) (golden-vector tests)

### Release status

The current stable release is **v1.3.0.0**. Serialization is verified byte-for-byte against the official Rust SDK by golden-vector tests (see test/fixtures/ and [tools/README.md](https://github.com/mariusgeorgescu/solana-haskell-sdk/blob/main/tools/README.md)).

## Usage Examples

### Simple transfer

This example demonstrates how to use the Haskell Solana SDK to interact with a Solana validator and perform basic blockchain operations such as keypair generation, account funding via airdrop, balance checking, and transferring SOL tokens.

In the provided sample:

1. We start by generating a new keypair to act as the transaction sender and fee payer.

2. We connect to a local Solana validator using an HTTP provider.

3. The newly generated keypair receives an airdrop of 10 SOL, and we wait for it to be finalized (later sends are simulated against the finalized bank).

4. We define a recipient's public address from a base58 encoded string.

5. We **construct**, **sign** and **submit** the transaction by defining the signers and the transaction's list of instructions with their parameters.

Before and after performing the transfer of 1 SOL to the recipient, we check and print the account balances to verify the transaction's success. The second read happens after waiting for the transfer to finalize: `getBalance` reads at the node's default commitment, `finalized`, so a balance read right after a merely `confirmed` transaction would still show the old value.

This straightforward example highlights the convenience and expressiveness of Haskell when building decentralized applications on Solana.

> The examples use `GHC2021` (this package's `default-language`). If you compile them under `Haskell2010`, additionally enable `NumericUnderscores` and `ImportQualifiedPost`.

> The programs need `solana-haskell-sdk` and `web3-provider` (the package providing `Network.Web3.Provider`, i.e. `runWeb3'` and `HttpProvider`) in `build-depends`. RPC failures such as a rejected preflight simulation are raised as a `JsonRpcException` from `runWeb3'`, not returned as `Left`. Each example is also built as an executable of this package (`solana-haskell-sdk`, `example-spl-transfer`, `example-priority-fee`) and runs as-is against `solana-test-validator`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Transaction
import Network.Solana.SolanaWeb3
import Network.Web3.Provider

main :: IO ()
main = do
  -- Generate keypairs for fee payer (sender)
  (myPublicKey, myPrivateKey) <- createSolanaKeyPair

  -- Create Connection, local validator in this example
  result <- runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    -- Fund the fee payer and wait until the airdrop is finalized: sends are
    -- preflighted against the finalized bank, and balances are read there too
    requestAirdrop myPublicKey 10_000_000_000 >>= confirmFinalized

    -- Define recipient's address from a base58-encoded string
    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4"

    -- Check balance
    printBalances [myPublicKey, recipient]

    -- Create a new transaction.
    txId <-
      newTransaction
        [myPrivateKey] -- Signing keys (the first key pays the fee)
        -- List of instructions
        [ SystemProgram.transfer
            myPublicKey -- sender address
            recipient -- recipient address
            1_000_000_000 -- amount to transfer 1 SOL
        ]
    liftIO $ putStrLn ("Transaction sent: " <> show txId)

    -- Wait for finalization (throws if the transaction failed on-chain or
    -- timed out), so the balances below reflect the transfer
    confirmFinalized txId
    printBalances [myPublicKey, recipient]

  either (\e -> putStrLn ("RPC error: " <> show e)) pure result
```

### SPL token transfer (Associated Token Accounts)

Tokens live in *associated token accounts* (ATAs) — program-derived addresses computed from the wallet and the mint. This example first creates a mint and funds the sender's token account (so it runs as-is against a local validator), then derives both ATAs with `getAssociatedTokenAddress`, creates the recipient's ATA if missing (idempotent, safe to include unconditionally), and moves tokens with the decimals-checked `transferChecked`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Network.Solana.Core.Crypto
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Tokenomics (getMinimumBalanceForRentExemption)
import Network.Solana.RPC.HTTP.Transaction (requestAirdrop)
import Network.Solana.SolanaWeb3
import Network.Solana.SplPrograms.AssociatedTokenAccount qualified as Ata
import Network.Solana.SplPrograms.Token qualified as Token
import Network.Web3.Provider

main :: IO ()
main = do
  (myPublicKey, myPrivateKey) <- createSolanaKeyPair
  (mint, mintPrivateKey) <- createSolanaKeyPair -- the new token's mint account

  result <- runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    requestAirdrop myPublicKey 10_000_000_000 >>= confirmFinalized

    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4" -- recipient wallet

        -- ATAs are PDAs of (wallet, token program, mint): derived, not generated.
        sourceAta = fromJust (Ata.getAssociatedTokenAddress myPublicKey mint)
        destinationAta = fromJust (Ata.getAssociatedTokenAddress recipient mint)

    -- Setup: create a mint with 6 decimals (we are its mint authority), our own
    -- token account, and mint 10 tokens into it. The mint keypair co-signs
    -- because createAccount requires the new account's signature.
    mintRent <- getMinimumBalanceForRentExemption 82 -- a mint account is 82 bytes
    setupTx <-
      newTransaction
        [myPrivateKey, mintPrivateKey]
        [ SystemProgram.createAccount myPublicKey mint mintRent 82 Token.tokenProgramId,
          Token.initializeMint2 mint 6 myPublicKey Nothing,
          Ata.createAssociatedTokenAccount myPublicKey myPublicKey mint,
          Token.mintTo mint sourceAta myPublicKey [] 10_000_000
        ]
    confirmFinalized setupTx

    -- Transfer 1 token to the recipient
    transferTx <-
      newTransaction
        [myPrivateKey]
        [ -- Create the recipient's token account if it does not exist yet (no-op otherwise).
          Ata.createAssociatedTokenAccountIdempotent
            myPublicKey -- funder (pays rent)
            recipient -- wallet that will own the ATA
            mint,
          -- Transfer 1 token (6 decimals); mint and decimals are verified on-chain.
          Token.transferChecked
            sourceAta -- source token account
            mint -- token mint
            destinationAta -- destination token account
            myPublicKey -- owner of the source account
            [] -- extra multisig signers (none)
            1_000_000 -- amount in base units
            6 -- decimals of the mint
        ]
    confirmFinalized transferTx

    -- Read back the recipient's token account (decoded on-chain state)
    account <- getTokenAccount destinationAta
    liftIO $ putStrLn ("Recipient token balance: " <> show (Token.taAmount <$> account))

  either (\e -> putStrLn ("RPC error: " <> show e)) pure result
```

### Priority fees and memo

Compute Budget instructions raise a transaction's scheduling priority by paying a fee per compute unit; a memo attaches a signed, human-readable note recorded on-chain. Both are ordinary `Instruction`s added to the same instruction list. The payer is funded first.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Solana.Core.Crypto
import Network.Solana.NativePrograms.ComputeBudget qualified as ComputeBudget
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Transaction (requestAirdrop)
import Network.Solana.SolanaWeb3
import Network.Solana.SplPrograms.Memo qualified as Memo
import Network.Web3.Provider

main :: IO ()
main = do
  (myPublicKey, myPrivateKey) <- createSolanaKeyPair

  result <- runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    requestAirdrop myPublicKey 10_000_000_000 >>= confirmFinalized

    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4"

    txId <-
      newTransaction
        [myPrivateKey]
        [ ComputeBudget.setComputeUnitLimit 200_000, -- cap the compute units this tx may use
          ComputeBudget.setComputeUnitPrice 10_000, -- priority fee: micro-lamports per compute unit
          SystemProgram.transfer myPublicKey recipient 1_000_000_000, -- 1 SOL
          Memo.buildMemo "thanks for the coffee" [myPublicKey] -- signed on-chain note
        ]
    confirmFinalized txId
    printBalances [recipient]

  either (\e -> putStrLn ("RPC error: " <> show e)) pure result
```

## Contributing

We welcome all contributors! See [contributing guide](https://github.com/mariusgeorgescu/solana-haskell-sdk/blob/main/CONTRIBUTING.md) for how to get started.

## Credits

Created and maintained by Marius Georgescu.

## License

[Apache-2.0](https://github.com/mariusgeorgescu/solana-haskell-sdk/blob/main/LICENSE)
