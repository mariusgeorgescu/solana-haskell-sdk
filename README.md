
<p align="center">
  <a href="https://solana.com/docs">
    <img src="solana-haskell-logo.jpeg" alt="Solana Haskell SDK Logo" width="425" />
  </a>
  <h1 align="center">Solana SDK library for Haskellers</h2>
  <p align="center">
    <a href="https://https://hackage.haskell.org/">
      <img src="https://img.shields.io/badge/-Haddock-5E5184?style=flat-square&logo=haskell&logoColor=white" />
    </a>
    <a href="./CONTRIBUTING.md">
      <img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square" />
    </a>
  </p>
</p>

## Table of contents

- 📋 [Documentation](#documentation)
- 🚀 [Features](#features)
- 📝 [Contributing](#contributing)
- ✨ [Credits](#credits)
- ⚖️ [License](#license)

## Documentation

## Features

- [x] [Full JSON RPC API](#usage-examples-for-json-rpc-api)
- [x] Wallet, account and keys management
- [x] Transaction Building 
  - [x] Clients for native programs
    - [x] System Program 
    - [ ] config
    - [ ] stake
    - [ ] vote
    - [ ] BPF Loader
    - [ ] Secp256k1
  - [ ] Clients for Solana Program Library (SPL)
  - [ ] Metaplex
  - [ ] More programs



### Current development status

There is currently **no stable release**. The SDK is actively developed and latest is `v1.0.0` which is an `alpha` release.





### Usage Examples 
### Simple transfer

This example demonstrates how to use the Haskell Solana SDK to interact with a Solana validator and perform basic blockchain operations such as keypair generation, account funding via airdrop, balance checking, and transferring SOL tokens.

In the provided sample:

1. We start by generating a new keypair to act as the transaction sender and fee payer.

2. We connect to a local Solana validator using an HTTP provider.

3. The newly generated keypair receives an airdrop of 10 SOL to ensure it has sufficient funds.

4. We define a recipient's public address from a base58 encoded string.

5. We **construct**, **sign** and **submit** the transaction by defining the signers and the transaction's list of instructions with their parameters.

Before and after performing the transfer of 1 SOL to the recipient, we check and print the account balances to verify the transaction's success.

This straightforward example highlights the convenience and expressiveness of Haskell when building decentralized applications on Solana.

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
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
  void $ runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    -- Fund fee payer
    void $ requestAirdrop myPublicKey 10_000_000_000
    wait 15 -- Wait 15 seconds be sure the tx was confirmed

    -- Define recipient's address from base58 hex ecoded string
    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4"

    -- Check balance
    printBalances [myPublicKey, recipient]

    -- Create a new transaction.
    _txId <-
      newTransaction
        [myPrivateKey] -- Signing keys (with all required signers)
        -- List of instructions
        [ SystemProgram.transfer
            myPublicKey -- sender address
            recipient -- recipient address
            1_000_000_000 -- amount to transfer 1 SOL
        ]

    wait 15 -- Wait 15 seconds be sure the tx was confirmed
    -- Check balance
    printBalances [myPublicKey, recipient]
```

#### System Program


## Contributing

We welcome all contributors! See [contributing guide](./CONTRIBUTING.md) for how to get started.

## Credits

## License