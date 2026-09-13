-- | Live SPL token lifecycle coverage: create a mint, derive both parties'
-- associated token accounts, mint the initial supply, transfer part of it,
-- and check the decoded mint and token account state matches.
module Test.Integration.Token (tests) where

import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto (createSolanaKeyPair)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Tokenomics (getMinimumBalanceForRentExemption)
import Network.Solana.SolanaWeb3 (getMint, getTokenAccount, newTransaction)
import Network.Solana.SplPrograms.AssociatedTokenAccount qualified as ATA
import Network.Solana.SplPrograms.Token qualified as Token
import Network.Web3.Provider (Web3)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Token"
    [ testCase "mint -> ATAs -> mintTo -> transferChecked, decoded state matches" (run tokenLifecycle),
      testCase "README SPL flow via newTransaction: setup tx, then idempotent ATA + transferChecked" (run readmeSplFlow)
    ]

-- | Creates a mint (payer as mint authority, no freeze authority), derives
-- the associated token accounts for the payer and a second wallet, mints
-- the initial supply into the payer's ATA, transfers part of it to the
-- second wallet's ATA, and checks the decoded mint and both token accounts
-- reflect the expected supply, decimals, authority, balances, and owners.
tokenLifecycle :: Web3 ()
tokenLifecycle = do
  (payerPk, payerSk) <- fundedKeypair 3_000_000_000
  (mintPk, mintSk) <- liftIO createSolanaKeyPair
  walletB <- fst <$> liftIO createSolanaKeyPair
  mintRent <- getMinimumBalanceForRentExemption 82
  _ <-
    sendAndConfirm
      [payerSk, mintSk]
      [ SystemProgram.createAccount payerPk mintPk mintRent 82 Token.tokenProgramId,
        Token.initializeMint2 mintPk 6 payerPk Nothing
      ]
  ataA <- requireJust "ATA A derivation" (ATA.getAssociatedTokenAddress payerPk mintPk)
  ataB <- requireJust "ATA B derivation" (ATA.getAssociatedTokenAddress walletB mintPk)
  _ <- sendAndConfirm [payerSk] [ATA.createAssociatedTokenAccount payerPk payerPk mintPk]
  _ <- sendAndConfirm [payerSk] [Token.mintTo mintPk ataA payerPk [] 1_000_000]
  _ <- sendAndConfirm [payerSk] [ATA.createAssociatedTokenAccount payerPk walletB mintPk]
  lastSig <- sendAndConfirm [payerSk] [Token.transferChecked ataA mintPk ataB payerPk [] 250_000 6]
  confirmFinalized lastSig
  mint <- requireJust "mint" =<< getMint mintPk
  liftIO ((Token.mSupply mint @?= 1_000_000) >> (Token.mDecimals mint @?= 6) >> (Token.mMintAuthority mint @?= Just payerPk))
  accA <- requireJust "token account A" =<< getTokenAccount ataA
  liftIO ((Token.taAmount accA @?= 750_000) >> (Token.taMint accA @?= mintPk) >> (Token.taOwner accA @?= payerPk))
  accB <- requireJust "token account B" =<< getTokenAccount ataB
  liftIO ((Token.taAmount accB @?= 250_000) >> (Token.taOwner accB @?= walletB))

-- | The README's SPL example, structurally verbatim: one setup transaction
-- (createAccount + initializeMint2 + createAssociatedTokenAccount + mintTo,
-- signed by the payer and the mint) and one transfer transaction
-- (createAssociatedTokenAccountIdempotent + transferChecked), both through
-- the public 'newTransaction' rather than the suite's 'sendAndConfirm'.
readmeSplFlow :: Web3 ()
readmeSplFlow = do
  (payerPk, payerSk) <- fundedKeypair 3_000_000_000
  (mintPk, mintSk) <- liftIO createSolanaKeyPair
  recipient <- fst <$> liftIO createSolanaKeyPair
  sourceAta <- requireJust "source ATA derivation" (ATA.getAssociatedTokenAddress payerPk mintPk)
  destinationAta <- requireJust "destination ATA derivation" (ATA.getAssociatedTokenAddress recipient mintPk)
  mintRent <- getMinimumBalanceForRentExemption 82
  setupTx <-
    newTransaction
      [payerSk, mintSk]
      [ SystemProgram.createAccount payerPk mintPk mintRent 82 Token.tokenProgramId,
        Token.initializeMint2 mintPk 6 payerPk Nothing,
        ATA.createAssociatedTokenAccount payerPk payerPk mintPk,
        Token.mintTo mintPk sourceAta payerPk [] 10_000_000
      ]
  confirmFinalized setupTx
  transferTx <-
    newTransaction
      [payerSk]
      [ ATA.createAssociatedTokenAccountIdempotent payerPk recipient mintPk,
        Token.transferChecked sourceAta mintPk destinationAta payerPk [] 1_000_000 6
      ]
  confirmFinalized transferTx
  source <- requireJust "source token account" =<< getTokenAccount sourceAta
  liftIO (Token.taAmount source @?= 9_000_000)
  destination <- requireJust "destination token account" =<< getTokenAccount destinationAta
  liftIO (Token.taAmount destination @?= 1_000_000)
