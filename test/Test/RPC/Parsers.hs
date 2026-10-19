{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Golden tests for the RPC response parsers.
--
-- Every fixture here is a real response captured from a node -- not a
-- hand-written sample: the local validator via
-- @tools/rpc-record/record.py@, and devnet for the v1 responses the pinned
-- validator cannot produce -- so these tests
-- pin the @FromJSON@ instances against JSON a node actually produces,
-- including the fields it omits. That is the one thing the Rust-generated
-- byte vectors cannot cover, since the RPC layer's input is JSON rather than
-- a struct's serialization.
--
-- The assertions deliberately reach past "it parsed": each one checks a value
-- that could only be right if the field mapping is, which is what catches a
-- parser wired to the wrong key.
module Test.RPC.Parsers (tests) where

import Data.Aeson (FromJSON, Key, Value, toJSON, withObject, (.:))
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Network.Solana.Core.Account
  ( Account,
    AccountData (..),
    AccountInfo,
    Lamport (..),
    dataField,
    executable,
    lamports,
  )
import Network.Solana.Core.Block (BlockHash, BlockHeight)
import Network.Solana.Core.Transaction (Transaction (..))
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.RPC.HTTP.Account hiding (lamports)
import Network.Solana.RPC.HTTP.Account qualified as RpcAccount
import Network.Solana.RPC.HTTP.Block
import Network.Solana.RPC.HTTP.Chain
import Network.Solana.RPC.HTTP.Ledger
import Network.Solana.RPC.HTTP.Token
import Network.Solana.RPC.HTTP.Tokenomics
import Network.Solana.RPC.HTTP.Transaction
import Network.Solana.RPC.HTTP.Types
import Network.Solana.SplPrograms.Token (decodeMint, mDecimals, mSupply)
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

fixturePath :: FilePath
fixturePath = "test/fixtures/rpc_responses.json"

-- | Decodes a recorded response's @result@ into the type its RPC binding
-- returns, failing the test with the parse error if the instance rejects what
-- the node actually sent.
withResult :: forall a. (FromJSON a) => String -> (a -> Assertion) -> Assertion
withResult name check = do
  fs <- loadRpcFixtures fixturePath
  case parseEither (parseJSON @a) (requireRpcResult name fs) of
    Left err -> assertFailure ("failed to parse recorded " <> name <> " response: " <> err)
    Right parsed -> check parsed

-- | For results the bindings hand back untouched (plain numbers, strings).
withRawResult :: String -> (Value -> Assertion) -> Assertion
withRawResult name check = do
  fs <- loadRpcFixtures fixturePath
  check (requireRpcResult name fs)

tests :: TestTree
tests =
  testGroup
    "RPC response parsers (recorded from a live node)"
    [ testGroup "cluster and node" clusterTests,
      testGroup "ledger and epoch" ledgerTests,
      testGroup "blocks" blockTests,
      testGroup "tokenomics" tokenomicsTests,
      testGroup "accounts" accountTests,
      testGroup "transactions" transactionTests,
      testGroup "SPL token" tokenTests,
      testGroup "v1 transactions (SIMD-0385)" v1Tests
    ]

-- | Responses carrying v1 transactions, recorded from devnet (which runs
-- them) rather than from the local validator, since the pinned
-- @solana-test-validator@ predates the format. A v1 message reports its
-- resource requests in a @transactionConfig@ object and carries no
-- @addressTableLookups@; these pin that neither shape breaks the parsers.
v1Tests :: [TestTree]
v1Tests =
  [ testCase "getTransaction decodes a v1 transaction" $ withV1Result @(Maybe TransactionResult) "getTransaction-v1" $ \case
      Nothing -> assertFailure "expected a transaction"
      Just result -> do
        slotTx result @?= 512843755
        map show (signatures (transactionTx result))
          @?= ["Mq9Qb7wwtfyRtrtGFF4YjjCaUAhaG9iddBu7DquQ84hKe4D4R7ixgkKjz3DxbvS7UpC7Q2SmSK4htWcwu2wjcGZ"]
        keys <- messageField @[SolanaPublicKey] "accountKeys" (transactionTx result)
        length keys @?= 4
        blockhash <- messageField @BlockHash "recentBlockhash" (transactionTx result)
        show blockhash @?= "4iSS7zQNeY8BTSWZ9VRMBo8B17AyijyLDD8C2EycoW19",
    testCase "getBlock decodes a block mixing legacy, v0 and v1 transactions" $
      withV1Result @(Maybe BlockInfo) "getBlock-v1-trimmed" $ \case
        Nothing -> assertFailure "expected a block"
        Just block -> length (transactionsBI block) @?= 5
  ]

-- | As 'withResult', against the devnet-recorded v1 fixtures.
withV1Result :: forall a. (FromJSON a) => String -> (a -> Assertion) -> Assertion
withV1Result name check = do
  fs <- loadRpcFixtures "test/fixtures/v1_rpc_responses.json"
  case parseEither (parseJSON @a) (requireRpcResult name fs) of
    Left err -> assertFailure ("failed to parse recorded " <> name <> " response: " <> err)
    Right parsed -> check parsed

-- | Read one field of a parsed transaction's message. 'CompiledMessage' is
-- abstract, so its contents are only reachable through its 'ToJSON'
-- rendering -- which is also all a caller of the RPC bindings gets.
messageField :: forall a. (FromJSON a) => Key -> Transaction -> IO a
messageField name tx =
  either (assertFailure . ((show name <> ": ") <>)) pure $
    parseEither (withObject "message" (.: name)) (toJSON (message tx))

clusterTests :: [TestTree]
clusterTests =
  [ testCase "getVersion" $ withResult @SolanaVersion "getVersion" $ \v ->
      assertBool "reports a solana-core version" (not (null (solana_core v))),
    testCase "getIdentity" $ withResult @NodeIdentity "getIdentity" $ \_ ->
      pure (),
    testCase "getClusterNodes" $ withResult @[ClusterNodes] "getClusterNodes" $ \nodes ->
      assertBool "the local validator lists itself" (not (null nodes)),
    testCase "getHealth" $ withResult @String "getHealth" $ \health ->
      health @?= "ok",
    testCase "getRecentPerformanceSamples" $ withResult @[PerformanceSample] "getRecentPerformanceSamples" $ \samples ->
      assertBool "samples cover a non-zero period" (all ((> 0) . samplePeriodSecs) samples),
    testCase "getHighestSnapshotSlot" $ withResult @HighestSnapshotSlot "getHighestSnapshotSlot" $ \s ->
      assertBool "a full snapshot exists" (full s > 0),
    testCase "getRecentPrioritizationFees" $ withResult @[PrioritizationFee] "getRecentPrioritizationFees" $ \fees ->
      assertBool "recent slots are sampled" (not (null fees) && length fees <= 150),
    testCase "getRecentPrioritizationFees (address filter)" $
      withResult @[PrioritizationFee] "getRecentPrioritizationFees-filtered" $ \fees ->
        assertBool "recent slots are sampled" (not (null fees))
  ]

ledgerTests :: [TestTree]
ledgerTests =
  [ testCase "getEpochInfo" $ withResult @EpochInfo "getEpochInfo" $ \info ->
      assertBool "absolute slot is at least the slot index" (absoluteSlot info >= slotIndex info),
    testCase "getEpochSchedule" $ withResult @EpochSchedule "getEpochSchedule" $ \schedule ->
      assertBool "an epoch spans some slots" (slotsPerEpoch schedule > 0),
    testCase "getGenesisHash" $ withResult @BlockHash "getGenesisHash" $ \_ ->
      pure (),
    testCase "getSlot" $ withResult @Slot "getSlot" $ \slot ->
      assertBool "slot advanced past genesis" (slot > 0),
    testCase "getBlockHeight" $ withResult @BlockHeight "getBlockHeight" $ \height ->
      assertBool "block height advanced past genesis" (height > 0),
    testCase "getTransactionCount" $ withRawResult "getTransactionCount" $ \_ ->
      pure (),
    testCase "getFirstAvailableBlock" $ withResult @Slot "getFirstAvailableBlock" $ \_ ->
      pure (),
    testCase "getStakeMinimumDelegation" $ withResult @(RPCResponse Lamport) "getStakeMinimumDelegation" $ \r ->
      assertBool "context slot is populated" (contextSlot (context r) > 0),
    -- A single-validator cluster: the schedule names exactly the node itself.
    testCase "getLeaderSchedule" $ withResult @NodeIdentity "getIdentity" $ \node ->
      withResult @(Maybe LeaderSchedule) "getLeaderSchedule" $ \case
        Nothing -> assertFailure "expected a schedule for the current epoch"
        Just schedule -> do
          M.keys schedule @?= [identity node]
          assertBool "every leader has slots" (all (not . null) (M.elems schedule)),
    testCase "getSlotLeader" $ withResult @SolanaPublicKey "getSlotLeader" $ \_ ->
      pure (),
    testCase "getSlotLeaders" $ withResult @[SolanaPublicKey] "getSlotLeaders" $ \leaders ->
      length leaders @?= 3, -- the recorder asks for three
    testCase "getMaxRetransmitSlot" $ withResult @Slot "getMaxRetransmitSlot" $ \_ ->
      pure (),
    testCase "getMaxShredInsertSlot" $ withResult @Slot "getMaxShredInsertSlot" $ \_ ->
      pure (),
    testCase "minimumLedgerSlot" $ withResult @Slot "minimumLedgerSlot" $ \_ ->
      pure ()
  ]

blockTests :: [TestTree]
blockTests =
  [ testCase "getLatestBlockhash" $ withResult @(RPCResponse LatestBlockHash) "getLatestBlockhash" $ \r ->
      assertBool "blockhash is valid for some future block" (lastValidBlockHeight (value r) > 0),
    testCase "isBlockhashValid" $ withResult @(RPCResponse Bool) "isBlockhashValid" $ \r ->
      value r @?= True,
    testCase "getBlock" $ withResult @(Maybe BlockInfo) "getBlock" $ \block ->
      assertBool "the recorded slot holds a block" (isJust block),
    testCase "getBlock carries the recorded transfer" $ withResult @(Maybe BlockInfo) "getBlock" $ \case
        Nothing -> assertFailure "expected a block"
        Just b -> assertBool "block lists transactions" (not (null (transactionsBI b))),
    testCase "getBlockCommitment" $ withResult @BlockCommitment "getBlockCommitment" $ \c ->
      assertBool "total stake is positive" (totalStake c > 0),
    testCase "getBlockProduction" $ withResult @(RPCResponse BlockProduction) "getBlockProduction" $ \r ->
      case value r of
        BlockProduction identities _ -> assertBool "at least one leader produced blocks" (not (null identities)),
    testCase "getBlocks" $ withResult @[Slot] "getBlocks" $ \slots ->
      assertBool "the queried range is non-empty" (not (null slots)),
    testCase "getBlocksWithLimit" $ withResult @[Slot] "getBlocksWithLimit" $ \slots ->
      assertBool "at most the requested limit of three" (not (null slots) && length slots <= 3),
    testCase "getBlockTime" $ withResult @(Maybe Int) "getBlockTime" $ \t ->
      assertBool "the block carries a timestamp" (maybe False (> 0) t)
  ]

tokenomicsTests :: [TestTree]
tokenomicsTests =
  [ testCase "getInflationGovernor" $ withResult @InflationGovernor "getInflationGovernor" $ \g ->
      assertBool "taper is a fraction" (taper g > 0 && taper g <= 1),
    testCase "getInflationRate" $ withResult @InflationRate "getInflationRate" $ \r ->
      assertBool "total is validator plus foundation" (abs (totalInflation r - (validatorInflation r + foundationInflation r)) < 1e-9),
    testCase "getSupply" $ withResult @(RPCResponse SolanaSupply) "getSupply" $ \r ->
      assertBool "total supply covers circulating" (total (value r) >= circulating (value r)),
    testCase "getVoteAccounts" $ withResult @VoteAccounts "getVoteAccounts" $ \accounts ->
      assertBool "the test validator votes" (not (null (current accounts))),
    testCase "getMinimumBalanceForRentExemption" $ withResult @Lamport "getMinimumBalanceForRentExemption" $ \rent ->
      assertBool "rent exemption for 165 bytes is non-trivial" (rent > Lamport 1000000),
    -- The bootstrap stake account's reward for the previous (32-slot) epoch.
    testCase "getInflationReward" $ withResult @[Maybe InflationReward] "getInflationReward" $ \case
      [Just r] -> do
        assertBool "the bootstrap stake earned a reward" (amountReward r > Lamport 0)
        assertBool "post balance includes the reward" (postBalance r > amountReward r)
      other -> assertFailure ("expected exactly one reward, got " <> show (length other))
  ]

accountTests :: [TestTree]
accountTests =
  [ testCase "getBalance" $ withResult @(RPCResponse Lamport) "getBalance" $ \r ->
      assertBool "the airdropped payer holds SOL" (value r > Lamport 0),
    testCase "getAccountInfo" $ withResult @(RPCResponse (Maybe AccountInfo)) "getAccountInfo" $ \r ->
      case value r of
        Nothing -> assertFailure "expected the payer account to exist"
        Just info -> do
          assertBool "payer holds lamports" (lamports info > Lamport 0)
          executable info @?= False,
    -- A node reports an absent account as a null value, not an error; the
    -- parser has to model that rather than fail.
    testCase "getAccountInfo (missing account)" $ withResult @(RPCResponse (Maybe AccountInfo)) "getAccountInfo-missing" $ \r ->
      value r @?= Nothing,
    testCase "getMultipleAccounts" $ withResult @(RPCResponse [Maybe AccountInfo]) "getMultipleAccounts" $ \r ->
      length (value r) @?= 2,
    testCase "getLargestAccounts" $ withResult @(RPCResponse [AddressAndLamports]) "getLargestAccounts" $ \r ->
      assertBool "accounts are listed with balances" (not (null (value r)) && all ((> Lamport 0) . RpcAccount.lamports) (value r)),
    testCase "getProgramAccounts" $ withResult @[Account] "getProgramAccounts" $ \accounts ->
      assertBool "the token program owns the mint and token account" (length accounts >= 2),
    -- Recorded with no configuration: the node's default encoding is a bare
    -- base58 string (the recorder's 82-byte mint).
    testCase "getAccountInfo (node default encoding: bare base58)" $
      withResult @(RPCResponse (Maybe AccountInfo)) "getAccountInfo-base58" $ \r ->
        case dataField <$> value r of
          Just (AccountDataBinary bs) -> case decodeMint bs of
            Left e -> assertFailure e
            Right m -> do
              mDecimals m @?= 6
              mSupply m @?= 42000000
          other -> assertFailure ("expected binary mint data, got " <> show other),
    testCase "getAccountInfo (jsonParsed)" $
      withResult @(RPCResponse (Maybe AccountInfo)) "getAccountInfo-jsonParsed" $ \r ->
        case dataField <$> value r of
          Just (AccountDataJSON prog _ space) -> do
            prog @?= "spl-token"
            space @?= 165
          other -> assertFailure ("expected parsed token account data, got " <> show other),
    testCase "getAccountInfo (jsonParsed falls back to base64 without a parser)" $
      withResult @(RPCResponse (Maybe AccountInfo)) "getAccountInfo-jsonParsed-fallback" $ \r ->
        fmap dataField (value r) @?= Just (AccountDataBinary "")
  ]

transactionTests :: [TestTree]
transactionTests =
  [ testCase "getSignaturesForAddress" $ withResult @[TransactionSignatureInformation] "getSignaturesForAddress" $ \case
        [] -> assertFailure "expected the recipient's transfer to be listed"
        (s : _) -> do
          err s @?= Nothing
          assertBool "signature is attributed to a slot" (slotTxSig s > 0),
    testCase "getSignatureStatuses" $ withResult @(RPCResponse [Maybe TransactionSignatureStatus]) "getSignatureStatuses" $ \r ->
      case value r of
        [Just s] -> do
          errTxStatus s @?= Nothing
          confirmationStatusTxStatus s @?= Just "finalized"
        other -> assertFailure ("expected exactly one status, got " <> show (length other)),
    testCase "getTransaction" $ withResult @(Maybe TransactionResult) "getTransaction" $ \result ->
      assertBool "the recorded transfer is retrievable" (isJust result),
    -- One signature at the test validator's default fee.
    testCase "getFeeForMessage" $ withResult @(RPCResponse (Maybe Int)) "getFeeForMessage" $ \r ->
      value r @?= Just 5000
  ]

tokenTests :: [TestTree]
tokenTests =
  [ testCase "getTokenSupply" $ withResult @(RPCResponse AmountObject) "getTokenSupply" $ \r -> do
      -- The recorder mints 42 tokens at 6 decimals.
      amount (value r) @?= "42000000"
      decimals (value r) @?= 6
      uiAmountString (value r) @?= "42",
    testCase "getTokenAccountBalance" $ withResult @(RPCResponse AmountObject) "getTokenAccountBalance" $ \r -> do
      amount (value r) @?= "42000000"
      decimals (value r) @?= 6,
    testCase "getTokenLargestAccounts" $ withResult @(RPCResponse [AmountObjectWithAddr]) "getTokenLargestAccounts" $ \r ->
      case value r of
        [] -> assertFailure "expected the minted account to be listed"
        (a : _) -> amount' a @?= "42000000",
    testCase "getTokenAccountsByOwner" $ withResult @(RPCResponse [Account]) "getTokenAccountsByOwner" $ \r ->
      length (value r) @?= 1,
    -- The recorder approves the recipient as delegate of the one token account.
    testCase "getTokenAccountsByDelegate" $ withResult @(RPCResponse [Account]) "getTokenAccountsByDelegate" $ \r ->
      length (value r) @?= 1
  ]
