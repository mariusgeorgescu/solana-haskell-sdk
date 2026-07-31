# Stake + Vote Clients Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Byte-verified Stake (discriminants 0-7, 9, 10, 13) and Vote (0, 1, 3, 4, 5, 7) program clients.

**Architecture:** Same golden-vector pattern. Both programs use bincode (u32 LE discriminants) like SystemProgram. New helpers for bincode `Option` fields (u8 tag + value) and `Int64` timestamps. Fixtures from `solana_sdk::stake::instruction::StakeInstruction` and `solana_sdk::vote` (both already in the resolved solana-sdk 1.18 — no new crate expected; fall back to the `solana-vote-program` crate only if the vote module isn't exposed).

**Tech Stack:** Haskell (GHC 9.6.7, tasty/QuickCheck), Rust (solana-sdk 1.18) for fixtures only.

**Spec:** `docs/superpowers/specs/2026-08-01-stake-vote-design.md`

## Global Constraints

- Branch `development`. Commit per task. Never push. `cabal build all` warning-clean (`-Wall`). **No new Haskell dependencies.**
- Fixtures: additions only; existing entries byte-identical. CI never runs cargo.
- Fixed constants (Rust generator and Haskell tests MUST match):
  - stake-account keypair seed: 32 bytes of `10` (its pubkey already exists as fixture `new-account-pubkey`); vote-account pubkey: 32 bytes of `17`; custodian pubkey: 32 bytes of `18`; node pubkey: 32 bytes of `19`; new-authority pubkey: 32 bytes of `20`
  - payer keypair seed: 32 bytes of `1`; blockhash: 32 bytes of `9` (existing)
  - lamports: `1000000`; withdraw amount: `500000`; split amount: `250000`; stake space: `200`; commission: `5`; lockup unix_timestamp: `1700000000` (i64); lockup epoch: `300` (u64)
- Program ids: Stake `Stake11111111111111111111111111111111111111`; stake config `StakeConfig11111111111111111111111111111111`; Vote `Vote111111111111111111111111111111111111111`.
- Wire formats (ground truth = Rust crates): u32 LE discriminant; pubkeys raw 32 bytes; u64/i64 LE; bincode `Option<T>` = u8 tag 0 | 1 + T; `StakeAuthorize`/`VoteAuthorize` = u32 LE 0 (staker/voter) or 1 (withdrawer). Stake discriminants: Initialize=0, Authorize=1, DelegateStake=2, Split=3, Withdraw=4, Deactivate=5, SetLockup=6, Merge=7, InitializeChecked=9, AuthorizeChecked=10, GetMinimumDelegation=13. Vote discriminants: InitializeAccount=0, Authorize=1, Withdraw=3, UpdateValidatorIdentity=4, UpdateCommission=5, AuthorizeChecked=7. If the Rust-generated fixture bytes contradict a discriminant listed here, THE FIXTURE WINS — update the Haskell side and note the correction in your report.
- Match existing code style (explicit instance signatures, qualified imports, record AccountMeta, Haddock like SystemProgram's builders).

---

### Task 1: Fixture generator — stake and vote vectors

**Files:**
- Modify: `tools/fixture-gen/src/main.rs` (and `Cargo.toml` ONLY if `solana_sdk::vote` is unavailable)
- Create (generated): `test/fixtures/stake_instruction_data.json`, `test/fixtures/vote_instruction_data.json`
- Modify (generated): `test/fixtures/transactions.json` (one appended entry)

**Interfaces:**
- Produces stake entries: `Initialize`, `Authorize-staker`, `Authorize-withdrawer`, `DelegateStake`, `Split`, `Withdraw`, `Deactivate`, `SetLockup-some`, `SetLockup-none`, `Merge`, `InitializeChecked`, `AuthorizeChecked`, `GetMinimumDelegation`; vote entries: `InitializeAccount`, `Authorize-voter`, `Authorize-withdrawer`, `Withdraw`, `UpdateValidatorIdentity`, `UpdateCommission`, `AuthorizeChecked`; transactions: `stake-setup-transaction`.

- [ ] **Step 1: Extend the generator**

Add imports to `tools/fixture-gen/src/main.rs`:

```rust
use solana_sdk::stake::instruction::{LockupArgs, StakeInstruction};
use solana_sdk::stake::state::{Authorized, Lockup, StakeAuthorize};
use solana_sdk::vote::instruction::VoteInstruction;
use solana_sdk::vote::state::{VoteAuthorize, VoteInit};
use solana_sdk::stake::{self};
use solana_sdk::vote::{self};
```

(If `solana_sdk::vote::instruction` does not exist in 1.18.26, add `solana-vote-program = "1.18"` to Cargo.toml and import `solana_vote_program::vote_instruction::VoteInstruction` and `solana_vote_program::vote_state::{VoteAuthorize, VoteInit}` instead — document which path was taken.)

Append at the end of `main()`:

```rust
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
```

and append to `tx_fixtures` (LAST; existing entries unchanged):

```rust
        json!({ "name": "stake-setup-transaction", "hex": hex(&bincode::serialize(&stake_tx).unwrap()) }),
```

(If import paths differ in 1.18 — e.g. `stake::program::id()` vs `solana_sdk::stake::program::id()` — adapt minimally and document.)

- [ ] **Step 2: Regenerate and verify**

Run: `cd tools/fixture-gen && cargo run && cd ../..`
Then: additions-only check on transactions.json (`git diff test/fixtures/transactions.json | grep '^-' | grep -v '^---'` → nothing); 13 stake entries, 7 vote entries; spot-checks: `Withdraw` stake hex = `0400000020a1070000000000` (u32 LE 4 + u64 LE 500000); vote `UpdateCommission` hex = `0500000005`. Suite: `cabal test` → 106 still green. Record the ACTUAL hex of stake `AuthorizeChecked` and vote `AuthorizeChecked` in your report (their discriminants confirm/correct the constraint list: expected 0a000000... and 07000000...).

- [ ] **Step 3: Commit**

```bash
git add tools/fixture-gen/ test/fixtures/
git commit -m "test: golden vectors for stake and vote programs"
```

---

### Task 2: Stake instruction type + serialization

**Files:**
- Create: `src/Network/Solana/NativePrograms/Stake.hs` (type + Binary this task)
- Create: `test/Test/NativePrograms/Stake.hs`
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Produces: `stakeProgramId`, `stakeConfigId :: SolanaPublicKey`; `data Authorized = Authorized {aStaker, aWithdrawer :: SolanaPublicKey}`; `data Lockup = Lockup {lUnixTimestamp :: Int64, lEpoch :: Word64, lCustodian :: SolanaPublicKey}`; `data StakeAuthorize = AuthorizeStaker | AuthorizeWithdrawer`; `data LockupArgs = LockupArgs {laUnixTimestamp :: Maybe Int64, laEpoch :: Maybe Word64, laCustodian :: Maybe SolanaPublicKey}`; `data StakeInstruction` (11 constructors); helpers `putBincodeOption :: (a -> Put) -> Maybe a -> Put`, `getBincodeOption :: Get a -> Get (Maybe a)`. Task 3 adds builders.

- [ ] **Step 1: Write the failing tests**

Create `test/Test/NativePrograms/Stake.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.Stake (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeypairFromSeed, unsafeSolanaPublicKeyRaw)
import Network.Solana.NativePrograms.Stake qualified as Stake
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

custodianPk, newAuthPk :: SolanaPublicKey
custodianPk = unsafeSolanaPublicKeyRaw (replicate 32 18)
newAuthPk = unsafeSolanaPublicKeyRaw (replicate 32 20)

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

fixedAuthorized :: Stake.Authorized
fixedAuthorized = Stake.Authorized {Stake.aStaker = payerPk, Stake.aWithdrawer = payerPk}

fixedLockup :: Stake.Lockup
fixedLockup = Stake.Lockup {Stake.lUnixTimestamp = 1700000000, Stake.lEpoch = 300, Stake.lCustodian = custodianPk}

enc :: Stake.StakeInstruction -> BS.ByteString
enc = BL.toStrict . encode

genStakeInstruction :: Gen Stake.StakeInstruction
genStakeInstruction =
  oneof
    [ Stake.Initialize <$> genAuthorized <*> genLockup,
      Stake.Authorize <$> genPk <*> genAuth,
      pure Stake.DelegateStake,
      Stake.Split <$> arbitrary,
      Stake.Withdraw <$> arbitrary,
      pure Stake.Deactivate,
      Stake.SetLockup <$> genLockupArgs,
      pure Stake.Merge,
      pure Stake.InitializeChecked,
      Stake.AuthorizeChecked <$> genAuth,
      pure Stake.GetMinimumDelegation
    ]
  where
    genPk = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary
    genAuth = elements [Stake.AuthorizeStaker, Stake.AuthorizeWithdrawer]
    genAuthorized = Stake.Authorized <$> genPk <*> genPk
    genLockup = Stake.Lockup <$> arbitrary <*> arbitrary <*> genPk
    genLockupArgs =
      Stake.LockupArgs
        <$> oneof [pure Nothing, Just <$> arbitrary]
        <*> oneof [pure Nothing, Just <$> arbitrary]
        <*> oneof [pure Nothing, Just <$> genPk]

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/stake_instruction_data.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "Stake instruction data (golden + properties)"
      [ goldenCase getFixtures "Initialize" (Stake.Initialize fixedAuthorized fixedLockup),
        goldenCase getFixtures "Authorize-staker" (Stake.Authorize newAuthPk Stake.AuthorizeStaker),
        goldenCase getFixtures "Authorize-withdrawer" (Stake.Authorize newAuthPk Stake.AuthorizeWithdrawer),
        goldenCase getFixtures "DelegateStake" Stake.DelegateStake,
        goldenCase getFixtures "Split" (Stake.Split 250000),
        goldenCase getFixtures "Withdraw" (Stake.Withdraw 500000),
        goldenCase getFixtures "Deactivate" Stake.Deactivate,
        goldenCase getFixtures "SetLockup-some" (Stake.SetLockup (Stake.LockupArgs (Just 1700000000) (Just 300) (Just custodianPk))),
        goldenCase getFixtures "SetLockup-none" (Stake.SetLockup (Stake.LockupArgs Nothing Nothing Nothing)),
        goldenCase getFixtures "Merge" Stake.Merge,
        goldenCase getFixtures "InitializeChecked" Stake.InitializeChecked,
        goldenCase getFixtures "AuthorizeChecked" (Stake.AuthorizeChecked Stake.AuthorizeWithdrawer),
        goldenCase getFixtures "GetMinimumDelegation" Stake.GetMinimumDelegation,
        testProperty "Binary round-trip" $
          forAll genStakeInstruction $ \si -> decode (encode si) === si,
        testCase "decode fails on unknown discriminant" $
          case decodeOrFail (BL.pack [16, 0, 0, 0]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Stake.StakeInstruction) of
            Left _ -> pure ()
            Right _ -> assertFailure "expected decode failure for discriminant 16"
      ]
  where
    goldenCase getFixtures name si =
      testCase name $ do
        fs <- getFixtures
        enc si @?= requireFixture name fs
```

- [ ] **Step 2: Register and verify compile failure**

Cabal: `exposed-modules` gains `Network.Solana.NativePrograms.Stake` (alphabetically after `...NativePrograms.ComputeBudget`, before `...NativePrograms.SystemProgram`); test `other-modules` gains `Test.NativePrograms.Stake`; wire into `test/Main.hs`. Run → compile failure.

- [ ] **Step 3: Implement**

Create `src/Network/Solana/NativePrograms/Stake.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Stake program. Instruction data is bincode-encoded
-- (u32 little-endian discriminant), like the System Program.
-- Covered discriminants: 0-7, 9, 10, 13; the seed-authority variants (8, 11),
-- SetLockupChecked (12), DeactivateDelinquent (14) and the deprecated
-- Redelegate (15) are not modeled.
module Network.Solana.NativePrograms.Stake where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction

-- | Stake program address.
stakeProgramId :: SolanaPublicKey
stakeProgramId = "Stake11111111111111111111111111111111111111"

-- | Stake config account address (legacy; still passed to DelegateStake).
stakeConfigId :: SolanaPublicKey
stakeConfigId = "StakeConfig11111111111111111111111111111111"

-- | The authorities of a stake account.
data Authorized = Authorized
  { aStaker :: SolanaPublicKey,
    aWithdrawer :: SolanaPublicKey
  }
  deriving (Eq, Show, Generic)

instance Binary Authorized where
  put :: Authorized -> Put
  put (Authorized staker withdrawer) = do
    putByteString (getSolanaPublicKeyRaw staker)
    putByteString (getSolanaPublicKeyRaw withdrawer)
  get :: Get Authorized
  get = Authorized <$> get <*> get

-- | Lockup constraints on a stake account.
data Lockup = Lockup
  { lUnixTimestamp :: Int64,
    lEpoch :: Word64,
    lCustodian :: SolanaPublicKey
  }
  deriving (Eq, Show, Generic)

instance Binary Lockup where
  put :: Lockup -> Put
  put (Lockup ts epoch cust) = do
    putInt64le ts
    putWord64le epoch
    putByteString (getSolanaPublicKeyRaw cust)
  get :: Get Lockup
  get = Lockup <$> getInt64le <*> getWord64le <*> get

-- | Which stake authority an operation concerns (bincode u32: 0 staker, 1 withdrawer).
data StakeAuthorize = AuthorizeStaker | AuthorizeWithdrawer
  deriving (Eq, Show, Enum, Bounded, Generic)

putStakeAuthorize :: StakeAuthorize -> Put
putStakeAuthorize sa = putWord32le (fromIntegral (fromEnum sa))

getStakeAuthorize :: Get StakeAuthorize
getStakeAuthorize = do
  v <- getWord32le
  if v <= 1
    then pure (toEnum (fromIntegral v))
    else fail ("StakeAuthorize: invalid value " <> show v)

-- | Partial lockup update; bincode Option fields (u8 tag + value).
data LockupArgs = LockupArgs
  { laUnixTimestamp :: Maybe Int64,
    laEpoch :: Maybe Word64,
    laCustodian :: Maybe SolanaPublicKey
  }
  deriving (Eq, Show, Generic)

-- | bincode Option<T>: u8 tag 0 (None) | 1 followed by T.
putBincodeOption :: (a -> Put) -> Maybe a -> Put
putBincodeOption _ Nothing = putWord8 0
putBincodeOption p (Just x) = putWord8 1 >> p x

getBincodeOption :: Get a -> Get (Maybe a)
getBincodeOption g = do
  tag <- getWord8
  case tag of
    0 -> pure Nothing
    1 -> Just <$> g
    _ -> fail ("bincode Option: invalid tag " <> show tag)

instance Binary LockupArgs where
  put :: LockupArgs -> Put
  put (LockupArgs ts epoch cust) = do
    putBincodeOption putInt64le ts
    putBincodeOption putWord64le epoch
    putBincodeOption (putByteString . getSolanaPublicKeyRaw) cust
  get :: Get LockupArgs
  get =
    LockupArgs
      <$> getBincodeOption getInt64le
      <*> getBincodeOption getWord64le
      <*> getBincodeOption get

-- | Stake program instructions (covered subset).
data StakeInstruction
  = Initialize Authorized Lockup
  | Authorize SolanaPublicKey StakeAuthorize
  | DelegateStake
  | Split Word64
  | Withdraw Word64
  | Deactivate
  | SetLockup LockupArgs
  | Merge
  | InitializeChecked
  | AuthorizeChecked StakeAuthorize
  | GetMinimumDelegation
  deriving (Eq, Show, Generic)

instance Binary StakeInstruction where
  put :: StakeInstruction -> Put
  put (Initialize authorized lockup) = do
    putWord32le 0
    put authorized
    put lockup
  put (Authorize newAuthority stakeAuthorize) = do
    putWord32le 1
    putByteString (getSolanaPublicKeyRaw newAuthority)
    putStakeAuthorize stakeAuthorize
  put DelegateStake = putWord32le 2
  put (Split amount) = do
    putWord32le 3
    putWord64le amount
  put (Withdraw amount) = do
    putWord32le 4
    putWord64le amount
  put Deactivate = putWord32le 5
  put (SetLockup args) = do
    putWord32le 6
    put args
  put Merge = putWord32le 7
  put InitializeChecked = putWord32le 9
  put (AuthorizeChecked stakeAuthorize) = do
    putWord32le 10
    putStakeAuthorize stakeAuthorize
  put GetMinimumDelegation = putWord32le 13

  get :: Get StakeInstruction
  get = do
    disc <- getWord32le
    case disc of
      0 -> Initialize <$> get <*> get
      1 -> Authorize <$> get <*> getStakeAuthorize
      2 -> pure DelegateStake
      3 -> Split <$> getWord64le
      4 -> Withdraw <$> getWord64le
      5 -> pure Deactivate
      6 -> SetLockup <$> get
      7 -> pure Merge
      9 -> pure InitializeChecked
      10 -> AuthorizeChecked <$> getStakeAuthorize
      13 -> pure GetMinimumDelegation
      _ -> fail ("StakeInstruction: unknown discriminant " <> show disc)
```

- [ ] **Step 4: Run and verify all pass** (13 goldens + property + unknown-disc). If a golden mismatches on a DISCRIMINANT (first 4 bytes), the fixture wins — correct the Haskell discriminant and note it.

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/NativePrograms/Stake.hs test/Test/NativePrograms/Stake.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "feat: Stake instruction type with bincode serialization"
```

---

### Task 3: Stake builders

**Files:**
- Modify: `src/Network/Solana/NativePrograms/Stake.hs` (append), `test/Test/NativePrograms/Stake.hs` (append)

**Interfaces:**
- Produces (Task 5 uses `initialize` and `delegateStake`): `initialize :: SolanaPublicKey -> Authorized -> Lockup -> Instruction`; `authorize :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> StakeAuthorize -> Maybe SolanaPublicKey -> Instruction` (stake, current authority, new authority, which, optional custodian); `delegateStake :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction` (stake, authorized staker, vote account); `split :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Instruction`; `withdraw :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Maybe SolanaPublicKey -> Instruction`; `deactivate :: SolanaPublicKey -> SolanaPublicKey -> Instruction`; `setLockup :: SolanaPublicKey -> LockupArgs -> SolanaPublicKey -> Instruction`; `merge :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction`; `initializeChecked :: SolanaPublicKey -> Authorized -> Instruction`; `authorizeChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> StakeAuthorize -> Maybe SolanaPublicKey -> Instruction`.

- [ ] **Step 1: Append the failing meta tests** (constants: stakePk = new-account keypair pubkey is NOT needed — use `stakeAcctPk = unsafeSolanaPublicKeyRaw (replicate 32 21)` as a plain test pubkey; add votePk = 32×17, recipientPk' = 32×22):

```haskell
        testCase "initialize metas" $
          iAccounts (Stake.initialize stakeAcctPk fixedAuthorized fixedLockup)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.rent False False
                ],
        testCase "authorize metas (no custodian)" $
          iAccounts (Stake.authorize stakeAcctPk payerPk newAuthPk Stake.AuthorizeStaker Nothing)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False
                ],
        testCase "authorize metas (custodian)" $
          iAccounts (Stake.authorize stakeAcctPk payerPk newAuthPk Stake.AuthorizeWithdrawer (Just custodianPk))
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False,
                  AccountMeta custodianPk True False
                ],
        testCase "delegateStake metas" $
          iAccounts (Stake.delegateStake stakeAcctPk payerPk votePk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta votePk False False,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta Sysvar.stakeHistory False False,
                  AccountMeta Stake.stakeConfigId False False,
                  AccountMeta payerPk True False
                ],
        testCase "split metas" $
          iAccounts (Stake.split stakeAcctPk recipientPk' payerPk 250000)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta payerPk True False
                ],
        testCase "withdraw metas (no custodian)" $
          iAccounts (Stake.withdraw stakeAcctPk recipientPk' payerPk 500000 Nothing)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta Sysvar.stakeHistory False False,
                  AccountMeta payerPk True False
                ],
        testCase "deactivate metas" $
          iAccounts (Stake.deactivate stakeAcctPk payerPk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False
                ],
        testCase "setLockup metas" $
          iAccounts (Stake.setLockup stakeAcctPk (Stake.LockupArgs Nothing Nothing Nothing) custodianPk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta custodianPk True False
                ],
        testCase "merge metas" $
          iAccounts (Stake.merge stakeAcctPk recipientPk' payerPk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta Sysvar.stakeHistory False False,
                  AccountMeta payerPk True False
                ],
        testCase "initializeChecked metas" $
          iAccounts (Stake.initializeChecked stakeAcctPk fixedAuthorized)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.rent False False,
                  AccountMeta payerPk False False,
                  AccountMeta payerPk True False
                ],
        testCase "authorizeChecked metas" $
          iAccounts (Stake.authorizeChecked stakeAcctPk payerPk newAuthPk Stake.AuthorizeStaker Nothing)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False,
                  AccountMeta newAuthPk True False
                ]
```

Add imports `Network.Solana.Core.Instruction`, `Network.Solana.Sysvar qualified as Sysvar` and the new constants (`stakeAcctPk` 32×21, `votePk` 32×17, `recipientPk'` 32×22) to the test module. NOTE for merge metas: destination first, then source — the test above uses (stakeAcctPk = destination, recipientPk' = source).

- [ ] **Step 2: Verify compile failure**, **Step 3: Implement the builders** (metas exactly as the tests assert; Rust source semantics: `initialize`=[w stake, ro rent]; `authorize`=[w stake, ro clock, s authority] + optional [s custodian]; `delegate_stake`=[w stake, ro vote, ro clock, ro stake_history, ro stake_config, s staker]; `split`=[w stake, w split-dest, s staker]; `withdraw`=[w stake, w recipient, ro clock, ro stake_history, s withdrawer] + optional [s custodian]; `deactivate`=[w stake, ro clock, s staker]; `set_lockup`=[w stake, s custodian-or-withdrawer]; `merge`=[w destination, w source, ro clock, ro stake_history, s staker]; `initialize_checked`=[w stake, ro rent, ro staker(non-signer), s withdrawer]; `authorize_checked`=[w stake, ro clock, s current-authority, s new-authority] + optional [s custodian]). Payloads: initialize→`Initialize authorized lockup`; initializeChecked→`InitializeChecked` (authorities travel as ACCOUNTS); authorize→`Authorize newAuthority which`; authorizeChecked→`AuthorizeChecked which`. Add `import Network.Solana.Sysvar qualified as Sysvar` to the source module. Follow the existing builder Haddock style.

- [ ] **Step 4: Green**, **Step 5: Commit** (`git commit -m "feat: stake program instruction builders"`, both files).

---

### Task 4: Vote module

**Files:**
- Create: `src/Network/Solana/NativePrograms/Vote.hs`, `test/Test/NativePrograms/Vote.hs`
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Produces: `voteProgramId`; `data VoteInit = VoteInit {viNodePubkey, viAuthorizedVoter, viAuthorizedWithdrawer :: SolanaPublicKey, viCommission :: Word8}`; `data VoteAuthorize = AuthorizeVoter | AuthorizeVoteWithdrawer` (u32 LE 0/1); `data VoteInstruction = InitializeAccount VoteInit | VoteAuthorizeIx SolanaPublicKey VoteAuthorize | VoteWithdraw Word64 | UpdateValidatorIdentity | UpdateCommission Word8 | VoteAuthorizeChecked VoteAuthorize` with discriminants 0, 1, 3, 4, 5, 7; builders `initializeVoteAccount :: SolanaPublicKey -> VoteInit -> Instruction` (metas [w vote, ro rent, ro clock, s node]); `authorizeVote :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> VoteAuthorize -> Instruction` (vote, current authority, new authority, which; metas [w vote, ro clock, s authority]); `withdrawVote :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Instruction` (vote, recipient, withdrawer; metas [w vote, w recipient, s withdrawer]); `updateValidatorIdentity :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction` (vote, new node, withdrawer; metas [w vote, s node, s withdrawer]); `updateCommission :: SolanaPublicKey -> SolanaPublicKey -> Word8 -> Instruction` (metas [w vote, s withdrawer]); `authorizeVoteChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> VoteAuthorize -> Instruction` (metas [w vote, ro clock, s current, s new]).

- [ ] **Step 1: Tests** — same shape as Stake's: 7 goldens (`InitializeAccount` with VoteInit{node 32×19, voter payer, withdrawer payer, commission 5}, `Authorize-voter`/`Authorize-withdrawer` with newAuthPk 32×20, `Withdraw` 500000, `UpdateValidatorIdentity`, `UpdateCommission` 5, `AuthorizeChecked` withdrawer), a round-trip property over all 6 constructors, an unknown-discriminant case (`BL.pack [2,0,0,0]` — the VOTE consensus instruction we deliberately do not model — expect Left), and meta tests for all 6 builders per the meta lists above (constants: votePk 32×17, nodePk 32×19, newAuthPk 32×20, recipientPk' 32×22, payerPk from seed 1). Serialization: InitializeAccount = disc 0 + node + voter + withdrawer + commission u8; Authorize = disc 1 + pubkey + u32; Withdraw = disc 3 + u64; UpdateValidatorIdentity = disc 4; UpdateCommission = disc 5 + u8; AuthorizeChecked = disc 7 + u32.
- [ ] **Step 2: Register, compile failure**; **Step 3: Implement** (mirror the Stake module's structure and style; symmetric Binary; `fail` on unknown discriminant); **Step 4: Green**; **Step 5: Commit** (`git commit -m "feat: vote program client (account-management surface)"`, four files).

---

### Task 5: Stake-setup transaction golden + docs

**Files:**
- Modify: `test/Test/Core/Message.hs`, `README.md`, `CHANGELOG.md`

**Interfaces:**
- Consumes: `Stake.initialize`, `Stake.delegateStake`, `SP.createAccount`, `newAccountKeys` (already defined in the Message test module from phase 2), `payerKeys`, `fixedBlockhash`.

- [ ] **Step 1: The golden test** (in Test/Core/Message.hs; add `import Network.Solana.NativePrograms.Stake qualified as Stake` and constants votePk' = 32×17, custodianPk' = 32×18):

```haskell
      testCase "stake setup transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let stakePk = fst newAccountKeys
            authorized = Stake.Authorized {Stake.aStaker = fst payerKeys, Stake.aWithdrawer = fst payerKeys}
            lockup = Stake.Lockup {Stake.lUnixTimestamp = 1700000000, Stake.lEpoch = 300, Stake.lCustodian = custodianPk'}
            ixs =
              [ SP.createAccount (fst payerKeys) stakePk 1000000 200 Stake.stakeProgramId,
                Stake.initialize stakePk authorized lockup,
                Stake.delegateStake stakePk (fst payerKeys) votePk'
              ]
        case newTransactionIntent [snd payerKeys, snd newAccountKeys] ixs fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "stake-setup-transaction" fs),
```

- [ ] **Step 2: RED** (fixture exists, test should pass only if everything is correct — observe; if it fails, debug against a hex diff before touching core code; core changes are NOT expected this phase and require a BLOCKED report).
- [ ] **Step 3: README** — check `- [x] Stake` and `- [x] Vote`. **CHANGELOG** top of Unreleased:

```markdown
* Added Stake program client (initialize, authorize, delegate, split, withdraw,
  deactivate, lockup, merge, checked variants) — golden-tested against the Rust SDK.
* Added Vote program client (account management: initialize, authorize, withdraw,
  identity/commission updates); consensus voting instructions are out of scope.
```

- [ ] **Step 4: Full suite green; commit** (`git commit -m "feat: stake-setup transaction golden; roadmap and changelog for stake & vote"`, three files).

---

## Success criteria (from the spec)

1. Suite green — every task. 2. Stake/Vote variant goldens — Tasks 1, 2, 4. 3. stake-setup transaction golden — Tasks 1, 5. 4. Docs — Task 5.
