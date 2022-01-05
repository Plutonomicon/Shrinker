module Shrink.Testing.Tactics (
  shrinkingTactics,
  testTacticOn,
  run,
  prettyPrintTerm,
  Similar ((~=)),
  (~/=),
) where

import Shrink (defaultShrinkParams, size)
import Shrink.Names (dTermToN)
import Shrink.Testing.Gen (genUplc)
import Shrink.Types (NTerm, SafeTactic, Tactic, safeTactics, tactics)

import Data.Function (on)

import Hedgehog (MonadTest, annotate, assert, failure, forAll, property, success)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (ExBudget, exBudgetCPU, exBudgetMemory),
  ExRestrictingBudget (ExRestrictingBudget, unExRestrictingBudget),
 )
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (), ExMemory ())
import PlutusCore.Name (Name)
import UntypedPlutusCore.Evaluation.Machine.Cek (CekEvaluationException, RestrictingSt (RestrictingSt), restricting, runCekNoEmit)

import PlutusCore qualified as PLC
import UntypedPlutusCore.Core.Type qualified as UPLC

type Result =
  Either
    (CekEvaluationException DefaultUni DefaultFun)
    (UPLC.Term Name DefaultUni DefaultFun ())

class Similar a where
  (~=) :: a -> a -> Bool

instance Similar (Result, RestrictingSt) where
  (lres, lcost) ~= (rres, rcost) = lres ~= rres && getCpu lcost ~= getCpu rcost && getMem lcost ~= getMem rcost
    where
      getCpu (RestrictingSt budget) = exBudgetCPU . unExRestrictingBudget $ budget
      getMem (RestrictingSt budget) = exBudgetMemory . unExRestrictingBudget $ budget

-- For now I test that cpu and memory changes are not too signifigant

instance Similar ExCPU where
  a ~= b = 5 * abs (a - b) < abs a + abs b

instance Similar ExMemory where
  a ~= b = 5 * abs (a - b) < abs a + abs b

instance Similar Result where
  (~=) = curry $ \case
    (Left _, Left _) -> True
    (Right lValue, Right rValue) -> lValue ~= rValue
    _ -> False

instance Similar (UPLC.Term Name DefaultUni DefaultFun ()) where
  (~=) = curry $ \case
    (UPLC.Var () _, UPLC.Var () _) -> True
    -- I don't love this clause but there are
    -- equivelent terms where this seems to be the best
    -- you can reasonably do
    -- !((\x -> #()) ()) == ()
    -- !(!(#(#()))) == ()
    -- !() == Error
    -- nearly anything in a force can be nearly anthing
    (UPLC.Force {}, _) -> True
    (_, UPLC.Force {}) -> True
    (UPLC.Delay () a, UPLC.Delay () b) -> a ~= b
    (UPLC.Apply () _ _, _) -> True
    (_, UPLC.Apply () _ _) -> True
    (UPLC.LamAbs () _ _, UPLC.LamAbs () _ _) -> True
    (UPLC.Builtin () a, UPLC.Builtin () b) -> a == b
    (UPLC.Constant () a, UPLC.Constant () b) -> a == b
    (UPLC.Error (), UPLC.Error ()) -> True
    _ -> False

instance Similar (UPLC.Term PLC.DeBruijn DefaultUni DefaultFun ()) where
  (~=) = (~=) `on` dTermToN

instance Similar (UPLC.Program PLC.DeBruijn DefaultUni DefaultFun ()) where
  (UPLC.Program () vl tl) ~= (UPLC.Program () vr tr) = vl == vr && tl ~= tr

instance Similar Script where
  (Script pl) ~= (Script pr) = pl ~= pr

(~/=) :: Similar a => a -> a -> Bool
a ~/= b = not $ a ~= b

shrinkingTactics :: TestTree
shrinkingTactics =
  testGroup
    "shrinking tactics"
    ( [ testGroup tactName [testSafeTactic tactName tact, testSafeTacticShrinks tact]
      | (tactName, tact) <- safeTactics defaultShrinkParams
      ]
        ++ [ testGroup tactName [testTactic tactName tact]
           | (tactName, tact) <- tactics defaultShrinkParams
           ]
    )

testSafeTactic :: String -> SafeTactic -> TestTree
testSafeTactic tactName safeTactic = testTactic tactName (return . safeTactic)

testSafeTacticShrinks :: SafeTactic -> TestTree
testSafeTacticShrinks st = testProperty "Safe tactic doesn't grow code" . property $ do
  uplc <- dTermToN <$> forAll genUplc
  assert $ size uplc >= size (st uplc)

testTactic :: String -> Tactic -> TestTree
testTactic tactName tactic = testProperty "Tactic doesn't break code" . property $ do
  uplc <- forAll genUplc
  testTacticOn tactName tactic (dTermToN uplc)

testTacticOn :: MonadTest m => String -> Tactic -> NTerm -> m ()
testTacticOn tactName tact uplc = do
  let res = run uplc
  let fails =
        [ uplc' | uplc' <- tact uplc, uplc' /= uplc, run uplc' ~/= res
        ]
  case fails of
    [] -> success
    (bad : _) -> do
      annotate $ prettyPrintTerm uplc
      annotate $ "produced: " ++ show res
      annotate $ "Shrank by " ++ tactName ++ " to"
      annotate $ prettyPrintTerm bad
      annotate $ "produced: " ++ show (run bad)
      failure

run :: NTerm -> (Result, RestrictingSt)
run =
  runCekNoEmit
    PLC.defaultCekParameters
    ( restricting . ExRestrictingBudget $
        ExBudget
          { exBudgetCPU = 1_000_000_000 :: ExCPU
          , exBudgetMemory = 1_000_000 :: ExMemory
          }
    )

prettyPrintTerm :: NTerm -> String
prettyPrintTerm =
  let showName n = "V-" ++ show (PLC.unUnique (PLC.nameUnique n))
   in \case
        UPLC.Var () name -> showName name
        UPLC.LamAbs () name term -> "(\\" ++ showName name ++ "->" ++ prettyPrintTerm term ++ ")"
        UPLC.Apply () f@UPLC.LamAbs {} x -> prettyPrintTerm f ++ " (" ++ prettyPrintTerm x ++ ")"
        UPLC.Apply () f x -> "(" ++ prettyPrintTerm f ++ ") (" ++ prettyPrintTerm x ++ ")"
        UPLC.Force () term -> "!(" ++ prettyPrintTerm term ++ ")"
        UPLC.Delay () term -> "#(" ++ prettyPrintTerm term ++ ")"
        UPLC.Constant () (PLC.Some (PLC.ValueOf ty con)) -> case (ty, con) of
          (PLC.DefaultUniInteger, i) -> show i
          (PLC.DefaultUniByteString, bs) -> show bs
          (PLC.DefaultUniString, txt) -> show txt
          (PLC.DefaultUniUnit, ()) -> "()"
          (PLC.DefaultUniBool, b) -> show b
          (PLC.DefaultUniData, dat) -> show dat
          _ -> "Exotic constant"
        UPLC.Builtin () f -> show f
        UPLC.Error () -> "Error"
