module Gen (
  genUplc,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Hedgehog (Gen)
import Hedgehog.Gen (ascii, bytes, choice, integral, list, text)
import Hedgehog.Range (linear)
import PlutusCore.Data (Data (B, Constr, I, List, Map))
import PlutusCore.DeBruijn (DeBruijn (..), Index (..))
import PlutusCore.Default (DefaultFun (..), DefaultUni, Some (Some), ValueOf (ValueOf))
import UntypedPlutusCore.Core.Type (Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var))

import PlutusCore.Default qualified as PLC

-- Passed to a generator, indicates the maximum recursion depth its children should have.
newtype RecursionDepth = RecursionDepth {unRecursionDepth :: Integer}
  deriving (Eq, Show, Ord, Enum, Num, Real, Integral)

type UplcTerm = Term DeBruijn DefaultUni DefaultFun ()

genRecursionDepth :: Gen RecursionDepth
genRecursionDepth = integral (linear 0 10)

genText :: Gen Text
genText = text (linear 0 100) ascii

genInteger :: Gen Integer
genInteger = integral (linear (-100_000_000_000) 100_000_000_000)

genByteString :: Gen ByteString
genByteString = bytes (linear 0 100)

genData :: RecursionDepth -> Gen Data
genData n =
  choice
    ( [ I <$> genInteger
      , B <$> genByteString
      ]
        ++ ( guard (n > 0)
              >> [ List <$> list (linear 0 10) (genData (n -1))
                 , Map
                    <$> list
                      (linear 0 10)
                      ((,) <$> genData (n -1) <*> genData (n -1))
                 , Constr <$> genInteger <*> list (linear 0 10) (genData (n -1))
                 ]
           )
    )

genUplc :: Gen UplcTerm
genUplc = do
  n <- genRecursionDepth
  genUplc' n 0

genUplc' :: RecursionDepth -> Integer -> Gen UplcTerm
genUplc' depth level =
  let next = genUplc' (depth -1) level
   in choice $
        [ Constant () <$> genConstant depth
        , Builtin () <$> genUplcBuiltin
        , return $ Error ()
        ]
          ++ ( guard (level >= 1)
                >> [Var () . DeBruijn . Index . fromIntegral <$> integral (linear 1 level)]
             )
          ++ ( guard (depth >= 1)
                >> [ LamAbs () (DeBruijn (Index 0)) <$> genUplc' (depth -1) (level + 1)
                   , Force () <$> next
                   , Delay () <$> next
                   , Apply () <$> next <*> next
                   ]
             )

genConstant :: RecursionDepth -> Gen (Some (ValueOf DefaultUni))
genConstant depth =
  choice
    [ Some . ValueOf PLC.DefaultUniInteger <$> genInteger
    , Some . ValueOf PLC.DefaultUniByteString <$> genByteString
    , Some . ValueOf PLC.DefaultUniString <$> genText
    , Some . ValueOf PLC.DefaultUniUnit <$> mempty
    , Some . ValueOf PLC.DefaultUniBool <$> choice (return <$> [True, False])
    , Some . ValueOf PLC.DefaultUniData <$> genData depth
    ]

genUplcBuiltin :: Gen DefaultFun
genUplcBuiltin =
  choice $
    return
      <$> [ AddInteger
          , SubtractInteger
          , MultiplyInteger
          , DivideInteger
          , QuotientInteger
          , RemainderInteger
          , ModInteger
          , EqualsInteger
          , LessThanInteger
          , LessThanEqualsInteger
          , AppendByteString
          , ConsByteString
          , SliceByteString
          , LengthOfByteString
          , IndexByteString
          , EqualsByteString
          , LessThanByteString
          , LessThanEqualsByteString
          , Sha2_256
          , Sha3_256
          , Blake2b_256
          , VerifySignature
          , AppendString
          , EqualsString
          , EncodeUtf8
          , DecodeUtf8
          , IfThenElse
          , ChooseUnit
          , Trace
          , FstPair
          , SndPair
          , ChooseList
          , MkCons
          , HeadList
          , TailList
          , NullList
          , ChooseData
          , ConstrData
          , MapData
          , ListData
          , IData
          , BData
          , UnConstrData
          , UnMapData
          , UnListData
          , UnIData
          , UnBData
          , EqualsData
          , MkPairData
          , MkNilData
          , MkNilPairData
          ]
