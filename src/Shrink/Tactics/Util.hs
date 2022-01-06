module Shrink.Tactics.Util (
  completeTactic,
  applyArgs,
  reconsile,
  findHoles,
  withTemplate,
  makeLambs,
  sepMaybe,
  uses,
  weakEquiv,
  mentions,
  equiv,
  subTerms,
  unsub,
  whnf,
  subName',
  completeRec,
  completeRecM,
  appBind,
  isData,
  succeds,
  completeSafeTactic,
) where

import Shrink.ScopeM (newName, runScopedTact)
import Shrink.Types (SafeTactic,PartialSafe,MonadScope, NTerm, PartialTactic, Scope, ScopeM, ScopeMT, ScopedTactic, SimpleType (Arr, Bool, ByteString, Data, Delayed, Integer, List, String, UnclearType, Unit), Tactic, WhnfRes (Err, Success, Unclear), (-->))

import Control.Applicative (liftA2)
import Control.Arrow (first, second)
import Control.Monad (guard, join, liftM2)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader, ask, local, runReaderT)
import Control.Monad.State (get, put, runStateT)
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Map (Map)
import Data.Maybe (fromMaybe)

--import PlutusCore.Default (DefaultFun)
import UntypedPlutusCore (Name)
import UntypedPlutusCore.Core.Type (Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var))

import Data.Map qualified as M
import Data.Set qualified as S
import PlutusCore.Default qualified as Default

completeTactic :: PartialTactic -> Tactic
completeTactic = runScopedTact . completeTactic'

completeTactic' :: PartialTactic -> ScopedTactic
completeTactic' pt term = do
  let st = completeTactic' pt
  extras <- fromMaybe [] <$> pt term
  descend st term <&> (++ extras)

descend :: ScopedTactic -> ScopedTactic
descend tact = \case
  Var _ name -> return [Var () name]
  LamAbs _ name term -> fmap (LamAbs () name) <$> addNameToScope name (tact term)
  Apply _ funTerm varTerm -> do
    funTerms <- tact funTerm
    varTerms <- tact varTerm
    return $
      Apply () funTerm varTerm :
      [Apply () funTerm' varTerm | funTerm' <- drop 1 funTerms]
      ++ [Apply () funTerm varTerm' | varTerm' <- drop 1 varTerms]
  Force _ term -> fmap (Force ()) <$> tact term
  Delay _ term -> fmap (Delay ()) <$> tact term
  Constant _ val -> return [Constant () val]
  Builtin _ fun -> return [Builtin () fun]
  Error _ -> return [Error ()]

addNameToScope :: MonadReader (Scope, Scope) m => Name -> m a -> m a
addNameToScope name = local $ second (S.insert name)

completeRec :: (NTerm -> Maybe NTerm) -> NTerm -> NTerm
completeRec partial = runIdentity . completeRecM (Identity . partial)

completeRecM :: Monad m => (NTerm -> m (Maybe NTerm)) -> NTerm -> m NTerm
completeRecM partial originalTerm =
  let rec = completeRecM partial
   in partial originalTerm >>= \case
        Just term -> return term
        Nothing ->
          case originalTerm of
            LamAbs _ name term -> LamAbs () name <$> rec term
            Apply _ f x -> Apply () <$> rec f <*> rec x
            Force _ term -> Force () <$> rec term
            Delay _ term -> Delay () <$> rec term
            term -> return term

appBind :: Name -> NTerm -> NTerm -> NTerm
appBind name val = completeRec $ \case
  Var _ varName ->
    if name == varName
      then Just val
      else Nothing
  _ -> Nothing

mentions :: Name -> NTerm -> Bool
mentions name = \case
  Var _ vname -> vname == name
  LamAbs _ lname term -> lname /= name && mentions name term
  Apply _ f x -> mentions name f || mentions name x
  Force _ term -> mentions name term
  Delay _ term -> mentions name term
  _ -> False

succeds :: MonadScope m => NTerm -> m Bool
succeds t = whnf t <&> (== Success ())

whnf :: MonadScope m => NTerm -> m (WhnfRes ())
whnf t = whnfT t <&> ($> ())

whnfT :: MonadScope m => NTerm -> m (WhnfRes SimpleType)
whnfT = whnf' 100

whnf' :: MonadScope m => Integer -> NTerm -> m (WhnfRes SimpleType)
whnf' 0 = const $ return Unclear
whnf' n =
  let rec = whnf' (n -1)
   in \case
        -- TODO: keep track of local binds in scopeM
        -- and variable types clear when possible
        Var {} -> return $ Success UnclearType
        -- TODO: add polymorphism to
        -- simple types so lambdas can be typed
        LamAbs {} -> return $ Success UnclearType
        Apply _ (LamAbs _ name lTerm) valTerm -> liftM2 (*>) (rec valTerm) (rec $ appBind name valTerm lTerm)
        Apply _ fTerm xTerm ->
          let f' = rec fTerm
              x' = rec xTerm
           in do
             f <- f'
             x <- x'
             return $ illegalJoin $
                liftA2
                  ( curry
                      ( \case
                          (Arr xt yt, xt')
                            | xt == xt' -> pure yt
                            | xt == UnclearType -> Unclear
                            | xt' == UnclearType -> Unclear
                            | otherwise -> Err
                          -- this case being when xt and xt' are both clear but still different
                          -- which will always result in a type error
                          (UnclearType, _) -> Unclear
                          (_, _) -> Err
                          -- f type is clear and not a function
                          -- must be a type error
                      )
                  )
                  f
                  x
        Force _ (Delay _ term) -> rec term
        Force _ t -> rec t <&> \case 
          Success (Delayed t') -> Success t' 
          Success UnclearType -> Unclear
          Success _ -> Err
          r -> r
        Delay _ t -> rec t <&> \case 
          Success t' -> Success (Delayed t')
          _ -> Success UnclearType 
          -- I think this is correct 
          -- with the way Force Delayed is handled 
          -- but there should probably be a DellayErr type to make this stronger
        Constant _ c -> return $ Success $
          case c of
            Default.Some (Default.ValueOf Default.DefaultUniInteger _) -> Integer
            Default.Some (Default.ValueOf Default.DefaultUniString _) -> String
            Default.Some (Default.ValueOf Default.DefaultUniByteString _) -> ByteString
            Default.Some (Default.ValueOf Default.DefaultUniUnit _) -> Unit
            Default.Some (Default.ValueOf Default.DefaultUniBool _) -> Bool
            Default.Some (Default.ValueOf Default.DefaultUniData _) -> Data
            _ -> UnclearType
        Builtin _ b ->
          return $ Success $
            let bin x = x --> x --> x
                binInt = bin Integer
                comp x = x --> x --> Bool
                compInts = comp Integer
                compBS = comp ByteString
             in case b of
                  Default.AddInteger -> binInt
                  Default.SubtractInteger -> binInt
                  Default.MultiplyInteger -> binInt
                  Default.EqualsInteger -> compInts
                  Default.LessThanInteger -> compInts
                  Default.LessThanEqualsInteger -> compInts
                  Default.AppendByteString -> bin ByteString
                  Default.ConsByteString -> Integer --> ByteString --> ByteString
                  Default.EqualsByteString -> compBS
                  Default.LessThanByteString -> compBS
                  Default.LessThanEqualsByteString -> compBS
                  Default.VerifySignature -> ByteString --> ByteString --> ByteString --> String
                  Default.AppendString -> bin String
                  Default.EqualsString -> comp String
                  Default.ConstrData -> Integer --> List Data --> Data
                  Default.EqualsData -> comp Data
                  _ -> UnclearType
        Error {} -> return Err

-- this would be illegal to implement as join
-- because it doesn't (and I think can't)
-- agree with the applicative instance
illegalJoin :: WhnfRes (WhnfRes a) -> WhnfRes a
illegalJoin Err = Err
illegalJoin Unclear = Unclear
illegalJoin (Success x) = x

--safe2Arg :: DefaultFun -> Bool
--safe2Arg = \case
--  Default.AddInteger -> True
--  Default.SubtractInteger -> True
--  Default.MultiplyInteger -> True
--  Default.EqualsInteger -> True
--  Default.LessThanInteger -> True
--  Default.LessThanEqualsInteger -> True
--  Default.AppendByteString -> True
--  Default.ConsByteString -> True
--  Default.IndexByteString -> True
--  Default.EqualsByteString -> True
--  Default.LessThanByteString -> True
--  Default.LessThanEqualsByteString -> True
--  Default.VerifySignature -> True
--  Default.AppendString -> True
--  Default.EqualsString -> True
--  Default.ChooseUnit -> True
--  Default.Trace -> True
--  Default.MkCons -> True
--  Default.ConstrData -> True
--  Default.EqualsData -> True
--  Default.MkPairData -> True
--  _ -> False

subTerms :: NTerm -> [(Scope, NTerm)]
subTerms t =
  (S.empty, t) : case t of
    LamAbs _ n term -> first (S.insert n) <$> subTerms term
    Apply _ funTerm varTerm -> subTerms funTerm ++ subTerms varTerm
    Force _ term -> subTerms term
    Delay _ term -> subTerms term
    Var {} -> []
    Constant {} -> []
    Builtin {} -> []
    Error {} -> []

unsub :: NTerm -> Name -> NTerm -> NTerm
unsub replacing replaceWith = completeRec $ \case
  term
    | term == replacing -> Just $ Var () replaceWith
  _ -> Nothing

equiv :: (Scope, NTerm) -> (Scope, NTerm) -> Bool
equiv (lscope, lterm) (rscope, rterm) =
  not (uses lscope lterm)
    && not (uses rscope rterm)
    && lterm == rterm

-- compares two (scoped) terms and maybe returns a template
-- the number of nodes of the template and the holes in the template
weakEquiv :: (Scope, NTerm) -> (Scope, NTerm) -> ScopeMT Maybe (NTerm, Integer, [Name])
weakEquiv (lscope, lterm) (rscope, rterm) = do
  -- ensure that unshared scope is not used
  guard $ not (uses lscope lterm)
  guard $ not (uses rscope rterm)
  weakEquiv' lterm rterm

weakEquiv' :: NTerm -> NTerm -> ScopeMT Maybe (NTerm, Integer, [Name])
weakEquiv' = curry $ \case
  (LamAbs _ ln lt, LamAbs _ rn rt)
    | ln == rn -> do
      (t, n, hs) <- weakEquiv' lt rt
      return (LamAbs () ln t, n, hs)
    | otherwise -> do
      rt' <- subName ln rn rt
      (t, n, hs) <- weakEquiv' lt rt'
      return (LamAbs () ln t, n, hs)
  (Apply _ lf lx, Apply _ rf rx) -> do
    (ft, fnodes, fholes) <- weakEquiv' lf rf
    (xt, xnodes, xholes) <- weakEquiv' lx rx
    return (Apply () ft xt, fnodes + xnodes, fholes ++ xholes)
  (Delay _ l, Delay _ r) -> do
    (t, n, h) <- weakEquiv' l r
    return (Delay () t, n + 1, h)
  (Force _ l, Force _ r) -> do
    (t, n, h) <- weakEquiv' l r
    return (Force () t, n + 1, h)
  (l, r)
    | l == r -> return (l, 1, [])
    | otherwise -> do
      guard =<< succeds l
      guard =<< succeds r
      holeName <- newName
      return (Var () holeName, 1, [holeName])

subName :: MonadScope m => Name -> Name -> NTerm -> m NTerm
subName replace replaceWith term = do
  new <- newName
  return $ subName' replace replaceWith $ subName' replaceWith new term

subName' :: Name -> Name -> NTerm -> NTerm
subName' replace replaceWith = completeRec $ \case
  LamAbs _ n t -> Just $ LamAbs () (if n == replace then replaceWith else n) (subName' replace replaceWith t)
  Var _ n -> Just $ Var () (if n == replace then replaceWith else n)
  _ -> Nothing

uses :: Scope -> NTerm -> Bool
uses s = \case
  Apply _ f x -> uses s f || uses s x
  Delay _ t -> uses s t
  Force _ t -> uses s t
  LamAbs _ n t -> n `S.notMember` s && uses s t
  Var _ n -> n `S.member` s
  _ -> False

sepMaybe :: ScopeMT Maybe a -> ScopeM (Maybe a)
sepMaybe smtma = do
  s <- ask
  f <- get
  case runStateT (runReaderT smtma s) f of
    Just (a, f') -> put f' >> return (Just a)
    Nothing -> return Nothing

makeLambs :: [Name] -> NTerm -> NTerm
makeLambs = flip $ foldr (LamAbs ())

withTemplate :: Name -> (NTerm, [Name]) -> NTerm -> ScopeM NTerm
withTemplate templateName (template, holes) = completeRecM $ \target -> do
  margs <- findHoles holes template target
  sepMaybe $ do
    mapArgs <- lift . lift $ margs
    let args = M.elems mapArgs
    guard . and =<< mapM succeds args 
    return $ applyArgs (Var () templateName) args

findHoles :: [Name] -> NTerm -> NTerm -> ScopeM (Maybe (Map Name NTerm))
findHoles holes template subTerm
  | template == subTerm = return $ Just M.empty
  | otherwise = case (template, subTerm) of
    (Var () nt, st)
      | nt `elem` holes -> return $ Just $ M.singleton nt st
    (Force _ t, Force _ s) -> findHoles holes t s
    (Delay _ t, Delay _ s) -> findHoles holes t s
    (LamAbs _ tn tt, LamAbs _ sn st)
      | tn == sn -> findHoles holes tt st
      | otherwise -> do
        st' <- subName tn sn st
        findHoles holes tt st'
    (Apply _ tf tx, Apply _ sf sx) ->
      join
        <$> liftM2
          (liftM2 reconsile)
          (findHoles holes tf sf)
          (findHoles holes tx sx)
    _ -> return Nothing

reconsile :: (Ord k, Eq a) => Map k a -> Map k a -> Maybe (Map k a)
reconsile m1 m2 = do
  guard $ and $ M.intersectionWith (==) m1 m2
  return $ M.union m1 m2

applyArgs :: NTerm -> [NTerm] -> NTerm
applyArgs = foldl (Apply ())

-- is data may have false negatives but not false positives
isData :: MonadScope m => NTerm -> m Bool
isData (Apply _ (Builtin _ Default.IData) _) = return True
isData (Apply _ (Builtin _ Default.BData) _) = return True
isData (Apply _ (Builtin _ Default.ConstrData) _) = return True
isData (Apply _ (Builtin _ Default.MapData) _) = return True
isData (Apply _ (Builtin _ Default.ListData) _) = return True
isData t = whnfT t <&> (== Success Data)

completeSafeTactic :: PartialSafe -> SafeTactic
completeSafeTactic = runScopedTact . completeRecM
