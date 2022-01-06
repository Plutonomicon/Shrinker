module Shrink.Tactics.Tactics (
  tactList,
) where

import Shrink.ScopeM (liftScope, newName)
import Shrink.Tactics.Util (appBind, completeTactic, equiv, makeLambs, sepMaybe, subTerms, succeds, unsub, weakEquiv, whnf, withTemplate)
import Shrink.Types (Tactic, WhnfRes (Err, Success, Unclear))

import Control.Monad (guard,liftM2)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)

import PlutusCore.Default (DefaultFun (MkPairData))
import UntypedPlutusCore.Core.Type (Term (Apply, Builtin, Error, LamAbs, Var))

tactList :: [(String, Tactic)]
tactList =
  [ ("subs", subs)
  , ("weakUnsubs", weakUnsubs)
  , ("curry", uplcCurry)
  , ("strongUnsubs", strongUnsubs)
  ]

subs :: Tactic
subs = completeTactic $ \case
  Apply _ (LamAbs _ name funTerm) varTerm ->
    whnf varTerm <&> \case 
      Success () -> Just [appBind name varTerm funTerm]
      Unclear -> Nothing
      Err -> Just [Error ()]
  _ -> return Nothing 

weakUnsubs :: Tactic
weakUnsubs = completeTactic $ \case
  Apply () funTerm varTerm ->
    let fSubterms = subTerms funTerm
        vSubterms = subTerms varTerm
     in (Just <$>) . fmap catMaybes . sequence $ do
          fSubterm <- fSubterms
          vSubterm <- vSubterms
          guard $ fSubterm `equiv` vSubterm
          --TODO rewrite this to be more readable
          return $
            liftM2 (*>) 
            (guard <$> succeds (snd fSubterm))
            ( do
                name <- newName
                let funTerm' = unsub (snd fSubterm) name funTerm
                    varTerm' = unsub (snd vSubterm) name varTerm
                return . Just $
                  Apply
                    ()
                    ( LamAbs
                        ()
                        name
                        (Apply () funTerm' varTerm')
                    )
                    (snd fSubterm)
                                                   ) 
  _ -> return Nothing

strongUnsubs :: Tactic
strongUnsubs = completeTactic $ \case
  Apply () funTerm varTerm ->
    let fSubterms = subTerms funTerm
        vSubterms = subTerms varTerm
     in sepMaybe $
          sequence $ do
            fSubterm <- fSubterms
            vSubterm <- vSubterms
            return $ do
              (template, nodes, holes) <- weakEquiv fSubterm vSubterm
              guard $ nodes > 0 -- 0 node holes are just wholy disimilar terms
              name <- newName
              funTerm' <- liftScope $ withTemplate name (template, holes) funTerm
              varTerm' <- liftScope $ withTemplate name (template, holes) varTerm
              let templateArg = makeLambs holes template
              return $
                Apply
                  ()
                  ( LamAbs
                      ()
                      name
                      (Apply () funTerm' varTerm')
                  )
                  templateArg
  _ -> return Nothing

uplcCurry :: Tactic
uplcCurry = completeTactic $ \case
  Apply
    _
    (LamAbs _ name term)
    (Apply _ (Apply _ (Builtin _ MkPairData) pairFst) pairSnd) -> do
      n1 <- newName
      n2 <- newName
      let newTerm = appBind name (Apply () (Apply () (Builtin () MkPairData) (Var () n1)) (Var () n2)) term
      return $ Just [Apply () (LamAbs () n2 (Apply () (LamAbs () n1 newTerm) pairFst)) pairSnd]
  _ -> return Nothing
