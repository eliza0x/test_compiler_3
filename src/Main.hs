{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Writer as W
import Control.Monad as M
import Data.List as L
import Data.List ((\\))

import qualified IR as I
import qualified Clojure as C
import qualified Op as O

sample :: [I.Expr]
sample = [I.Define "main" ["a", "b"] [I.Let [
    I.Define "f" [] [I.AInst O.Add "_return" "a" "b"]
    ] []]]
        
clojure :: [String] -> I.Expr -> IO [C.Clojure]
clojure globalDef (I.Define name args irs) = do
    (result, clojures) <- W.runWriterT 
        $ M.mapM (clojureW globalDef) irs
    return $ C.Clojure name args [] (concat result) C.Def : clojures
    where
    clojureW :: [String] -> I.IR -> W.WriterT [C.Clojure] IO [C.IR]
    clojureW globalDef = \case
        I.AInst{..} -> return $ [C.AInst op result var1 var2]
        I.Int  {..} -> return $ [C.Int result num]
        I.Label{..} -> return $ [C.Label label]
        I.Call {..} -> return $ [C.Call label args]
        I.If   {..} -> do
            uuid <- return "hoge"
            continueLabel <- return "Continue"
            condC <- concat <$> M.mapM (clojureW globalDef) cond
            thenC <- concat <$> M.mapM (clojureW globalDef) then'
            elseC <- concat <$> M.mapM (clojureW globalDef) else'
            tell [C.Clojure uuid [] 
                            (concatMap usingVariables else' \\ globalDef) 
                            elseC 
                            (C.Addr continueLabel)]
            return $ [C.Boe condC thenC uuid continueLabel]
        I.Let  {..} -> do
            uuids  <- return $ L.replicate (length exprs) "hoge"
            exprsC <- M.mapM (\(I.Define{..}) -> do
                bodyC <- concat <$> M.mapM (clojureW globalDef) body
                return $ C.Clojure name explicitArgs 
                    (concatMap usingVariables body \\ globalDef) bodyC C.Def) exprs
            W.tell exprsC
            concat <$> M.mapM (clojureW globalDef) irs

    usingVariables :: I.IR -> [String]
    usingVariables = \case
        I.AInst{..} -> [result, var1 , var2]
        I.Int  {..} -> [result]
        I.Label{..} -> [label]
        I.Call {..} -> label:args
        I.If   {..} -> L.nub 
            (  concatMap usingVariables cond 
            ++ concatMap usingVariables then'
            ++ concatMap usingVariables else')
        I.Let  {..} -> L.nub
            (  concatMap usingVariablesExpr exprs
            ++ concatMap usingVariables irs )

    usingVariablesExpr :: I.Expr -> [String]
    usingVariablesExpr (I.Define name args irs) = 
        concatMap usingVariables irs

inspectGlobalDefined :: [I.Expr] -> [String]
inspectGlobalDefined = map inspect
    where
    inspect I.Define{..} = name

main :: IO ()
main = M.mapM_ print =<< 
    M.mapM (clojure $ "_return":inspectGlobalDefined sample) sample

