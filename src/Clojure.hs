{-# LANGUAGE RecordWildCards #-}

module Clojure where

import Op

data Clojure = Clojure {
      name         :: String
    , explicitArgs :: [String]
    , implicitArgs :: [String]
    , body         :: [IR]
    , retAddr      :: Addr
    } deriving Eq

instance Show Clojure where
    show (Clojure{..}) =
        name++show explicitArgs++show implicitArgs++" := \n    "
        ++show body

data Addr = Def
          | Addr String
    deriving (Show, Eq)

data IR =
      AInst { op :: Op, result :: Var, var1 :: Var, var2 :: Var }
    | Int   { result :: Var, num :: Int }
    | Call  { label :: String, args :: [Var], implicit :: [Var] }
    | Boe   { cond :: [IR], then' :: [IR], elseLabel :: String, continueLabel :: String}
    | Label { label :: String }
    deriving (Show, Eq)

