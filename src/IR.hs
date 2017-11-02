{-# LANGUAGE RecordWildCards #-}

module IR where

import Op

data Expr = Define {
      name         :: String
    , explicitArgs :: [String]
    , body         :: [IR]
    } deriving Eq

instance Show Expr where
    show Define{..} = name++show explicitArgs++" := \n    "++show body

data IR =
      AInst { op :: Op, result :: Var, var1 :: Var, var2 :: Var }
    | Int   { result :: Var, num :: Int }
    | Call  { label :: String, args :: [Var] }
    | If    { cond :: [IR], then' :: [IR], else' :: [IR] }
    | Label { label :: String }
    | Let   { exprs :: [Expr], irs :: [IR] }
    deriving (Show, Eq)

