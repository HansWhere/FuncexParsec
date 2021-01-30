{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures, GADTs, TypeApplications, FlexibleContexts, MultiParamTypeClasses, ExistentialQuantification #-}
module Context where

import Text.Parsec as Parsec
import Control.Monad.Identity (Identity)
import Data.Map.Strict as Map

infixl 6 :+
infixl 7 :*
infixr 8 :^
infixr 9 :$, :$.

data Expr = Expr :+ Expr | Expr :* Expr | Expr :^ Expr
    | Func :$ Expr | Lambda :$. Expr
    | Negative Expr | Reciprocal Expr
    | Var String
    | Val Int
    | IllegalExpr String
    deriving Eq

instance Show Expr where
    show expr = case expr of
        Var str -> str
        Val value -> show value
        a :+ Negative b -> show a ++ "-" ++ showU b
        -- a :+ (Negative b :+ c) -> show a ++ "-" ++ showU b ++ show c -- a + (-b + ...)
        a :* Reciprocal b -> showM a ++ "/" ++ showM b
        a :+ b -> show a ++ "+" ++ show b
        a :* b -> showM a ++ "*" ++ showM b
        a :^ b -> showP a ++ "^" ++ showP b
        (f := lmd) :$ a -> f ++ (surround . show) a
        (v :-> ex) :$. a -> "λ" ++ v ++ "." ++ show ex
        Negative a -> "-" ++ showU a
        Reciprocal a -> "1/" ++ showM a
        where
            surround str = "(" ++ str ++ ")"
            showM factor = case factor of
                a :+ b -> surround $ show factor
                Negative a -> surround $ show factor
                Reciprocal a -> surround $ show factor
                _ -> show factor
            showP part = case part of
                a :* b -> surround $ show part
                _ -> showM part
            showU arg = case arg of
                a :^ b -> surround $ show arg
                _ -> showP arg

infix 3 :->
infix 2 :=

data Lambda = String :-> Expr | Undefd
    deriving (Eq, Show)

data Func = String := Lambda 
    deriving Eq

instance Show Func where
    show (f := x :-> ex) = f ++ "(" ++ x ++ ")=" ++ show ex
    show (f := Undefd) = "! Undefined \"" ++ f ++ "\"!"

infix 8 @?
(@?) :: [Func] -> String -> Func
(func@(f := lmd) : fs) @? str = if f == str then func else fs @? str
[] @? str = str := Undefd

infixl 1 @>
(@>) :: [Func] -> Func -> [Func]
fs @> f = updateFs [] fs f
    where 
        updateFs fs0 [] f' = f' : fs0
        updateFs fs0 (func1@(f1 := lmd1) : fs1) func'@(f' := lmd') = 
            if f1 == f' then func' : reverse fs0 ++ fs1 
            else updateFs (func1:fs0) fs1 func'