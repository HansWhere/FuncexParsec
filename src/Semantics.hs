{-# LANGUAGE KindSignatures, GADTs, TypeApplications, FlexibleContexts, MultiParamTypeClasses #-}
module Semantics where

import Text.Parsec as Parsec
import Text.Parsec.Pos (SourcePos)
import Control.Monad.Identity (Identity)
import Context
import Syntax

class Reducible a where
    reduce :: a -> a

instance Reducible Expr where 
    reduce ex = case ex of 
        a :+ (b :+ c) -> reduce a :+ reduce b :+ reduce c
        a :+ b -> reduce a .+ reduce b
        a :* (b :* c) -> reduce a .* reduce b .* reduce c
        a :* Reciprocal b -> reduce a ./ reduce b
        a :* b -> reduce a .* reduce b
        a :^ b -> reduce a .^ reduce b
        Negative a -> negative $ reduce a
        Reciprocal a -> reciprocal $ reduce a
        f :$ a -> f .$ a
        r :$. a -> r .$. a
        ow -> ow

instance Reducible Func where reduce (g := x :-> ex) = g := x :-> reduce ex

infixl 6 .+, .-
infixl 7 .*, ./
infixr 8 .^
infixr 9 .$ 

(.+), (.-), (.*), (./), (.^) :: Expr -> Expr -> Expr 
negative, reciprocal :: Expr -> Expr

Val 0 .+ b = b
a .+ Val 0 = a
Val n .+ Val m = Val (n + m)
a .+ b = a :+ b

a .- b = a .+ negative b

Val 1 .* b = b
a .* Val 1 = a
Val 0 .* b = Val 0
a .* Val 0 = Val 0
Val n .* Val m = Val (n * m)
a .* b = a :* b

Val n ./ Val m = if snd (divMod n m) == 0 
    then Val $ div n m 
    else Val (div n gcdmn) .* reciprocal (Val (div m gcdmn))
    where gcdmn = gcd m n
a ./ b = a .* reciprocal b

Val 0 .^ Val 0 = IllegalExpr "! undefined 0^0 !"
a .^ Val 1 = a
Val 0 .^ b = Val 0
Val n .^ Val m = Val (n ^ m)
a .^ b = a :^ b

negative (Val 0) = Val 0
negative a = Negative a

reciprocal (Val 0) = IllegalExpr "! division by 0 !"
reciprocal a = Reciprocal a

(.$.) :: Lambda -> Expr -> Expr
(v :-> ex) .$. arg = applyFunc (Var v) arg ex
    where
        applyFunc :: Expr -> Expr -> Expr -> Expr
        applyFunc v arg f = let apf = applyFunc v arg in case f of
            a :+ b -> apf a :+ apf b
            a :* b -> apf a :* apf b
            a :^ b -> apf a :^ apf b
            Negative a -> Negative $ apf a
            Reciprocal a -> Reciprocal $ apf a
            Val n -> Val n
            var@(Var str) -> if var == v then arg else var

(.$) :: Func -> Expr -> Expr
(f := lmd) .$ arg = lmd .$. arg
