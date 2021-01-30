{-# LANGUAGE KindSignatures, GADTs, TypeApplications, FlexibleContexts, MultiParamTypeClasses #-}
module Syntax where

import Text.Parsec as Parsec
import Text.Parsec.Pos (SourcePos)
import Control.Monad.Trans
import Lexical
import Context

exprParsec, termParsec, factorParsec, atomParsec :: Stream s m Token => ParsecT s [Func] m Expr

exprParsec = do
    v <- do
        fstTerm <- termParsec
        tailTerms <- Parsec.many (addTerm <|> sbsTerm)
        return $ foldl (.) id (reverse tailTerms) fstTerm
    notFollowedBy tokenNm 
    notFollowedBy tokenId
    notFollowedBy (tokenC Lparen)
    return v
    where 
        addTerm, sbsTerm :: Stream s m Token => ParsecT s [Func] m (Expr -> Expr)
        addTerm = do
            tokenC Add
            appendedTerm <- termParsec
            return (:+ appendedTerm)
        sbsTerm = do
            tokenC Sbs
            appendedTerm <- termParsec
            return (:+ Negative appendedTerm)

termParsec = do
    v <- do
        fstFactor <- factorParsec
        tailFactors <- Parsec.many (mulFactor <|> dvdFactor)
        return $ foldl (.) id (reverse tailFactors) fstFactor
    notFollowedBy tokenNm 
    notFollowedBy tokenId
    notFollowedBy (tokenC Lparen)
    return v
    where
        mulFactor, dvdFactor :: Stream s m Token => ParsecT s [Func] m (Expr -> Expr)
        mulFactor = do
            tokenC Mul
            appendedFactor <- factorParsec
            return (:* appendedFactor)
        dvdFactor = do
            tokenC Dvd
            appendedFactor <- factorParsec
            return (:* Reciprocal appendedFactor)

factorParsec = do 
    v <- do
        bases <- Parsec.many . try $ (do {a <- atomParsec; tokenC Pwr; return a})
        top <- atomParsec
        return $ foldr (:^) top bases
    notFollowedBy tokenNm 
    notFollowedBy tokenId
    notFollowedBy (tokenC Lparen)
    return v

atomParsec = do
    v <- surroundedExpr
        <|> (tokenId >>= \i -> funcParsec i <|> return (Var i))
        <|> (tokenNm >>= return . Val)
    notFollowedBy tokenNm 
    notFollowedBy tokenId
    notFollowedBy (tokenC Lparen)
    return v
    where
        surroundedExpr :: Stream s m Token => ParsecT s [Func] m Expr
        surroundedExpr = between (tokenC Lparen) (tokenC Rparen) exprParsec
        funcParsec name = do
            ex <- surroundedExpr
            fs <- getState
            return $ (fs @? name) :$ ex

statementParsecDefaultName :: Stream s m Token => String -> ParsecT s [Func] m Func
statementParsecDefaultName g = do
    tokenC Lparen
    x <- tokenId
    tokenC Rparen
    tokenC DefEq
    ex <- exprParsec
    let f = g := x :-> ex
    return f

statementParsec :: Stream s m Token => ParsecT s [Func] m Func
statementParsec = do
    g <- tokenId
    statementParsecDefaultName g

statementParsecOptionalBody :: Stream s m Token => ParsecT s [Func] m Func
statementParsecOptionalBody = do
    ti <- tokenId
    statementParsecDefaultName ti
        <|> (@? ti) <$> getState
    <|> (eof >> (!! 0) <$> getState)

