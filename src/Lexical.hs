{-# LANGUAGE KindSignatures, GADTs, TypeApplications, FlexibleContexts, MultiParamTypeClasses #-}
module Lexical where

import Data.Char (isSpace)
import Text.Parsec as Parsec
import Text.Parsec.Pos (SourcePos)
import Control.Monad.Identity (Identity)
import Context (Func)

data Token = Id String | Nm Int
    | Add | Sbs | Mul | Dvd | Pwr
    | Lparen | Rparen | DefEq 
    | Dot | Excla | Colon
    deriving (Eq, Show)

tokenC :: Stream s m Token => Token -> ParsecT s [Func] m Token
tokenC tok = tokenPrim showTok nextPos testTok
    where
        showTok t = show t
        nextPos pos t restStr = incSourceColumn pos 1
        testTok t = if tok == t then Just t else Nothing

tokenId :: Stream s m Token => ParsecT s [Func] m String
tokenId = tokenPrim showTok nextPos testTok
    where
        showTok t = show t
        nextPos pos t restStr = incSourceColumn pos 1
        testTok t = case t of
            Id str -> Just str
            _ -> Nothing

tokenNm :: Stream s m Token => ParsecT s [Func] m Int
tokenNm = tokenPrim showTok nextPos testTok
    where
        showTok t = show t
        nextPos pos t restStr = incSourceColumn pos 1
        testTok t = case t of
            Nm n -> Just n
            _ -> Nothing

tokenParsec :: Stream s m Char => ParsecT s [Func] m Token
tokenParsec = 
    spaces *> (do
        x <- letter
        xs <- many alphaNum
        return $ Id (x:xs)
    <|> (many1 digit >>= return . Nm . read)
    <|> (char '(' >> return Lparen)
    <|> (char ')' >> return Rparen)
    <|> (char '+' >> return Add)
    <|> (char '-' >> return Sbs)
    <|> (char '*' >> return Mul)
    <|> (char '/' >> return Dvd)
    <|> (char '^' >> return Pwr)
    <|> (char '=' >> return DefEq)
    <|> (char '.' >> return Dot)
    <|> (char '!' >> return Excla)
    <|> (char ':' >> return Colon)
    ) <* spaces

tokensParsec :: Stream s m Char => ParsecT s [Func] m [Token]
tokensParsec = many tokenParsec