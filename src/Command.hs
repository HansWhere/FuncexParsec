{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures, GADTs, TypeApplications, FlexibleContexts, MultiParamTypeClasses, ExistentialQuantification #-}
module Command (cmdParsec) where

import Text.Parsec as Parsec
import Control.Monad.Identity (Identity)
import Control.Monad.Trans
import Data.Map.Strict as Map
import Context
import Lexical
import Syntax
import Semantics 

cmdParsec, defCmd, simplCmd, showCmd, helpCmd, quitCmd :: Stream s IO Token => ParsecT s [Func] IO ()

cmdParsec = simplCmd <|> helpCmd <|> quitCmd <|> (showCmd <|> defCmd)

defCmd = do
    f <- statementParsec
    modifyState (@> f)
    lift . print $ f

simplCmd = do
    tokenC (Id "simpl")
    singleTar <|> exprTar <|> allTar
    where 
        singleTar = do
            tokenC Dot
            tar <- statementParsecOptionalBody
            let tar' = reduce tar
            modifyState (@> tar')
            lift . print $ tar'
        exprTar = do
            tokenC Colon
            tarEx <- exprParsec
            lift . print $ reduce tarEx
        allTar = do
            tokenC Excla
            modifyState (reduce <$>)
            lift . putStrLn $ "All simplified!"

showCmd = do
    option (Id "show") (tokenC (Id "show"))
    singleTar <|> exprTar <|> allTar
    where
        singleTar = do
            tokenC Dot
            tar <- statementParsecOptionalBody
            lift . print $ tar
        exprTar = do
            tokenC Colon
            tarEx <- exprParsec
            lift . print $ reduce tarEx
        allTar = do
            tokenC Excla
            fs <- getState
            lift . putStr . unlines $ show <$> fs
        
helpCmd = do
    tokenC (Id "help")
    tokenC Dot
    lift $ putStrLn "Help yourself :)"

quitCmd = do
    tokenC (Id "quit")
    tokenC Dot
    fail "Quit" 
