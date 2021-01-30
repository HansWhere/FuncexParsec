{-# LANGUAGE FlexibleContexts #-}
module Main where
import Text.Parsec (ParseError, ParsecT, Stream, runPT, getState)
import Text.Parsec.Error (Message(Message), errorMessages)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Control.Monad.Trans.State (StateT(..), get, put, modify, evalStateT)
import Lexical (tokensParsec)
import Context (Func)
import Command (cmdParsec)
-- import Syntax ()
-- import Semantics ()

interactCmds :: StateT [Func] IO () -- StateT [Func] IO (Either ParseError ())
interactCmds = do 
    lift $ putStr ":)>>  "
    cmd <- lift $ getLine
    fs <- get
    eifs <- runExceptT $ do 
        ts <- ExceptT $ runPT tokensParsec fs "(scanning)" cmd
        ExceptT . lift $ runPT (cmdParsec >> getState) fs "(parsing)" ts
    case eifs of
        Right fs' -> put fs' >> interactCmds
        Left e -> if elem (Message "Quit") (errorMessages e)
            then return ()
            else (lift . print) e >> interactCmds

main :: IO ()
main = runStateT interactCmds [] >> return ()