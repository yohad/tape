module Tape.Repl where

import Text.ParserCombinators.Parsec

import Control.Monad.State.Lazy
import Control.Lens

import Data.Functor ((<$>))

import System.Win32.DebugApi

import Tape.Types
import Tape.Debug

type Path = String
data Expression = Start Path
                | Continue
                | Exit
    deriving (Show, Eq)

parseStart :: Parser Expression
parseStart = do
    _ <- string "start"
    _ <- spaces
    path <- many1 anyChar
    return $ Start path

parseContinue :: Parser Expression
parseContinue = do
    _ <- string "continue"
    return Continue

parseExit :: Parser Expression
parseExit = do
    _ <- string "exit"
    return Exit

expressionParser :: Parser Expression
expressionParser = parseStart
               <|> parseContinue
               <|> parseExit

parseExpression :: String -> App Expression
parseExpression input = case parse expressionParser "" input of
    Left err -> error $ "parse error at  " ++ show err
    Right value -> return value

eventLoop :: App ()
eventLoop = do
    maybeDebugEvent <- liftIO $ waitForDebugEvent Nothing -- Defualt is INFINITE
    case maybeDebugEvent of
        Nothing -> liftIO $ putStrLn "DebugEvent: Nothing"
        Just debugEvent -> do
            put =<< over lastEvent (const debugEvent) <$> get
            processDebugEvent debugEvent
            continueEventLoop debugEvent
    where
        continueEventLoop (_, Exception _ Breakpoint) = return ()
        continueEventLoop (debugEventId, _) = do
            liftIO $ continueDebugEvent debugEventId True
            eventLoop
    
repl :: App ()
repl = do
    input <- liftIO getLine
    expression <- parseExpression input
    case expression of
        Start path -> do
            handle <- view debuggeeHandle <$> get
            case handle of
                Just _ -> liftIO $ putStrLn "Can't start a new process while another is being debugged"
                Nothing -> do
                    liftIO . putStrLn $ "Starting the process: " ++ path
                    processHandle <- liftIO $ createDebuggedProcess path
                    put =<< over debuggeeHandle (const $ Just processHandle) <$> get
                    eventLoop
        Continue -> do
            debugEventId <- fst . view lastEvent <$> get
            liftIO $ continueDebugEvent debugEventId True
            eventLoop
        _ -> return ()
    exitRepl expression
    
    where
        exitRepl Exit = liftIO $ putStrLn "Exit tape"
        exitRepl _ = repl