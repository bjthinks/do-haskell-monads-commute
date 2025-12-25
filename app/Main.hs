module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

nl :: Writer String ()
nl = tell "\n"

line :: String -> Writer String ()
line str = tell str >> nl

readerState :: Writer String ()
readerState = do
  line "Composing Reader and State monads."
  line "Program: do { x <- get ; y <- ask ; put (x+y) }"
  let doA = do { x <- get ; y <- ask ; put (x+y) } :: ReaderT Int (State Int) ()
      doB = do { x <- get ; y <- ask ; put (x+y) } :: StateT Int (Reader Int) ()
  line "Running with Reader data 2, State data 3"
  let resultA = runState (runReaderT doA 2) 3
  let resultB = runReader (runStateT doB 3) 2
  line $ "Monad = ReaderT Int (State Int), result: " ++ show resultA
  line $ "Monad = StateT Int (Reader Int), result: " ++ show resultB
  return ()

readerWriter :: Writer String ()
readerWriter = do
  line "Composing Reader and Writer monads."
  line "Program: do { x <- ask ; tell (show x) }"
  let doA = do { x <- ask ; tell (show x) } :: ReaderT Int (Writer String) ()
      doB = do { x <- ask ; tell (show x) } :: WriterT String (Reader Int) ()
  return ()

stateWriter :: Writer String ()
stateWriter = do
  line "Composing State and Writer monads."
  line "Program: do { x <- get ; tell (show x) ; put (x+1) }"
  let doA = do { x <- get ; tell (show x) ; put (x+1) } :: StateT Int (Writer String) ()
      doB = do { x <- get ; tell (show x) ; put (x+1) } :: WriterT String (State Int) ()
  return ()

allPairs :: Writer String ()
allPairs = do
  readerState
  nl
  readerWriter
  nl
  stateWriter

main :: IO ()
main = putStr $ execWriter allPairs
