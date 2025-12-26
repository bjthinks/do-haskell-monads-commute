module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Maybe
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
  line "do { x <- get ; y <- ask ; put (x+y) }"
  let doA = do { x <- get ; y <- ask ; put (x+y) } :: ReaderT Int (State Int) ()
      doB = do { x <- get ; y <- ask ; put (x+y) } :: StateT Int (Reader Int) ()
  line "Running with Reader data 2, State data 3"
  let resultA = runState (runReaderT doA 2) 3
  let resultB = runReader (runStateT doB 3) 2
  line $ "Monad = ReaderT Int (State Int), result: " ++ show resultA
  line $ "Monad = StateT Int (Reader Int), result: " ++ show resultB
  line "Conclusion: Reader and State commute."
  return ()

readerWriter :: Writer String ()
readerWriter = do
  line "Composing Reader and Writer monads."
  line "do { x <- ask ; tell (show x) }"
  let doA = do { x <- ask ; tell (show x) } :: ReaderT Int (Writer String) ()
      doB = do { x <- ask ; tell (show x) } :: WriterT String (Reader Int) ()
  line "Running with Reader data 2"
  let resultA = runWriter (runReaderT doA 2)
  let resultB = runReader (runWriterT doB) 2
  line $ "Monad = ReaderT Int (Writer String), result: " ++ show resultA
  line $ "Monad = WriterT String (Reader Int), result: " ++ show resultB
  line "Conclusion: Reader and Writer commute."
  return ()

stateWriter :: Writer String ()
stateWriter = do
  line "Composing State and Writer monads."
  line "do { x <- get ; tell (show x) ; put (x+1) }"
  let doA = do { x <- get ; tell (show x) ; put (x+1) } :: StateT Int (Writer String) ()
      doB = do { x <- get ; tell (show x) ; put (x+1) } :: WriterT String (State Int) ()
  line "Running with State data 3"
  let resultA = runWriter (runStateT doA 3)
  let resultB = runState (runWriterT doB) 3
  line $ "Monad = StateT Int (Writer String), result: " ++ show resultA
  line $ "Monad = WriterT String (State Int), result: " ++ show resultB
  line "Conclusion: State and Writer essentially commute."
  return ()

maybeReader :: Writer String ()
maybeReader = do
  line "Composing Maybe and Reader monads."
  line "do { _ <- ask ; return () }"
  let doA = do { _ <- ask ; return () } :: MaybeT (Reader Int) ()
      doB = do { _ <- ask ; return () } :: ReaderT Int (Maybe) ()
  line "Running with Reader data 2"
  let resultA = runReader (runMaybeT doA) 2
  -- if there were a runMaybe function, it would be defined as:
  -- runMaybe = id :: Maybe a -> Maybe a
  let resultB = runReaderT doB 2
  line $ "Monad = MaybeT (Reader Int), result: " ++ show resultA
  line $ "Monad = ReaderT Int (Maybe), result: " ++ show resultB

  line "do { _ <- ask ; mzero }"
  let doC = do { _ <- ask ; mzero } :: MaybeT (Reader Int) ()
      doD = do { _ <- ask ; mzero } :: ReaderT Int (Maybe) ()
  line "Running with Reader data 2"
  let resultC = runReader (runMaybeT doC) 2
  -- runMaybe = id
  let resultD = runReaderT doD 2
  line $ "Monad = MaybeT (Reader Int), result: " ++ show resultC
  line $ "Monad = ReaderT Int (Maybe), result: " ++ show resultD
  line "Conclusion: Maybe and Reader commute."
  return ()

maybeState :: Writer String ()
maybeState = do
  line "Composing Maybe and State monads."
  line "do { x <- get ; put (x+1) }"
  let doA = do { x <- get ; put (x+1) } :: MaybeT (State Int) ()
      doB = do { x <- get ; put (x+1) } :: StateT Int (Maybe) ()
  line "Running with State data 3"
  let resultA = runState (runMaybeT doA) 3
  -- runMaybe = id
  let resultB = runStateT doB 3
  line $ "Monad = MaybeT (State Int), result: " ++ show resultA
  line $ "Monad = StateT Int (Maybe), result: " ++ show resultB

  line "do { x <- get ; put (x+1) ; mzero }"
  let doC = do { x <- get ; put (x+1) ; mzero } :: MaybeT (State Int) ()
      doD = do { x <- get ; put (x+1) ; mzero } :: StateT Int (Maybe) ()
  line "Running with State data 3"
  let resultC = runState (runMaybeT doC) 3
  -- runMaybe = id
  let resultD = runStateT doD 3
  line $ "Monad = MaybeT (State Int), result: " ++ show resultC
  line $ "Monad = StateT Int (Maybe), result: " ++ show resultD
  line "Conclusion: Maybe and State do not commute, because the resulting state"
  line "of a failing computation is handled differently."
  return ()

maybeWriter :: Writer String ()
maybeWriter = do
  line "Composing Maybe and Writer monads."
  line "do { tell \"xyzzy\" }"
  let doA = do { tell "xyzzy" } :: MaybeT (Writer String) ()
      doB = do { tell "xyzzy" } :: WriterT String (Maybe) ()
  let resultA = runWriter (runMaybeT doA)
  -- runMaybe = id
  let resultB = runWriterT doB
  line $ "Monad = MaybeT (Writer String), result: " ++ show resultA
  line $ "Monad = WriterT String (Maybe), result: " ++ show resultB

  line "do { tell \"xyzzy\" ; mzero }"
  let doC = do { tell "xyzzy" ; mzero } :: MaybeT (Writer String) ()
      doD = do { tell "xyzzy" ; mzero } :: WriterT String (Maybe) ()
  let resultC = runWriter (runMaybeT doC)
  -- runMaybe = id
  let resultD = runWriterT doD
  line $ "Monad = MaybeT (Writer String), result: " ++ show resultC
  line $ "Monad = WriterT String (Maybe), result: " ++ show resultD
  line "Conclusion: Maybe and Writer do not commute, because the written value"
  line "of a failing computation is handled differently."
  return ()

exceptReader :: Writer String ()
exceptReader = do
  line "Composing Except and Reader monads."
  line "do { _ <- ask ; return () }"
  let doA = do { _ <- ask ; return () } :: ExceptT String (Reader Int) ()
      doB = do { _ <- ask ; return () } :: ReaderT Int (Except String) ()
  line "Running with Reader data 2"
  let resultA = runReader (runExceptT doA) 2
  let resultB = runExcept (runReaderT doB 2)
  line $ "Monad = ExceptT String (Reader Int), result: " ++ show resultA
  line $ "Monad = ReaderT Int (Except String), result: " ++ show resultB

  line "do { _ <- ask ; throwError \"error\" }"
  let doC = do { _ <- ask ; throwError "error" } :: ExceptT String (Reader Int) ()
      doD = do { _ <- ask ; throwError "error" } :: ReaderT Int (Except String) ()
  line "Running with Reader data 2"
  let resultC = runReader (runExceptT doC) 2
  let resultD = runExcept (runReaderT doD 2)
  line $ "Monad = ExceptT String (Reader Int), result: " ++ show resultC
  line $ "Monad = ReaderT Int (Except String), result: " ++ show resultD
  line "Conclusion: Except and Reader commute."
  return ()

exceptState :: Writer String ()
exceptState = do
  line "Composing Except and State monads."
  line "do { x <- get ; put (x+1) }"
  let doA = do { x <- get ; put (x+1) } :: ExceptT String (State Int) ()
      doB = do { x <- get ; put (x+1) } :: StateT Int (Except String) ()
  line "Running with State data 3"
  let resultA = runState (runExceptT doA) 3
  let resultB = runExcept (runStateT doB 3)
  line $ "Monad = ExceptT String (State Int), result: " ++ show resultA
  line $ "Monad = StateT Int (Except String), result: " ++ show resultB

  line "do { x <- get ; put (x+1) ; throwError \"error\" }"
  let doC = do { x <- get ; put (x+1) ; throwError "error" } :: ExceptT String (State Int) ()
      doD = do { x <- get ; put (x+1) ; throwError "error" } :: StateT Int (Except String) ()
  line "Running with State data 3"
  let resultC = runState (runExceptT doC) 3
  let resultD = runExcept (runStateT doD 3)
  line $ "Monad = ExceptT String (State Int), result: " ++ show resultC
  line $ "Monad = StateT Int (Except String), result: " ++ show resultD
  line "Conclusion: Except and State do not commute, because the resulting"
  line "state of a failing computation is handled differently."
  return ()

exceptWriter :: Writer String ()
exceptWriter = do
  line "Composing Except and Writer monads."
  line "do { tell \"xyzzy\" }"
  let doA = do { tell "xyzzy" } :: ExceptT String (Writer String) ()
      doB = do { tell "xyzzy" } :: WriterT String (Except String) ()
  let resultA = runWriter (runExceptT doA)
  let resultB = runExcept (runWriterT doB)
  line $ "Monad = ExceptT String (Writer String), result: " ++ show resultA
  line $ "Monad = WriterT String (Except String), result: " ++ show resultB

  line "do { tell \"xyzzy\" ; throwError \"error\" }"
  let doC = do { tell "xyzzy" ; throwError "error" } :: ExceptT String (Writer String) ()
      doD = do { tell "xyzzy" ; throwError "error" } :: WriterT String (Except String) ()
  let resultC = runWriter (runExceptT doC)
  let resultD = runExcept (runWriterT doD)
  line $ "Monad = ExceptT String (Writer String), result: " ++ show resultC
  line $ "Monad = WriterT String (Except String), result: " ++ show resultD
  line "Conclusion: Except and Writer do not commute, because the written value"
  line "of a failing computation is handled differently."
  return ()

allPairs :: Writer String ()
allPairs = do
  readerState
  nl
  readerWriter
  nl
  stateWriter
  nl
  maybeReader
  nl
  maybeState
  nl
  maybeWriter
  nl
  exceptReader
  nl
  exceptState
  nl
  exceptWriter

main :: IO ()
main = putStr $ execWriter allPairs
