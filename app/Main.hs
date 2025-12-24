module Main where

import Control.Monad.Trans.Writer

readerState :: Writer String ()
readerState = do
  return ()

readerWriter :: Writer String ()
readerWriter = do
  return ()

stateWriter :: Writer String ()
stateWriter = do
  return ()

all :: Writer String ()
all = do
  readerState
  readerWriter
  stateWriter

main :: IO ()
main = mapM_ putStrLn $ execWriter all
