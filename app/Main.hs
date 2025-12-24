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

allPairs :: Writer String ()
allPairs = do
  readerState
  readerWriter
  stateWriter

main :: IO ()
main = putStr $ execWriter allPairs
