{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

main :: IO ()
main = do
  candidate <- readCandidate
  candidateTest <- assessCandidate $ pure candidate
  putStrLn candidateTest
