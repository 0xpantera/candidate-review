module Lib
    ( assessCandidateIO
    ) where

import Lib.Types
import qualified Data.Map as Map
viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding
                ,passedCultureFit
                ,educationMin]


readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "Enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "Enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureGrade
                    , education = degree })


assessCandidateIO :: IO ()
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  putStrLn statement


candidate1 :: Candidate
candidate1 = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  ,education = BA }

candidate2 :: Candidate
candidate2 = Candidate
  { candidateId = 2
  , codeReview = C
  , cultureFit = A
  ,education = PhD }

candidate3 :: Candidate
candidate3 = Candidate
  { candidateId = 3
  , codeReview = A
  , cultureFit = B
  ,education = MS }


candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1)
                            ,(2,candidate2)
                            ,(3,candidate3)]


assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement


  
