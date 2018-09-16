module Lib
    ( someFunc
    ) where

import Lib.Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding
                ,passedCultureFit
                ,educationMin]


                               
