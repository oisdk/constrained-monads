module Main (main) where

import Test.QuickCheck
import Test.DocTest

oneIsOne :: Property
oneIsOne = 1 === (1 :: Integer)

main :: IO ()
main = do
  quickCheck (once oneIsOne)
  doctest
    [ "-isrc"
    , "src/" ]
