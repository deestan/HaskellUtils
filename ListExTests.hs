import Data.Char
import ListEx
import List
import Test.QuickCheck
import Test.QuickCheck.Batch

main = do
  runTests "endsWith" defOpt
    [ run prop_endsWith_self
    , run prop_endsWith_tails
    ]  
  
instance Arbitrary Char where
  arbitrary = elements ['A'..'z']
  coarbitrary x = if isAsciiUpper x then variant 0 else variant 1

prop_endsWith_self :: String -> Bool
prop_endsWith_self xs = xs `endsWith` xs

prop_endsWith_tails :: String -> Bool
prop_endsWith_tails xs = all id [xs `endsWith` t | t <- tails xs]
