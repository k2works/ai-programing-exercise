import Test.Hspec

import qualified FizzBuzzSpec

main :: IO ()
main = hspec $ do
  FizzBuzzSpec.spec
