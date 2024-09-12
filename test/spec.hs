{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import GHC.TypeLits.Printf
import Test.Hspec

-- TODO: test for type errors, stuff like that. For now we just test for
-- parsing long strings
main :: IO ()
main = hspec do
  describe "typelits-printf" do
    it "formats a basic string" do
      printf "Hello %0.2f, %s" (3.26 :: Double) "Luigi" `shouldBe` "Hello 3.26, Luigi"
      printf' @"Hello %0.2f, %s" (3.26 :: Double) "Luigi" `shouldBe` "Hello 3.26, Luigi"
    it "formats a long string" do
      printf "This is a long string a very %s in the past this took a long time to compile %d"
        "long string"
        (2 :: Int)
        `shouldBe` "This is a long string a very long string in the past this took a long time to compile 2"
