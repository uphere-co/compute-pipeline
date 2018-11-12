module Test.MyTestSpec where

import Test.Hspec
------
import MyTest

spec :: Spec
spec = do
  describe "MyTest" $
    it "should fail" $
      0 `shouldBe` 1
