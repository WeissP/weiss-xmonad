import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "weiss-xmonad-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
