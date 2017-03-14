import Test.Hspec
import Test.QuickCheck
import Lib


main :: IO ()
main = hspec $ 
  describe "functions" $ do
    it "moja" $ 
      moja `shouldBe` [()]
    it "ijayo" $ 
      ijayo (ijayo sufuri) `shouldBe` [(), ()]
    it "ijayo3" $ 
      ijayo (ijayo $ ijayo sufuri) `shouldBe` [(), (), ()]
    it "uliopita" $ 
      uliopita (ijayo sufuri) `shouldBe` []
    it "uliopita moja" $ 
      uliopita moja `shouldBe` []
    it "uliopita $ ijayo $ ijayo sufuri" $ 
      uliopita (ijayo $ ijayo sufuri) `shouldBe` [()]
    it "jumla sufuri sufuri" $ 
      jumla sufuri sufuri `shouldBe` sufuri
    it "jumla k sufuri" $
      jumla k sufuri `shouldBe` k
    it "jumla sufuri k" $
      jumla sufuri k `shouldBe` k
    it "jumla k k2" $
      jumla k k2 `shouldBe` [(), (), (), (), (), ()]
    it "jumla k k2  ==  jumla k2 k" $
      jumla k k2 `shouldBe` jumla k2 k
    where k = [(), (), (), ()]
          k2 = [(), ()]

