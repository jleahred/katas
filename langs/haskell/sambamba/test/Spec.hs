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
    it "bidhaa k k2  ==  [(), (), (), (), (), (), (), ()]" $
      bidhaa k k2 `shouldBe` [(), (), (), (), (), (), (), ()]
    it "bidhaa k k2  ==  bidhaa k2 k" $
      bidhaa k k2 `shouldBe` bidhaa k2 k
    it "bidhaa k k2  ==  bidhaa2 k k2" $
      bidhaa k k2 `shouldBe` bidhaa2 k k2
    it "jumla k k2 == jumla2 k k2" $
      jumla k k2 `shouldBe` jumla2 k k2
    it "int2idadi 4 == k" $
      int2idadi 4 `shouldBe` k
    it "idadi2int k == 4" $
      idadi2int k `shouldBe` 4
    where k = [(), (), (), ()]
          k2 = [(), ()]

