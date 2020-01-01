import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

testFindWord word = 
 let (Just result) = findWord gwc word
     string = map cell2char result
 in string `shouldBe` word

main :: IO ()
main = hspec $ do
 describe "How to write a test" $ do
  it "Should be able to run tests" $ do
    someString `shouldBe` "someString"
    someString `shouldBe` "another string"
 
 describe "formatGrid" $ do
  it "Should concatenate every line with a newline" $ do
   (formatGrid (gridWithCoords ["abc", "def","ghi"])) `shouldBe` "abc\ndef\nghi\n"

--   (formatGrid ["abc", "def","ghi"]) `shouldBe` "abc\ndef\nghi"

 describe "findWord" $ do
  it "Should find words that exist on the Grid" $ do
   testFindWord "HASKELL"
   testFindWord "PERL"
  it "Should not find words that do not exist on the Grid" $ do
   findWord gwc "TIAGO" `shouldBe` Nothing   
 
--   findWord grid "HAMSTER" `shouldBe` Just "HAMSTER"

 describe "findWords" $ do
  it "Should find all the words that exist on the Grid" $ do
   let found = findWords gwc languages
       asString = map (map cell2char) found
   asString `shouldBe` languages
  it "Should not find all the words that do not exist on the Grid" $ do
   findWords gwc ["PORTUGUESE","FRENCH","CZECH"] `shouldBe` []
