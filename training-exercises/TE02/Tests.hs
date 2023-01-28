import Test.Hspec
import Control.Monad ( mapM_ )
import TrainingExercises
import Data.List ( sort )
--
main :: IO ()
main = hspec $ do

  describe "findItem" $ mapM_
    ( tester2 "findItem" findItem )
    [
    ([("A", 4), ("B", 88)], "B", [("B",88)]),
    ([("A", 4), ("B", 88)], "C", [])
    ]

  describe "findItem" $ mapM_
    ( tester2 "findItem" findItem )
    [
    ([("word", "puh"), ("words", "haskell"), ("Words", "programming in haskell")], "words", [("words", "haskell")])
    ]

  describe "contains" $ mapM_
    ( tester2 "contains" contains )
    [
    ([("word", "puh"), ("words", "haskell"), ("Words", "programming in haskell")], "words", True),
    ([("word", "puh"), ("words", "haskell"), ("Words", "programming in haskell")], "word", True),
    ([("word", "puh"), ("words", "haskell"), ("Words", "programming in haskell")], "A", False),
    ([("word", "puh"), ("words", "haskell"), ("Words", "programming in haskell")], "", False)
    ]

  describe "lookup" $ mapM_
    ( tester2 "lookup" TrainingExercises.lookup )
    [
    ([("word", "puh"), ("words", "haskell"), ("Words", "programming in haskell")], "words", "haskell"),
    ([("word", "puh"), ("words", "haskell"), ("Words", "programming in haskell")], "word", "puh")
    ]

  describe "insert" $ mapM_
    ( tester2 "insert" $ insert . sort )
    [
    ([], ("key", "value"), [("key", "value")]),
    ([("k", "v")], ("a", "c"), [("a", "c"), ("k", "v")]),
    ([("k", "v")], ("k", "c"), [("k", "v")])
    ]

  describe "remove" $ mapM_
    ( tester2 "remove" remove )
    [
    ([("k", "v"), ("a", "c")], "k", [("a", "c")])
    ]

  describe "update" $ mapM_
    ( tester3 "update" $ update . sort )
    [
    ([("k", "v"), ("a", "c")], "keeey", "valuee", [("a", "c"), ("k", "v")]),
    ([("k", "v"), ("a", "c")], "k", "valuee", [("a", "c"), ("k", "valuee")])
    ]

  where
    tester0 fname f (x) =
      it ( fname ++ " ==> " ++ show x ) $ f `shouldBe` x
    tester fname f (x, y) =
      it ( fname ++ " " ++ show x ++ " ==> " ++ show y ) $ f x `shouldBe` y
    tester2 fname f (x, y, z) =
      it ( fname ++ " " ++ show x ++ ", " ++ show y ++ " ==> " ++ show z ) $ f x y `shouldBe` z
    tester3 fname f (w, x, y, z) =
      it ( fname ++ " " ++ show w ++ " " ++ show x ++ ", " ++ show y ++ " ==> " ++ show z ) $ f w x y `shouldBe` z
