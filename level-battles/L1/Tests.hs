import Test.Hspec
import Control.Monad ( mapM_ )
import LevelBattle
--
main :: IO ()
main = hspec $ do

  describe "LB 1.1" $ mapM_
    ( tester "lb11" lb11 )
    [
     (1996, 29),
     (1900, 28),
     (2018, 28),
     (2000, 29),
     (1995, 28),
     (1200, 29),
     (2001, 28)
    ]

  describe "LB 1.2" $ mapM_
    ( tester "lb12" lb12 )
    [("Ana voli milovana", True),
     ("Radar jej radar", True),
     ("abcba", True),
     ("AbCB A", True),
     ("Never a foot too far even", True),
     ("Dva radara", False),
     ("Hello there", False)
    ]

  describe "LB 1.3 String" $ mapM_
    ( tester2 "lb13" lb13 )
    [
     (3, "Haskell 2018", "sl28"),
     (1, "General Kenobi", "General Kenobi")
    ]

  describe "LB 1.3 Int" $ mapM_
    ( tester2 "lb13" lb13 )
    [
     (2, [2, 3, 5, 7, 11, 13, 17], [3, 7, 13]),
     (5, [1, 2, 3, 4, 5], [5]),
     (4, [1, 2, 3], [])
    ]

  where
    tester fname f (x, y) =
      it ( fname ++ " " ++ show x ++ " ==> " ++ show y ) $ f x `shouldBe` y
    tester2 fname f (x, y, z) =
      it ( fname ++ " " ++ show x ++ ", " ++ show y ++ " ==> " ++ show z ) $ f x y `shouldBe` z
