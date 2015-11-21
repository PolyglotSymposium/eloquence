module TestLambdaCalc where

import Test.Hspec
import LambdaCalc
import Control.Monad (forM_, when)

idFnS :: String -> String
idFnS name = concat ["(λ (", name, ") ", name, ")"]

idFn :: String -> DataType
idFn name = Fn [name] (Name name)

idFnExecutesCorrectly :: Bool
idFnExecutesCorrectly = executeText [] (idFnS "z") == Fn ["z"] (Name "z")

main :: IO ()
main = hspec $ do
  describe "execution of" $ do
    describe "simple variable lookup" $ do
      it "works for values in the environment" $ do
        executeText [("foo", Name "42")] "foo" `shouldBe` Name "42"

    describe "the identity function" $ do
      it "works correctly" $ do
        idFnExecutesCorrectly `shouldBe` True

    when idFnExecutesCorrectly $
      forM_ ["λ", "lambda"] $ \op ->
        describe op $ do
          it "evaluates to itself" $ do
            executeText [] ("(" ++ op ++ " (x) x)") `shouldBe` idFn "x"
          it "can be passed and return functions" $ do
            executeText [] ("((" ++ op ++ " (x) x) " ++ idFnS "y" ++ ")") `shouldBe` idFn "y"
          it "can run passed functions" $ do
            executeText [] ("((" ++ op ++ " (x) (x " ++ idFnS "z" ++ ")) " ++ idFnS "y" ++ ")") `shouldBe` idFn "z"

  describe "showing" $ do
    describe "a simple function" $ do
      it "uses λ" $ do
        showCode (idFn "x") `shouldBe` "(λ (x) x)"
      it "spaces out the args" $ do
        showCode (Fn ["a", "b"] (Name "c")) `shouldBe` "(λ (a b) c)"
      it "shows nested functions" $ do
        showCode (Fn ["a"] (idFn "b")) `shouldBe` "(λ (a) (λ (b) b))"
