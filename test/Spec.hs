{-# LANGUAGE InstanceSigs #-}
import Eval
import Parser
import SKI
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $
    do
      -- testing Parser
      -- testing Eval
      evalITest
      evalAppITest
      propPrimitivesEval
      propIEval

-- evaluation tests
evalIExp :: Expectation
evalIExp =
  eval I
    `shouldBe` Right I

evalITest :: SpecWith ()
evalITest =
  describe "eval" $
    context "when evaluating I" $
      it
        "should return I"
        evalIExp

evalIAppExp :: SKI -> Expectation
evalIAppExp x =
  eval (App [I, x])
    `shouldBe` Right x

evalAppITest :: SpecWith ()
evalAppITest =
  describe "eval"
    $ do
      context "when applying I to x"
    $ do it "should return x"
    $ do
      evalIAppExp
        K
      evalIAppExp I
      evalIAppExp S

-- property tests
newtype SKIPrimitives = SKIPrimitives {getSKIPrimitives :: SKI} deriving (Show, Eq)

instance Arbitrary SKIPrimitives where
  arbitrary :: Gen SKIPrimitives
  arbitrary = SKIPrimitives <$> elements [I, K, S]

prop_primitives_eval_to_themselves :: SKIPrimitives -> Bool
prop_primitives_eval_to_themselves x = eval (getSKIPrimitives x) == Right (getSKIPrimitives x)

propPrimitivesEval :: SpecWith ()
propPrimitivesEval = describe "eval" $ do
  it "should evaluate primitives to themselves" $ property prop_primitives_eval_to_themselves

prop_I_is_identity x = eval (App [I,getSKIPrimitives x]) == Right (getSKIPrimitives x)

propIEval :: SpecWith ()
propIEval = describe "eval" $ do
  it "I should behave like identity function" $ property prop_I_is_identity