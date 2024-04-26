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
      -- Unit Test
      evalITest
      evalAppITest
      -- property Tests
      propPrimitivesEval
      propIEval
      propKEval
      propKIEval
      propSKEval

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

prop_I_is_identity :: SKIPrimitives -> Bool
prop_I_is_identity x = eval (App [I, getSKIPrimitives x]) == Right (getSKIPrimitives x)

propIEval :: SpecWith ()
propIEval = describe "eval" $ do
  it "I should behave like identity function" $ property prop_I_is_identity

prop_K_is_const :: SKIPrimitives -> SKIPrimitives -> Bool
prop_K_is_const x y = eval (App [K, getSKIPrimitives x, getSKIPrimitives y]) == Right (getSKIPrimitives x)

propKEval :: SpecWith ()
propKEval = describe "eval" $ do
  it "K should return the first argument" $ property prop_K_is_const

prop_KI ::  SKIPrimitives -> SKIPrimitives -> Bool
prop_KI x y  = eval (App [K, I, getSKIPrimitives x, getSKIPrimitives y]) == Right (getSKIPrimitives y)

propKIEval :: SpecWith ()
propKIEval = describe "eval" $ do
  it "KI should return the second argument" $ property prop_KI


prop_SK_is_KI ::  SKIPrimitives -> SKIPrimitives -> Bool
prop_SK_is_KI x y  = eval (App [S, K, getSKIPrimitives x, getSKIPrimitives y]) == eval (App [K, I, getSKIPrimitives x, getSKIPrimitives y])

propSKEval :: SpecWith ()
propSKEval = describe "eval" $ do
  it "SK should behave like KI" $ property prop_SK_is_KI