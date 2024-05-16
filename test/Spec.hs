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
      parseITest
      tokenizeITest
      syntaxErrTest
      parseErrTest
      -- testing Eval
      -- Unit Test
      evalITest
      evalAppITest
      evalAppTest
      -- property Tests
      propPrimitivesEval
      propIEval
      propKEval
      propKIEval
      propSKEval
      propSEval
      propAppPrimitiveEval

-- parsing tests
parseIExp :: Expectation
parseIExp =
  parse "I"
    `shouldBe` Right (App [I])

parseITest :: SpecWith ()
parseITest =
  describe "parse" $
    context "when parsing \"I\"" $
      it
        "should return Right I"
        parseIExp
syntaxErrExp :: Expectation
syntaxErrExp =
  parse "A"
    `shouldBe` Left SyntaxError

syntaxErrTest :: SpecWith ()
syntaxErrTest =
  describe "parse" $
    context "when parsing \"A\"" $
      it
        "should return a SyntaxError"
        syntaxErrExp

parseErrExp :: Expectation
parseErrExp =
  parse "K(KI"
    `shouldBe` Left ParserError

parseErrTest :: SpecWith ()
parseErrTest =
  describe "parse" $
    context "when parsing unclosed paretheses" $
      it
        "should return a ParseError"
        parseErrExp
tokenizeIExp :: Expectation
tokenizeIExp =
  tokenize "I"
    `shouldBe` Right [IToken]

tokenizeITest :: SpecWith ()
tokenizeITest =
  describe "tokenize" $
    context "when parsing \"I\"" $
      it
        "should return Right I"
        tokenizeIExp

-- evaluation tests
evalIExp :: Expectation
evalIExp =
  eval I
    `shouldBe` I

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
    `shouldBe` x

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

evalAppExp :: SKI -> SKI -> Expectation
evalAppExp x y =
  eval (App [K, App [x, y], App [y, x]])
    `shouldBe` eval (App [x, y])

evalAppTest :: SpecWith ()
evalAppTest =
  describe "eval"
    $ do
      context "when applying I to x"
    $ do it "should return x"
    $ do
      evalAppExp I K
      evalAppExp K I
      evalAppExp (App [K, I, I]) S

-- property tests
newtype SKIPrimitives = SKIPrimitives {getSKIPrimitives :: SKI} deriving (Show, Eq)

instance Arbitrary SKIPrimitives where
  arbitrary :: Gen SKIPrimitives
  arbitrary = SKIPrimitives <$> elements [I, K, S]

prop_primitives_eval_to_themselves :: SKIPrimitives -> Bool
prop_primitives_eval_to_themselves x = eval (getSKIPrimitives x) == getSKIPrimitives x

propPrimitivesEval :: SpecWith ()
propPrimitivesEval = describe "eval" $ do
  it "should evaluate primitives to themselves" $ property prop_primitives_eval_to_themselves

prop_I_is_identity :: SKIPrimitives -> Bool
prop_I_is_identity x = eval (App [I, getSKIPrimitives x]) == getSKIPrimitives x

propIEval :: SpecWith ()
propIEval = describe "eval" $ do
  it "I should behave like identity function" $ property prop_I_is_identity

prop_K_is_const :: SKIPrimitives -> SKIPrimitives -> Bool
prop_K_is_const x y = eval (App [K, getSKIPrimitives x, getSKIPrimitives y]) == getSKIPrimitives x

propKEval :: SpecWith ()
propKEval = describe "eval" $ do
  it "K should return the first argument" $ property prop_K_is_const

prop_KI :: SKIPrimitives -> SKIPrimitives -> Bool
prop_KI x y = eval (App [K, I, getSKIPrimitives x, getSKIPrimitives y]) == getSKIPrimitives y

propKIEval :: SpecWith ()
propKIEval = describe "eval" $ do
  it "KI should return the second argument" $ property prop_KI

prop_SK_is_KI :: SKIPrimitives -> SKIPrimitives -> Bool
prop_SK_is_KI x y = eval (App [S, K, getSKIPrimitives x, getSKIPrimitives y]) == eval (App [K, I, getSKIPrimitives x, getSKIPrimitives y])

propSKEval :: SpecWith ()
propSKEval = describe "eval" $ do
  it "SK should behave like KI" $ property prop_SK_is_KI

prop_S_apply :: SKIPrimitives -> SKIPrimitives -> SKIPrimitives -> Bool
prop_S_apply x y z = eval (App [S, getSKIPrimitives x, getSKIPrimitives y, getSKIPrimitives z]) == eval (App [App [getSKIPrimitives x, getSKIPrimitives z], App [getSKIPrimitives y, getSKIPrimitives z]])

propSEval :: SpecWith ()
propSEval = describe "eval" $ do
  it "S should apply the first argument to the third and the second to the third" $ property prop_S_apply

prop_app_primitive_returns_primitive :: SKIPrimitives -> Bool
prop_app_primitive_returns_primitive x = eval (App [getSKIPrimitives x]) == eval (getSKIPrimitives x)

propAppPrimitiveEval :: SpecWith ()
propAppPrimitiveEval = describe "eval" $ do
  it "evaluating primitives wrapped in App should evaluate to the primitive" $ property prop_app_primitive_returns_primitive
