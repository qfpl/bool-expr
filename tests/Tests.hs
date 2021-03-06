{-# language TemplateHaskell #-}
module Main where

import Data.Foldable (for_)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty(..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Abstract
import qualified Concrete

genAbstract :: MonadGen m => m Abstract.Expr
genAbstract =
  Gen.recursive Gen.choice
    [ Abstract.Lit <$> Gen.bool <*> genWhitespace
    , Abstract.Ident <$> Gen.list (Range.constant 1 10) Gen.lower <*> genWhitespace
    ]
    [ Abstract.Not <$> genWhitespace <*> genAbstract
    , Abstract.Paren <$> genWhitespace <*> genAbstract <*> genWhitespace
    ]
  where
    genWhitespace =
      Gen.list (Range.constant 0 10) (pure Abstract.Space)

genAbstractSC :: MonadGen m => m Abstract.Expr
genAbstractSC =
  Gen.recursive Gen.choice
    [ pure Abstract.true_
    , pure Abstract.false_
    , Abstract.ident_ <$> Gen.list (Range.constant 1 10) Gen.lower
    ]
    [ Abstract.not_ <$> genAbstractSC
    , Abstract.paren_ <$> genAbstractSC
    ]

genConcrete :: MonadGen m => m (Concrete.Expr [])
genConcrete = genExprList
  where
    genExprList =
      Gen.recursive Gen.choice
        [ Concrete.Lit <$> Gen.bool <*> genWhitespace
        , Concrete.Ident <$>
          Gen.list (Range.constant 1 10) Gen.lower <*>
          genWhitespace
        ]
        [ Concrete.Not0 <$>
          genWhitespace <*>
          genExpr1List
        , Concrete.Not1 <$>
          genWhitespace1 <*>
          genExprList
        ]

    genExprNonEmpty =
      Gen.recursive Gen.choice
        [ Concrete.Lit <$>
          Gen.bool <*>
          genWhitespace1
        ]
        [ Concrete.Not0 <$>
          genWhitespace <*>
          genExpr1NonEmpty
        , Concrete.Not1 <$>
          genWhitespace1 <*>
          genExprNonEmpty
        ]

    genExpr1List =
      Concrete.Paren <$>
      genWhitespace <*>
      genExprList <*>
      genWhitespace

    genExpr1NonEmpty =
      Concrete.Paren <$>
      genWhitespace <*>
      genExprList <*>
      genWhitespace1

    genWhitespace =
      Gen.list (Range.constant 0 10) (pure Concrete.Space)

    genWhitespace1 =
      Gen.nonEmpty (Range.constant 1 10) (pure Concrete.Space)

prop_concrete_parseprint_id :: Property
prop_concrete_parseprint_id =
  property $ do
    tree <- forAll genConcrete
    let tree' = Concrete.pretty tree
    annotate tree'
    annotateShow tree'
    Just tree' === fmap Concrete.pretty (Concrete.parseExpr tree')

prop_concrete_printparse_id :: Property
prop_concrete_printparse_id =
  property $ do
    tree <- forAll genConcrete
    annotate $ Concrete.pretty tree
    Just tree === Concrete.parseExpr (Concrete.pretty tree)

{-
prop_abstract_printparseprint_print :: Property
prop_abstract_printparseprint_print =
  property $ do
    tree <- forAll genAbstract
    annotate $ Abstract.pretty tree
    let tree' = Abstract.parseExpr (Abstract.pretty tree)
    annotateShow tree'
    Just (Abstract.pretty tree) === fmap Abstract.pretty tree'
-}

prop_abstractSC_printparse_id :: Property
prop_abstractSC_printparse_id =
  property $ do
    tree <- forAll genAbstractSC
    annotate $ Abstract.pretty tree
    Just tree === Abstract.parseExpr (Abstract.pretty tree)

prop_abstract_parseprintparse_parse :: Property
prop_abstract_parseprintparse_parse =
  property $ do
    tree <- Concrete.pretty <$> forAll genConcrete
    annotate tree
    let tree' = Abstract.parseExpr tree
    annotateShow tree'
    (fmap Abstract.pretty tree' >>= Abstract.parseExpr) ===
      Abstract.parseExpr tree

prop_abstract_parseprint_id :: Property
prop_abstract_parseprint_id =
  property $ do
    tree <- Concrete.pretty <$> forAll genConcrete
    annotate tree
    let tree' = Abstract.parseExpr tree
    annotateShow tree'
    fmap Abstract.pretty tree' === Just tree

notInvolutiveTests :: [(String, String)]
notInvolutiveTests =
  [ ("not not true", "true")
  , ("not(not(true))", "(true)")
  , ("not not(true)", "(true)")
  , ("not(not true)", "true")
  , ("not not(not true)", "not true")
  , ("not(  not(not true))", "not(  true)")
  , ("((((not(not true)))))", "((((true))))")
  ]

prop_abstract_notInvolutive :: Property
prop_abstract_notInvolutive =
  withTests 1 . property $
  for_ notInvolutiveTests $ \(input, output) ->
    fmap
      (Abstract.pretty . Abstract.notInvolutive)
      (Abstract.parseExpr input)

    ===

    Just output

prop_concrete_notInvolutive :: Property
prop_concrete_notInvolutive =
  withTests 1 . property $
  for_ notInvolutiveTests $ \(input, output) ->
    fmap
      (Concrete.pretty . Concrete.notInvolutive)
      (Concrete.parseExpr input)

    ===

    Just output

main :: IO Bool
main = checkParallel $$(discover)
