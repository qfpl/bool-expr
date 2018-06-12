{-# language TemplateHaskell #-}
module Main where

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
    ]
    [ Abstract.Not <$> genWhitespace <*> genAbstract
    , Abstract.Paren <$> genWhitespace <*> genAbstract <*> genWhitespace
    ]
  where
    genWhitespace =
      Gen.list (Range.constant 0 10) (pure Abstract.Space)

genConcrete :: MonadGen m => m (Concrete.Expr [])
genConcrete = genExprList
  where
    genExprList =
      Gen.recursive Gen.choice
        [ Concrete.Lit <$> Gen.bool <*> genWhitespace
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

prop_concrete_printparseprint_print :: Property
prop_concrete_printparseprint_print =
  property $ do
    tree <- forAll genConcrete
    annotate $ Concrete.pretty tree
    let tree' = Concrete.parseExpr (Concrete.pretty tree)
    annotateShow tree'
    Just (Concrete.pretty tree) ===
      fmap Concrete.pretty tree'

prop_abstract_printparseprint_print :: Property
prop_abstract_printparseprint_print =
  property $ do
    tree <- forAll genAbstract
    annotate $ Abstract.pretty tree
    let tree' = Abstract.parseExpr (Abstract.pretty tree)
    annotateShow tree'
    Just (Abstract.pretty tree) ===
      fmap Abstract.pretty tree'

main :: IO Bool
main = checkParallel $$(discover)
