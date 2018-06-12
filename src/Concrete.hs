{-# language StandaloneDeriving, UndecidableInstances #-}
module Concrete where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Text.Trifecta hiding (space)

data Space = Space deriving (Eq, Show)

data Expr f
  = Lit Bool (f Space)
  | Not0 [Space] (Expr1 f)
  | Not1 (NonEmpty Space) (Expr f)
  | And10 (Expr NonEmpty) [Space] (Expr1 f)
  | And11 (Expr NonEmpty) (NonEmpty Space) (Expr f)
  | Expr1 (Expr1 f)
deriving instance Eq (f Space) => Eq (Expr f)
deriving instance Show (f Space) => Show (Expr f)

data Expr1 f
  = Paren [Space] (Expr []) (f Space)
  | And00 (Expr1 []) [Space] (Expr1 f)
  | And01 (Expr1 []) (NonEmpty Space) (Expr f)
deriving instance Eq (f Space) => Eq (Expr1 f)
deriving instance Show (f Space) => Show (Expr1 f)

not_ :: Expr f -> Expr f
not_ = Not1 (Space :| [])

and_ :: Expr NonEmpty -> Expr f -> Expr f
and_ a = And11 a (Space :| [])

true_ :: Expr []
true_ = Lit True []

false_ :: Expr []
false_ = Lit False []

prettySpaces :: Foldable f => f Space -> String
prettySpaces = foldMap $ const " "

pretty :: (f Space -> String) -> Expr f -> String
pretty spaces e =
  case e of
    Lit b sp -> (if b then "true" else "false") <> spaces sp
    Not0 sp e1 -> "not" <> prettySpaces sp <> pretty1 spaces e1
    Not1 sp e -> "not" <> prettySpaces sp <> pretty spaces e
    And10 e sp e1 -> pretty prettySpaces e <> "and" <> prettySpaces sp <> pretty1 spaces e1
    And11 e sp e' -> pretty prettySpaces e <> "and" <> prettySpaces sp <> pretty spaces e'
    Expr1 e1 -> pretty1 spaces e1

pretty1 :: (f Space -> String) -> Expr1 f -> String
pretty1 spaces e =
  case e of
    Paren sp e' sp' -> "(" <> prettySpaces sp <> pretty prettySpaces e' <> ")" <> spaces sp'
    And00 e sp e1 -> pretty1 prettySpaces e <> "and" <> prettySpaces sp <> pretty1 spaces e1
    And01 e1 sp e -> pretty1 prettySpaces e1 <> "and" <> prettySpaces sp <> pretty spaces e

space :: Parser Space
space = Space <$ char ' '

parseExpr :: Parser (f Space) -> String -> Maybe (Expr f)
parseExpr sp str =
  case parseString (expr sp) mempty str of
    Success a -> Just a
    _ -> Nothing
  where
    expr :: Parser (f Space) -> Parser (Expr f)
    expr spaces = and0 spaces

    expr1 :: Parser (f Space) -> Parser (Expr1 f)
    expr1 spaces = and1 spaces

    and0 :: Parser (f Space) -> Parser (Expr f)
    and0 spaces = do
      e <- atom0 $ some1 space
      rest <- optional $ do
        string "and"
        sp <- optional space
        case sp of
          Nothing -> And10 e [] <$> expr1 spaces
          Just sp' -> do
            sps <- many space
            And10 e (sp' : sps) <$> expr1 spaces <|> And11 e (sp' :| sps) <$> expr spaces
      pure $ fromMaybe e rest

    and1 :: Parser (f Space) -> Parser (Expr1 f)
    and1 spaces = do
      e1 <- atom1 $ many space
      rest <- optional $ do
        string "and"
        sp <- optional space
        case sp of
          Nothing -> And00 e1 [] <$> expr1 spaces
          Just sp' -> do
            sps <- many space
            And00 e1 (sp' : sps) <$> expr1 spaces <|> And01 e1 (sp' :| sps) <$> expr spaces
      pure $ fromMaybe e1 rest

    notExpr :: Parser (f Space) -> Parser (Expr f)
    notExpr spaces =
      string "not" *>
      (fmap (\(a, b, c) -> either (Not0 (a : b)) (Not1 (a :| b)) c)
       ((,,) <$>
        space <*>
        many space <*>
        (Left <$> expr1 spaces <|> Right <$> expr spaces))

       <|>

       Not0 [] <$> expr1 spaces)

    atom0 :: Parser (f Space) -> Parser (Expr f)
    atom0 spaces =
      Lit True <$ string "true" <*> spaces <|>
      Lit False <$ string "false" <*> spaces <|>
      notExpr spaces

    atom1 :: Parser (f Space) -> Parser (Expr1 f)
    atom1 spaces =
      Paren <$ char '(' <*> many space <*> expr (many space) <* char ')' <*> spaces
