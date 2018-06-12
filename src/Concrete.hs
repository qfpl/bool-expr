{-# language StandaloneDeriving, UndecidableInstances #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
module Concrete where

import Control.Applicative ((<|>))
import Control.Lens.Fold ((^?))
import Control.Lens.Traversal (Traversal')
import Control.Lens.Prism (Prism', prism')
import Control.Lens.Review (re)
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Text.Trifecta hiding (space)

{-
Notes: got rid of boolean 'and' because it was introducing too
much complexity
--}
data Space = Space deriving (Eq, Show)

data Expr f
  = Lit Bool (f Space)
  | Not0 [Space] (Expr1 f)
  | Not1 (NonEmpty Space) (Expr f)
  | Ident String (f Space)
  | Expr1 (Expr1 f)
deriving instance Eq (f Space) => Eq (Expr f)
deriving instance Show (f Space) => Show (Expr f)

data Expr1 f
  = Paren [Space] (Expr []) (f Space)
deriving instance Eq (f Space) => Eq (Expr1 f)
deriving instance Show (f Space) => Show (Expr1 f)

not_ :: Expr f -> Expr f
not_ = Not1 (Space :| [])

true_ :: Expr []
true_ = Lit True []

false_ :: Expr []
false_ = Lit False []

ident_ :: String -> Expr []
ident_ s = Ident s []

prettySpaces :: Foldable f => f Space -> String
prettySpaces = foldMap $ const " "

pretty :: Expr [] -> String
pretty = pretty' prettySpaces
  where
    pretty' :: (f Space -> String) -> Expr f -> String
    pretty' spaces e =
      case e of
        Ident s sp -> s <> spaces sp
        Lit b sp -> (if b then "true" else "false") <> spaces sp
        Not0 sp e1 -> "not" <> prettySpaces sp <> pretty1 spaces e1
        Not1 sp e -> "not" <> prettySpaces sp <> pretty' spaces e
        Expr1 e1 -> pretty1 spaces e1

    pretty1 :: (f Space -> String) -> Expr1 f -> String
    pretty1 spaces e =
      case e of
        Paren sp e' sp' -> "(" <> prettySpaces sp <> pretty' prettySpaces e' <> ")" <> spaces sp'

space :: Parser Space
space = Space <$ char ' '

parseExpr :: String -> Maybe (Expr [])
parseExpr str =
  case parseString (expr $ many space) mempty str of
    Success a -> Just a
    _ -> Nothing
  where
    expr :: Parser (f Space) -> Parser (Expr f)
    expr = atom0

    expr1 :: Parser (f Space) -> Parser (Expr1 f)
    expr1 = atom1

    notExpr :: Parser (f Space) -> Parser (Expr f)
    notExpr spaces =
      (try $ string "not" <* notFollowedBy lower) *>
      (fmap (\(a, b, c) -> either (Not0 (a : b)) (Not1 (a :| b)) c)
       ((,,) <$>
        space <*>
        many space <*>
        (Left <$> atom1 spaces <|> Right <$> atom0 spaces))

       <|>

       Not0 [] <$> expr1 spaces)

    atom0 :: Parser (f Space) -> Parser (Expr f)
    atom0 spaces =
      Lit True <$ try (string "true" <* notFollowedBy lower) <*> spaces <|>
      Lit False <$ try (string "false" <* notFollowedBy lower) <*> spaces <|>
      notExpr spaces <|>
      Ident <$> some lower <*> spaces

    atom1 :: Parser (f Space) -> Parser (Expr1 f)
    atom1 spaces =
      Paren <$ char '(' <*> many space <*> expr (many space) <* char ')' <*> spaces

notInvolutive :: Expr f -> Expr f
notInvolutive = undefined
