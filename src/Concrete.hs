{-# language StandaloneDeriving, UndecidableInstances #-}
module Concrete where

import Control.Applicative ((<|>))
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

prettySpaces :: Foldable f => f Space -> String
prettySpaces = foldMap $ const " "

pretty :: Expr [] -> String
pretty = pretty' prettySpaces
  where
    pretty' :: (f Space -> String) -> Expr f -> String
    pretty' spaces e =
      case e of
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
      string "not" *>
      (fmap (\(a, b, c) -> either (Not0 (a : b)) (Not1 (a :| b)) c)
       ((,,) <$>
        space <*>
        many space <*>
        (Left <$> atom1 spaces <|> Right <$> atom0 spaces))

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
