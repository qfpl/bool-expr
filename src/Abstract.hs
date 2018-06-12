{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
module Abstract where

import Control.Applicative ((<|>))
import Control.Lens.Fold ((^?))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Plated (Plated(..), gplate, rewrite)
import Control.Lens.Setter ((%~), (.~))
import Control.Lens.Traversal (Traversal')
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Text.Trifecta hiding (space)

data Space = Space deriving (Eq, Show)

data Expr
  = Lit Bool [Space]
  | Not [Space] Expr
  | And Expr [Space] Expr
  | Paren [Space] Expr [Space]
  deriving (Eq, Show, Generic)

_Lit :: Traversal' Expr Bool
_Lit f (Lit b sp) = (\b' -> Lit b' sp) <$> f b
_Lit f e = pure e

_Not :: Traversal' Expr Expr
_Not f (Not sp e) = Not sp <$> f e
_Not f e = pure e

_And :: Traversal' Expr (Expr, Expr)
_And f (And e sp e') = (\(e1, e2) -> And e1 sp e2) <$> f (e, e')
_And f e = pure e

_Paren :: Traversal' Expr Expr
_Paren f (Paren sp e sp') = (\e' -> Paren sp e' sp') <$> f e
_Paren f e = pure e

underParens :: Traversal' Expr Expr
underParens f (Paren sp e sp') = (\e' -> Paren sp e' sp') <$> underParens f e
underParens f e = f e

instance Plated Expr where
  plate = gplate

spaceAfter :: Lens' Expr [Space]
spaceAfter =
  lens
    (\e ->
       case e of
         Lit _ sp -> sp
         Not _ e' -> e' ^. spaceAfter
         And _ _ e' -> e' ^. spaceAfter
         Paren _ _ sp -> sp)
    (\e sp ->
       case e of
         Lit b _ -> Lit b sp
         Not s e' -> Not s (e' & spaceAfter .~ sp)
         And e' s e'' -> And e' s (e'' & spaceAfter .~ sp)
         Paren s e' _ -> Paren s e' sp)

toNonEmpty :: a -> [a] -> NonEmpty a
toNonEmpty _ (a : as) = a :| as
toNonEmpty a [] = pure a

not_ :: Expr -> Expr
not_ = Not [Space]

and_ :: Expr -> Expr -> Expr
and_ a = And (a & spaceAfter %~ toList . toNonEmpty Space) [Space]

true_ :: Expr
true_ = Lit True []

false_ :: Expr
false_ = Lit False []

prettySpaces :: Foldable f => f Space -> String
prettySpaces = foldMap (const " ")

pretty :: Expr -> String
pretty e =
  case e of
    Lit b sp -> (if b then "true" else "false") <> prettySpaces sp
    Not sp e -> "not" <> prettySpaces sp <> pretty e
    And e sp e' -> pretty e <> "and" <> prettySpaces sp <> pretty e'
    Paren sp e sp' -> "(" <> prettySpaces sp <> pretty e <> ")" <> prettySpaces sp'

space :: Parser Space
space = Space <$ char ' '

parseExpr :: String -> Maybe Expr
parseExpr str =
  case parseString expr mempty str of
    Success a -> Just a
    _ -> Nothing
  where
    expr =
      foldl (\a (sp, at) -> And a sp at) <$>
      atom <*>
      many ((,) <$ string "and" <*> many space <*> atom)
    atom =
      Lit True <$ string "true" <*> many space <|>
      Lit False <$ string "false" <*> many space <|>
      Paren <$ char '(' <*> many space <*> expr <* char ')' <*> many space <|>
      Not <$ string "not" <*> many space <*> expr

notInvolutive :: Expr -> Expr
notInvolutive =
  rewrite (^? underParens._Not.underParens._Not)
