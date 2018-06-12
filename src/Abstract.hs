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
  | Ident String [Space]
  | Not [Space] Expr
  | Paren [Space] Expr [Space]
  deriving (Eq, Show, Generic)

_Lit :: Traversal' Expr Bool
_Lit f (Lit b sp) = (\b' -> Lit b' sp) <$> f b
_Lit f e = pure e

_Not :: Traversal' Expr Expr
_Not f (Not sp e) = Not sp <$> f e
_Not f e = pure e

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
         Paren _ _ sp -> sp)
    (\e sp ->
       case e of
         Lit b _ -> Lit b sp
         Not s e' -> Not s (e' & spaceAfter .~ sp)
         Paren s e' _ -> Paren s e' sp)

toNonEmpty :: a -> [a] -> NonEmpty a
toNonEmpty _ (a : as) = a :| as
toNonEmpty a [] = pure a

not_ :: Expr -> Expr
not_ = Not [Space]

true_ :: Expr
true_ = Lit True []

false_ :: Expr
false_ = Lit False []

prettySpaces :: Foldable f => f Space -> String
prettySpaces = foldMap (const " ")

pretty :: Expr -> String
pretty e =
  case e of
    Ident s sp -> s <> prettySpaces sp
    Lit b sp -> (if b then "true" else "false") <> prettySpaces sp
    Not sp e -> "not" <> prettySpaces sp <> pretty e
    Paren sp e sp' -> "(" <> prettySpaces sp <> pretty e <> ")" <> prettySpaces sp'

space :: Parser Space
space = Space <$ char ' '

parseExpr :: String -> Maybe Expr
parseExpr str =
  case parseString atom mempty str of
    Success a -> Just a
    _ -> Nothing
  where
    atom =
      Lit True <$ try (string "true" <* notFollowedBy lower) <*> many space <|>
      Lit False <$ try (string "false" <* notFollowedBy lower) <*> many space <|>
      Paren <$ char '(' <*> many space <*> atom <* char ')' <*> many space <|>
      Not <$ try (string "not" <* notFollowedBy lower) <*> many space <*> atom <|>
      Ident <$> some lower <*> many space

notInvolutive :: Expr -> Expr
notInvolutive =
  rewrite (^? underParens._Not.underParens._Not)
