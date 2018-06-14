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
-}

data Space = Space deriving (Eq, Show)

data Expr f
  = Lit Bool (f Space)
  | Not0 (SExpr1 f)
  | Not1 (SExpr0 f)
  | Ident String (f Space)
  | Expr1 (Expr1 f)
deriving instance Eq (f Space) => Eq (Expr f)
deriving instance Show (f Space) => Show (Expr f)

validate :: Abstract.Expr anything -> Either SyntaxError (Abstract.Expr)

data SExpr1 f =
  SExpr1
    [Space] (Expr1 f)
deriving instance Eq (f Space) => Eq (SExpr1 f)
deriving instance Show (f Space) => Show (SExpr1 f)

data SExpr0 f =
  SExpr0
    (NonEmpty Space) (Expr f)
deriving instance Eq (f Space) => Eq (SExpr0 f)
deriving instance Show (f Space) => Show (SExpr0 f)

data SExprX f =
  SExprX0 (SExpr0 f) 
  | SExprX1 (SExpr1 f) 
deriving instance Eq (f Space) => Eq (SExprX f)
deriving instance Show (f Space) => Show (SExprX f)

oneToZero :: Space -> SExpr1 [] -> SExpr0 []
oneToZero sp (SExpr1 sps (Paren _ e _)) = SExpr0 (case sps of; [] -> pure sp; a : as -> a :| as) e

zeroToOne :: SExpr0 [] -> SExpr1 []
zeroToOne (SExpr0 (s :| ss) e) = SExpr1 (s : ss) (Paren [] e [])

-- forall (s :: Space), oneToZero s . zeroToOne = id
-- forall (s :: Space), let k = zeroToOne . oneToZero s in k . k = k

data WhatIsThis a b c =
  WhatIsThis
    (a -> b -> c)
    (c -> b)

-- forall x (WhatIsThis f g), f x . g = id
-- forall x (WhatIsThis f g), let k = g . f x in k . k = k


_Not :: Prism' (Expr f) (SExprX f)
_Not = 
  prism'
    (\case; SExprX0 x -> Not1 x; SExprX1 x -> Not0 x)
    (\case; Not0 x -> Just $ SExprX1 x; Not1 x -> Just $ SExprX0 x; _ -> Nothing)

class AsSExpr0 a where
  _SExpr0 :: 
    Prism' (a f) (SExpr0 f)

instance AsSExpr0 SExpr0 where
  _SExpr0 = id

instance AsSExpr0 SExprX where
  _SExpr0 =
    prism'
      SExprX0
      (\case 
          SExprX0 x -> Just x
          SExprX1 _ -> Nothing)

class AsSExpr1 a where
  _SExpr1 :: 
    Prism' (a f) (SExpr1 f)

instance AsSExpr1 SExpr1 where
  _SExpr1 = id

instance AsSExpr1 SExprX where
  _SExpr1 =
    prism'
      SExprX1
      (\case 
          SExprX1 x -> Just x
          SExprX0 _ -> Nothing)

data Expr1 f
  = Paren [Space] (Expr []) (f Space)
deriving instance Eq (f Space) => Eq (Expr1 f)
deriving instance Show (f Space) => Show (Expr1 f)

not_ :: Expr f -> Expr f
not_ = Not1 . SExpr0 (Space :| [])

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
        Not0 (SExpr1 sp e1) -> "not" <> prettySpaces sp <> pretty1 spaces e1
        Not1 (SExpr0 sp e) -> "not" <> prettySpaces sp <> pretty' spaces e
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
    expr space = atom0 space <|> Expr1 <$> atom1 space

    expr1 :: Parser (f Space) -> Parser (Expr1 f)
    expr1 = atom1

    notExpr :: Parser (f Space) -> Parser (Expr f)
    notExpr spaces =
      (try $ string "not" <* notFollowedBy lower) *>
      (fmap (\(a, b, c) -> either (Not0 . SExpr1 (a : b)) (Not1 . SExpr0 (a :| b)) c)
       ((,,) <$>
        space <*>
        many space <*>
        (Left <$> atom1 spaces <|> Right <$> atom0 spaces))

       <|>

       Not0 . SExpr1 [] <$> expr1 spaces)

    atom0 :: Parser (f Space) -> Parser (Expr f)
    atom0 spaces =
      Lit True <$ try (string "true" <* notFollowedBy lower) <*> spaces <|>
      Lit False <$ try (string "false" <* notFollowedBy lower) <*> spaces <|>
      notExpr spaces <|>
      Ident <$> some lower <*> spaces

    atom1 :: Parser (f Space) -> Parser (Expr1 f)
    atom1 spaces =
      Paren <$ char '(' <*> many space <*> expr (many space) <* char ')' <*> spaces

rewrite
  :: (Expr [] -> Maybe (Expr []))
  -> (Expr1 [] -> Maybe (Expr1 []))
  -> Expr [] -> Expr []
rewrite fe fe1 e =
  case e of
    Not0 (SExpr1 sp e1) -> repeatedly $ Not0 . SExpr1 sp $ rewrite1 fe fe1 e1
    Not1 (SExpr0 sp e') -> repeatedly $ Not1 . SExpr0 sp $ rewrite fe fe1 e'
    Expr1 e1 -> repeatedly $ Expr1 $ rewrite1 fe fe1 e1
    _ -> repeatedly e
  where
    repeatedly e = maybe e (rewrite fe fe1) (fe e)

rewrite1
  :: (Expr [] -> Maybe (Expr []))
  -> (Expr1 [] -> Maybe (Expr1 []))
  -> Expr1 [] -> Expr1 []
rewrite1 fe fe1 e1 =
  case e1 of
    Paren sp e sp' -> repeatedly1 $ Paren sp (rewrite fe fe1 e) sp'
  where
    repeatedly1 e1 = maybe e1 (rewrite1 fe fe1) (fe1 e1)

notInvolutive :: Expr [] -> Expr []
notInvolutive =
  rewrite
    (\case
        Not0 (SExpr1 _ (Paren _ (Not0 (SExpr1 _ e)) _)) -> Just $ Expr1 e
        Not0 (SExpr1 _ (Paren _ (Not1 (SExpr0 _ e)) _)) -> Just e
        Not1 (SExpr0 _ (Not0 (SExpr1 _ e))) -> Just $ Expr1 e
        Not1 (SExpr0 _ (Not1 (SExpr0 _ e))) -> Just e
        _ -> Nothing)
    (\case
        Paren{} -> Nothing)
