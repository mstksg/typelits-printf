{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module GHC.TypeLits.Printf.Internal.Parser where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

type Parser a = a -> Type

type family RunParser (p :: Parser a) (uncons :: Maybe (Char, Symbol)) :: Maybe (a, Symbol)

data Pure :: a -> Parser a
type instance RunParser (Pure x) (Just '(c, cs)) = Just '(x, ConsSymbol c cs)
type instance RunParser (Pure x) Nothing = Just '(x, "")

data AsChar :: Char -> Parser Char
type instance RunParser (AsChar c) (Just '(d, cs)) = If (c == d) (Just '(c, cs)) Nothing
type instance RunParser (AsChar c) Nothing = Nothing

data NotChar :: Char -> Parser Char
type instance RunParser (NotChar c) (Just '(d, cs)) = If (c == d) Nothing (Just '(d, cs))
type instance RunParser (NotChar c) Nothing = Nothing

data AnyChar :: Parser Char
type instance RunParser AnyChar (Just '(c, cs)) = Just '(c, cs)
type instance RunParser AnyChar Nothing = Nothing

data Alpha :: Parser Char
type instance RunParser Alpha (Just '(c, cs)) = If (IsAlpha c) (Just '(c, cs)) Nothing
type instance RunParser Alpha Nothing = Nothing

data (<$) :: b -> Parser a -> Parser b
type family RepHelp (x :: b) (r :: Maybe (a, Symbol)) :: Maybe (b, Symbol) where
  RepHelp x Nothing = Nothing
  RepHelp x (Just '(y, s)) = Just '(x, s)
type instance RunParser (x <$ p) str = RepHelp x (RunParser p str)

-- | This is very slow because it goes down both branches
data (<|>) :: Parser a -> Parser a -> Parser a

type family ChoiceMaybe (x :: Maybe a) (y :: Maybe a) :: Maybe a where
  ChoiceMaybe (Just x) y = Just x
  ChoiceMaybe Nothing y = y
type instance RunParser (x <|> y) str = ChoiceMaybe (RunParser x str) (RunParser y str)

type Optional p = (Just <$> p) <|> Pure Nothing

data (*>) :: Parser a -> Parser b -> Parser b
type family SeqHelp (p :: Parser b) (r :: Maybe (a, Symbol)) :: Maybe (b, Symbol) where
  SeqHelp p Nothing = Nothing
  SeqHelp p (Just '(x, str)) = RunParser p (UnconsSymbol str)
type instance RunParser (x *> y) str = SeqHelp y (RunParser x str)

-- | Parse a single digit
data Digit :: Parser Nat

type family DigitHelp (d :: Maybe Nat) (cs :: Symbol) :: Maybe (Nat, Symbol) where
  DigitHelp Nothing cs = Nothing
  DigitHelp (Just d) cs = Just '(d, cs)
type instance RunParser Digit (Just '(c, cs)) = DigitHelp (CharDigit c) cs
type instance RunParser Digit Nothing = Nothing

data (<$>) :: (a -> b) -> Parser a -> Parser b
type family MapConHelp (f :: a -> b) (r :: Maybe (a, Symbol)) :: Maybe (b, Symbol) where
  MapConHelp f Nothing = Nothing
  MapConHelp f (Just '(x, str)) = Just '(f x, str)
type instance RunParser (f <$> p) str = MapConHelp f (RunParser p str)

data (<*>) :: Parser (a -> b) -> Parser a -> Parser b
type family ApHelp (r :: Maybe (a -> b, Symbol)) (q :: Parser a) :: Maybe (b, Symbol) where
  ApHelp Nothing q = Nothing
  ApHelp (Just '(f, str)) q = RunParser (f <$> q) (UnconsSymbol str)
type instance RunParser (p <*> q) str = ApHelp (RunParser p str) q

data Many :: Parser a -> Parser [a]
type instance RunParser (Many p) str = RunParser (Some p <|> Pure '[]) str
data Some :: Parser a -> Parser [a]
type instance RunParser (Some p) str = RunParser ('(:) <$> p <*> Many p) str

-- | Parse a number
data Number :: Parser Nat

type family NumberHelp (xs :: Maybe ([Nat], Symbol)) :: Maybe (Nat, Symbol) where
  NumberHelp (Just '(ns, str)) = Just '(FromDigits ns 0, str)
  NumberHelp Nothing = Nothing
type instance RunParser Number str = NumberHelp (RunParser (Some Digit) str)

data Cat :: Parser [Char] -> Parser Symbol
type family CatHelp (xs :: Maybe ([Char], Symbol)) :: Maybe (Symbol, Symbol) where
  CatHelp Nothing = Nothing
  CatHelp (Just '(cs, str)) = Just '(CatChars cs, str)
type instance RunParser (Cat p) str = CatHelp (RunParser p str)

type family EvalHelp (r :: Maybe (a, Symbol)) :: Maybe a where
  EvalHelp (Just '(x, str)) = Just x
  EvalHelp Nothing = Nothing
type EvalParser (p :: Parser a) (str :: Symbol) =
  EvalHelp (RunParser p (UnconsSymbol str)) :: Maybe a

type family EvalHelp_ (r :: Maybe (a, Symbol)) :: a where
  EvalHelp_ (Just '(x, str)) = x
  EvalHelp_ Nothing = TypeError ('Text "Parse failed")
type EvalParser_ (p :: Parser a) (str :: Symbol) = EvalHelp_ (RunParser p (UnconsSymbol str)) :: a

type family CharDigit (c :: Char) :: Maybe Nat where
  CharDigit '0' = Just 0
  CharDigit '1' = Just 1
  CharDigit '2' = Just 2
  CharDigit '3' = Just 3
  CharDigit '4' = Just 4
  CharDigit '5' = Just 5
  CharDigit '6' = Just 6
  CharDigit '7' = Just 7
  CharDigit '8' = Just 8
  CharDigit '9' = Just 9
  CharDigit c = Nothing

type family FromDigits (xs :: [Nat]) (n :: Nat) :: Nat where
  FromDigits '[] n = n
  FromDigits (a ': bs) n = FromDigits bs (n * 10 + a)

type family CatChars (cs :: [Char]) :: Symbol where
  CatChars '[] = ""
  CatChars (c ': cs) = ConsSymbol c (CatChars cs)

type family IsAlpha (c :: Char) :: Bool where
  IsAlpha 'a' = True
  IsAlpha 'b' = True
  IsAlpha 'c' = True
  IsAlpha 'd' = True
  IsAlpha 'e' = True
  IsAlpha 'f' = True
  IsAlpha 'g' = True
  IsAlpha 'h' = True
  IsAlpha 'i' = True
  IsAlpha 'j' = True
  IsAlpha 'k' = True
  IsAlpha 'l' = True
  IsAlpha 'm' = True
  IsAlpha 'n' = True
  IsAlpha 'o' = True
  IsAlpha 'p' = True
  IsAlpha 'q' = True
  IsAlpha 'r' = True
  IsAlpha 's' = True
  IsAlpha 't' = True
  IsAlpha 'u' = True
  IsAlpha 'v' = True
  IsAlpha 'w' = True
  IsAlpha 'x' = True
  IsAlpha 'y' = True
  IsAlpha 'z' = True
  IsAlpha 'A' = True
  IsAlpha 'B' = True
  IsAlpha 'C' = True
  IsAlpha 'D' = True
  IsAlpha 'E' = True
  IsAlpha 'F' = True
  IsAlpha 'G' = True
  IsAlpha 'H' = True
  IsAlpha 'I' = True
  IsAlpha 'J' = True
  IsAlpha 'K' = True
  IsAlpha 'L' = True
  IsAlpha 'M' = True
  IsAlpha 'N' = True
  IsAlpha 'O' = True
  IsAlpha 'P' = True
  IsAlpha 'Q' = True
  IsAlpha 'R' = True
  IsAlpha 'S' = True
  IsAlpha 'T' = True
  IsAlpha 'U' = True
  IsAlpha 'V' = True
  IsAlpha 'W' = True
  IsAlpha 'X' = True
  IsAlpha 'Y' = True
  IsAlpha 'Z' = True
  IsAlpha a = False
