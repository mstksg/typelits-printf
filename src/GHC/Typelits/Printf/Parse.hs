{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Typelits.Printf.Parse where

-- module GHC.Typelits.Printf.Parse (
--     RunParser
--   ) where

import           Data.Kind
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits

-- hello, we're going to attempt to implement
-- https://docs.microsoft.com/en-us/cpp/c-runtime-library/format-specification-syntax-printf-and-wprintf-functions?view=vs-2019

-- | A type synonym for a single-character symbol
type SChar = Symbol

data FieldFormat = FF
    { fmtWidth     :: Maybe Nat
    , fmtChar      :: SChar
    }

type Parser a = a -> Type

type family RunParser (p :: Parser a) (lst :: [SChar]) :: Maybe (a, [SChar])

data Sym :: SChar -> Parser SChar
type family SymHelp (c :: SChar) (lst :: [SChar]) :: Maybe (SChar, [SChar]) where
    SymHelp c (c ': cs) = 'Just '(c, cs)
    SymHelp c d         = 'Nothing
type instance RunParser (Sym c) cs = SymHelp c cs

data Pure :: a -> Parser a
type instance RunParser (Pure x) str = 'Just '(x, str)

data AnySym :: Parser SChar
type family AnySymHelp (lst :: [SChar]) :: Maybe (SChar, [SChar]) where
    AnySymHelp (c ': cs) = 'Just '(c, cs)
    AnySymHelp '[]       = 'Nothing
type instance RunParser AnySym cs = AnySymHelp cs

data (<$) :: b -> Parser a -> Parser b
type family RepHelp (x :: b) (r :: Maybe (a, [SChar])) :: Maybe (b, [SChar]) where
    RepHelp x 'Nothing        = 'Nothing
    RepHelp x ('Just '(y, s)) = 'Just '(x, s)
type instance RunParser (x <$ p) str = RepHelp x (RunParser p str)

data (<|>) :: Parser a -> Parser a -> Parser a
type family ChoiceMaybe (x :: Maybe a) (y :: Maybe a) :: Maybe a where
    ChoiceMaybe ('Just x) y = 'Just x
    ChoiceMaybe 'Nothing  y = y
type instance RunParser (x <|> y) str = ChoiceMaybe (RunParser x str) (RunParser y str)

type Optional p = LiftCon1 'Just p <|> Pure 'Nothing

data (*>) :: Parser a -> Parser b -> Parser b
type family SeqHelp (p :: Parser b) (r :: Maybe (a, [SChar])) :: Maybe (b, [SChar]) where
    SeqHelp p 'Nothing          = 'Nothing
    SeqHelp p ('Just '(x, str)) = RunParser p str
type instance RunParser (x *> y) str = SeqHelp y (RunParser x str)

-- | Parse a single digit
data Digit :: Parser Nat 
type family DigitHelp (d :: Maybe Nat) (cs :: [SChar]) :: Maybe (Nat, [SChar]) where
    DigitHelp 'Nothing  cs = 'Nothing
    DigitHelp ('Just d) cs = 'Just '(d, cs)
type instance RunParser Digit '[]       = 'Nothing
type instance RunParser Digit (c ': cs) = DigitHelp (CharDigit c) cs

data LiftCon1 :: (a -> b) -> Parser a -> Parser b
type family LiftCon1Help (f :: a -> b) (r :: Maybe (a, [SChar])) :: Maybe (b, [SChar]) where
    LiftCon1Help f 'Nothing = 'Nothing
    LiftCon1Help f ('Just '(x, str)) = 'Just '(f x, str)
type instance RunParser (LiftCon1 f p) str = LiftCon1Help f (RunParser p str)

data LiftCon2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
type family LiftCon2Help (f :: a -> b -> c) (r :: Maybe (a, [SChar])) (q :: Parser b) :: Maybe (c, [SChar]) where
    LiftCon2Help f 'Nothing          q = 'Nothing
    LiftCon2Help f ('Just '(x, str)) q = RunParser (LiftCon1 (f x) q) str
type instance RunParser (LiftCon2 f p q) str = LiftCon2Help f (RunParser p str) q

data Many :: Parser a -> Parser [a]
type instance RunParser (Many p) str = RunParser (Some p <|> Pure '[]) str
data Some :: Parser a -> Parser [a]
type instance RunParser (Some p) str = RunParser (LiftCon2 '(:) p (Many p)) str

-- | Parse a number
data Number :: Parser Nat
type family NumberHelp (xs :: Maybe ([Nat], [SChar])) :: Maybe (Nat, [SChar]) where
    NumberHelp 'Nothing           = 'Nothing
    NumberHelp ('Just '(ns, str)) = 'Just '(FromDigits ns 0, str)
type instance RunParser Number str = NumberHelp (RunParser (Many Digit) str)


type SimpleFF = Sym "%" *> LiftCon2 'FF (Optional Number) AnySym

-- type family ParseFF (lst :: [Symbol]) :: Maybe (FieldFormat, Symbol) where
--     ParseFF '[] = 'Nothing
--     ParseFF 

-- type family Parse (lst :: [Symbol]) :: [Either Symbol FieldFormat] where
--     Parse '[] = '[]
--     Parse ("%" ': ())

type family CharDigit (c :: SChar) :: Maybe Nat where
    CharDigit "0" = 'Just 0
    CharDigit "1" = 'Just 1
    CharDigit "2" = 'Just 2
    CharDigit "3" = 'Just 3
    CharDigit "4" = 'Just 4
    CharDigit "5" = 'Just 5
    CharDigit "6" = 'Just 6
    CharDigit "7" = 'Just 7
    CharDigit "8" = 'Just 8
    CharDigit "9" = 'Just 9
    CharDigit c   = 'Nothing

type family FromDigits (xs :: [Nat]) (n :: Nat) :: Nat where
    FromDigits '[]       n = n
    FromDigits (a ': bs) n = FromDigits bs (n * 10 + a)

