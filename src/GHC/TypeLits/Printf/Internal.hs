{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : GHC.TypeLits.Printf.Internal
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal workings of the printf mechanisms, exposed for potential
-- debugging purposes.
--
-- Please do not use this module for anything besides debugging, as is
-- definitely very unstable and might go away or change dramatically
-- between versions.
module GHC.TypeLits.Printf.Internal (
  ParseFmtStr,
  ParseFmtStr_,
  ParseFmt,
  ParseFmt_,
  FormatAdjustment (..),
  ShowFormat,
  FormatSign (..),
  WidthMod (..),
  Flags (..),
  EmptyFlags,
  FieldFormat (..),
  SChar,
  Demote,
  Reflect (..),
  FormatType (..),
  Printf (..),
  FormatFun (..),
  PFmt (..),
  pfmt,
  mkPFmt,
  mkPFmt_,
  PHelp (..),
) where

import Data.Int
import Data.Proxy
import Data.Symbol.Utils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import GHC.OverloadedLabels
import GHC.TypeLits hiding (SChar)
import GHC.TypeLits.Printf.Internal.Unsatisfiable
import GHC.TypeLits.Printf.Parse
import qualified Text.Printf as P

-- | Typeclass associating format types (@d@, @f@, etc.) with the types
-- that can be formatted by them.
--
-- You can extend the printf methods here for your own types by writing
-- your instances here.
class FormatType (t :: SChar) a where
  formatArg :: p t -> a -> P.FieldFormat -> ShowS
  default formatArg :: P.PrintfArg a => p t -> a -> P.FieldFormat -> ShowS
  formatArg _ = P.formatArg

instance FormatType "c" Char
instance FormatType "c" Word8
instance FormatType "c" Word16

instance FormatType "d" Char
instance FormatType "d" Int
instance FormatType "d" Int8
instance FormatType "d" Int16
instance FormatType "d" Int32
instance FormatType "d" Int64
instance FormatType "d" Integer
instance FormatType "d" Natural
instance FormatType "d" Word
instance FormatType "d" Word8
instance FormatType "d" Word16
instance FormatType "d" Word32
instance FormatType "d" Word64

instance FormatType "o" Char
instance FormatType "o" Int
instance FormatType "o" Int8
instance FormatType "o" Int16
instance FormatType "o" Int32
instance FormatType "o" Int64
instance FormatType "o" Integer
instance FormatType "o" Natural
instance FormatType "o" Word
instance FormatType "o" Word8
instance FormatType "o" Word16
instance FormatType "o" Word32
instance FormatType "o" Word64

instance FormatType "x" Int
instance FormatType "x" Int8
instance FormatType "x" Int16
instance FormatType "x" Int32
instance FormatType "x" Int64
instance FormatType "x" Integer
instance FormatType "x" Natural
instance FormatType "x" Word
instance FormatType "x" Word8
instance FormatType "x" Word16
instance FormatType "x" Word32
instance FormatType "x" Word64

instance FormatType "X" Char
instance FormatType "X" Int
instance FormatType "X" Int8
instance FormatType "X" Int16
instance FormatType "X" Int32
instance FormatType "X" Int64
instance FormatType "X" Integer
instance FormatType "X" Natural
instance FormatType "X" Word
instance FormatType "X" Word8
instance FormatType "X" Word16
instance FormatType "X" Word32
instance FormatType "X" Word64

instance FormatType "b" Char
instance FormatType "b" Int
instance FormatType "b" Int8
instance FormatType "b" Int16
instance FormatType "b" Int32
instance FormatType "b" Int64
instance FormatType "b" Integer
instance FormatType "b" Natural
instance FormatType "b" Word
instance FormatType "b" Word8
instance FormatType "b" Word16
instance FormatType "b" Word32
instance FormatType "b" Word64

instance FormatType "u" Char
instance FormatType "u" Int
instance FormatType "u" Int8
instance FormatType "u" Int16
instance FormatType "u" Int32
instance FormatType "u" Int64
instance FormatType "u" Integer
instance FormatType "u" Natural
instance FormatType "u" Word
instance FormatType "u" Word8
instance FormatType "u" Word16
instance FormatType "u" Word32
instance FormatType "u" Word64

instance FormatType "f" Double
instance FormatType "f" Float

instance FormatType "F" Double
instance FormatType "F" Float

instance FormatType "g" Double
instance FormatType "g" Float

instance FormatType "G" Double
instance FormatType "G" Float

instance FormatType "e" Double
instance FormatType "e" Float

instance FormatType "E" Double
instance FormatType "E" Float

instance FormatType "s" String
instance FormatType "s" T.Text where
  formatArg _ = P.formatArg . T.unpack
instance FormatType "s" TL.Text where
  formatArg _ = P.formatArg . TL.unpack

-- | Treats as @c@
instance FormatType "v" Char

-- | Treats as @d@
instance FormatType "v" Int

-- | Treats as @d@
instance FormatType "v" Int8

-- | Treats as @d@
instance FormatType "v" Int16

-- | Treats as @d@
instance FormatType "v" Int32

-- | Treats as @d@
instance FormatType "v" Int64

-- | Treats as @d@
instance FormatType "v" Integer

-- | Treats as @u@
instance FormatType "v" Natural

-- | Treats as @u@
instance FormatType "v" Word

-- | Treats as @u@
instance FormatType "v" Word8

-- | Treats as @u@
instance FormatType "v" Word16

-- | Treats as @u@
instance FormatType "v" Word32

-- | Treats as @u@
instance FormatType "v" Word64

-- | Treats as @g@
instance FormatType "v" Double

-- | Treats as @g@
instance FormatType "v" Float

-- | Treats as @s@
instance FormatType "v" String

-- | Treats as @s@
instance FormatType "v" T.Text where
  formatArg _ = P.formatArg . T.unpack

-- | Treats as @s@
instance FormatType "v" TL.Text where
  formatArg _ = P.formatArg . TL.unpack

-- | The typeclass supporting polyarity used by
-- 'GHC.TypeLits.Printf.printf'. It works in mostly the same way as
-- 'P.PrintfType' from "Text.Printf", and similar the same as
-- 'Data.Symbol.Examples.Printf.FormatF'.  Ideally, you will never have to
-- run into this typeclass or have to deal with it directly.
--
-- Every item in the first argument of 'FormatFun' is a chunk of the
-- formatting string, split between format holes ('Right') and string
-- chunks ('Left').
--
-- If you want to see some useful error messages for feedback, 'pHelp' can
-- be useful:
--
-- >>> pHelp $ printf @"You have %.2f dollars, %s" 3.62
-- -- ERROR: Call to printf missing argument fulfilling "%s"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
class FormatFun (ffs :: [Either Symbol FieldFormat]) fun where
  formatFun :: p ffs -> String -> fun

-- | A useful tool for helping the type system give useful errors for
-- 'GHC.TypeLits.Printf.printf':
--
-- >>> printf @"You have ".2f" dollars, %s" 3.26 :: PHelp
-- -- ERROR: Call to printf missing argument fulfilling "%s"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
--
-- Mostly useful if you want to force a useful type error to help see what
-- is going on.
--
-- See also 'pHelp'
newtype PHelp = PHelp
  { pHelp :: String
  -- ^ A useful helper function for helping the type system give useful
  -- errors for 'printf':
  --
  -- >>> pHelp $ printf @"You have %.2f dollars, %s" 3.62
  -- -- ERROR: Call to printf missing argument fulfilling "%s"
  -- -- Either provide an argument or rewrite the format string to not expect
  -- -- one.
  --
  -- Mostly useful if you want to force a useful type error to help see
  -- what is going on.
  }

-- What is this incoherent instance for? It serves to make String the *default*
-- result when the result type is entirely ambiguous.
instance {-# INCOHERENT #-} a ~ String => FormatFun '[] a where
    formatFun _ = id
instance FormatFun '[] PHelp where
    formatFun _ = PHelp
instance FormatFun '[] T.Text where
    formatFun _ = T.pack
instance FormatFun '[] TL.Text where
    formatFun _ = TL.pack
instance a ~ () => FormatFun '[] (IO a) where
    formatFun _ = putStr

instance Unsatisfiable ( 'Text "An extra argument of type "
               ':<>: 'ShowType a
               ':<>: 'Text " was given to a call to printf."
               ':$$: 'Text "Either remove the argument, or rewrite the format string to include the appropriate hole"
                   )
      => FormatFun '[] (a -> b) where
    formatFun = unsatisfiable

instance (KnownSymbol str, FormatFun ffs fun) => FormatFun ('Left str ': ffs) fun where
  formatFun _ str = formatFun (Proxy @ffs) (str ++ symbolVal (Proxy @str))

type family IsFunction fun where
  IsFunction (_ -> _) = 'True
  IsFunction _ = 'False

instance FormatFun' (IsFunction afun) ff ffs afun => FormatFun ('Right ff ': ffs) afun where
    formatFun _ = formatFun' @(IsFunction afun) (Proxy @'(ff,ffs))

-- A helper class for the case where we expect to produce a function
class FormatFun' (is_function :: Bool) (ff :: FieldFormat) (ffs :: [Either Symbol FieldFormat]) fun where
    formatFun' :: p '(ff, ffs) -> String -> fun

-- What are these INCOHERENT horrors??? They're purely about getting good
-- error messages. If GHC can see that we're definitely expected to produce
-- something other than a function, then we want to get a custom error message
-- rather than the one the compiler would produce. But if the result type is
-- *ambiguous*, then we want to choose the instance that might make progress.
instance {-# INCOHERENT #-} (afun ~ (arg -> fun), Reflect ff, ff ~ 'FF f w p m c, FormatType c arg, FormatFun ffs fun) => FormatFun' is_function ff ffs afun where
    formatFun' _ str x = formatFun (Proxy @ffs) (str ++ formatArg (Proxy @c) x ff "")
      where
        ff = reflect (Proxy @ff)

instance Unsatisfiable (MissingError ff) => FormatFun' 'False ff ffs notafun where
    formatFun' = unsatisfiable

type family MissingError ff where
  MissingError ff =
    'Text "Call to printf missing an argument fulfilling \"%"
      ':<>: 'Text (ShowFormat ff)
      ':<>: 'Text "\""
      ':$$: 'Text "Either provide an argument or rewrite the format string to not expect one."

class Printf (str :: Symbol) fun where
  -- | A version of 'GHC.TypeLits.Printf.printf' taking an explicit
  -- proxy, which allows usage without /TypeApplications/
  --
  -- >>> putStrLn $ printf_ (Proxy :: Proxy "You have %.2f dollars, %s") 3.62 "Luigi"
  -- You have 3.62 dollars, Luigi
  printf_ :: p str -> fun

instance (Listify str lst, ffs ~ ParseFmtStr_ lst, FormatFun ffs fun) => Printf str fun where
  printf_ _ = formatFun (Proxy @ffs) ""

-- | Utility type powering 'pfmt'.  See documentation for 'pfmt' for more
-- information on usage.
--
-- Using /OverloadedLabels/, you never need to construct this directly
-- can just write @#f@ and a @'PFmt' "f"@ will be generated.  You can also
-- create this using 'mkPFmt' or 'mkPFmt_', in the situations where
-- /OverloadedLabels/ doesn't work or is not wanted.
newtype PFmt c = PFmt P.FieldFormat

-- | A version of 'mkPFmt' that takes an explicit proxy input.
--
-- >>> pfmt (mkPFmt_ (Proxy :: Proxy ".2f")) 3.6234124
-- "3.62"
mkPFmt_ ::
  forall str lst ff f w q m c p.
  (Listify str lst, ff ~ ParseFmt_ lst, Reflect ff, ff ~ 'FF f w q m c) =>
  p str ->
  PFmt c
mkPFmt_ _ = PFmt ff
  where
    ff = reflect (Proxy @ff)

-- | Useful for using 'pfmt' without /OverloadedLabels/, or also when
-- passing format specifiers that aren't currently allowed with
-- /OverloadedLabels/ until GHC 8.10+ (like @#.2f@).
--
-- >>> pfmt (mkPFmt @".2f") 3.6234124
-- "3.62"
mkPFmt ::
  forall str lst ff f w q m c.
  (Listify str lst, ff ~ ParseFmt_ lst, Reflect ff, ff ~ 'FF f w q m c) =>
  PFmt c
mkPFmt = mkPFmt_ @str @lst (Proxy @str)

instance (Listify str lst, ff ~ ParseFmt_ lst, Reflect ff, ff ~ 'FF f w p m c) => IsLabel str (PFmt c) where
  fromLabel = mkPFmt @str @lst

-- | Parse and run a /single/ format hole on a single vale.  Can be useful
-- for formatting individual items or for testing your own custom instances of
-- 'FormatType'.
--
-- Usually meant to be used with /OverloadedLabels/:
--
-- >>> pfmt #f 3.62
-- "3.62"
--
-- However, current versions of GHC disallow labels that aren't valid
-- identifier names, disallowing things like @'pfmt' #.2f 3.62@.  While
-- there is an
-- <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0170-unrestricted-overloadedlabels.rst
-- approved proposal> that allows this, if you are using an earlier GHC
-- version, you can get around this using 'mkPFmt':
--
-- >>> pfmt (mkPFmt @".2f") 3.6234124
-- "3.62"
--
-- Ideally we'd want to be able to write
--
-- >>> pfmt #.2f 3.6234124
-- "3.62"
--
-- (which should be possible in GHC 8.10+)
--
-- Note that the format string should not include the leading @%@.
pfmt :: forall c a. FormatType c a => PFmt c -> a -> String
pfmt (PFmt ff) x = formatArg (Proxy @c) x ff ""
