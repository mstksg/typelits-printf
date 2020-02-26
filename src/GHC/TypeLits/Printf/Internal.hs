{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_HADDOCK not-home        #-}

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
    ParseFmtStr
  , ParseFmtStr_
  , ParseFmt
  , ParseFmt_
  , FormatAdjustment(..)
  , ShowFormat
  , FormatSign(..)
  , WidthMod(..)
  , Flags(..)
  , EmptyFlags
  , FieldFormat(..)
  , SChar
  , Demote
  , Reflect(..)
  , FormatType(..)
  , PP(..)
  , RPrintf(..)
  , FormatArgs
  , RFormat(..)
  , Printf(..)
  , FormatFun(..)
  , PFmt(..)
  , pfmt
  , mkPFmt, mkPFmt_
  , PHelp(..)
  ) where

import           Data.Int
import           Data.Proxy
import           Data.Symbol.Utils
import           Data.Vinyl
import           Data.Word
import           GHC.OverloadedLabels
import           GHC.TypeLits
import           GHC.TypeLits.Printf.Parse
import           Numeric.Natural
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Text.Printf               as P

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

-- | Required wrapper around inputs to 'GHC.TypeLits.Printf.pprintf'
-- (guarded polyarity).  See documentation for
-- 'GHC.TypeLits.Printf.pprintf' for examples of usage.
--
-- You can "wrap" any value in 'PP' as long as it can be formatted as the
-- format type indicated.
--
-- For example, to make a @'PP' "f"@, you can use @'PP' 3.5@ or @'PP'
-- 94.2@, but not @'PP' (3 :: Int)@ or @'PP' "hello"@.  To make a value of
-- type @'PP' c@, you must wrap a value that can be formatted via @c@.
data PP (c :: SChar) = forall a. FormatType c a => PP a

-- | A heterogeneous list (from "Data.Vinyl.Core") used for calling with
-- 'GHC.TypeLits.Printf.rprintf'.  Instead of supplying the inputs as
-- different arguments, we can gather all the inputs into a single list to
-- give to 'GHC.TypeLits.Printf.rprintf'.
--
-- >>> :t rprintf @"You have %.2f dollars, %s"
-- FormatArgs '["f", "s"] -> String
--
-- To construct a @'FormatArgs' '["f", "s"]@, you need to give a value
-- formattable by @f@ and a value formattable by @s@, given like a linked
-- list, with 'GHC.TypeLits.Printf.:%' for cons and 'RNil' for nil.
--
-- >>> putStrLn $ rprintf @"You have %.2f dollars, %s" (3.62 :% "Luigi" :% RNil)
-- You have 3.62 dollars, Luigi
--
-- (This should evoke the idea of of @3.62 : "Luigi" : []@, even though the
-- latter is not possible in Haskell)
type FormatArgs = Rec PP

class RFormat (ffs :: [Either Symbol FieldFormat]) (ps :: [SChar]) | ffs -> ps where
    rformat :: p ffs -> FormatArgs ps -> ShowS

instance RFormat '[] '[] where
    rformat _ _ = id

instance (KnownSymbol str, RFormat ffs ps) => RFormat ('Left str ': ffs) ps where
    rformat _ r = showString (symbolVal (Proxy @str))
                . rformat (Proxy @ffs) r

instance (Reflect ff, ff ~ 'FF f w p m c, RFormat ffs ps) => RFormat ('Right ff ': ffs) (c ': ps) where
    rformat _ (PP x :& xs) = formatArg (Proxy @c) x ff
                           . rformat (Proxy @ffs) xs
      where
        ff = reflect (Proxy @ff)

class RPrintf (str :: Symbol) ps where
    -- | A version of 'GHC.TypeLits.Printf.rprintf' taking an explicit
    -- proxy, which allows usage without /TypeApplications/
    --
    -- >>> :t rprintf_ (Proxy :: Proxy "You have %.2f dollars, %s")
    -- FormatArgs '["f", "s"] -> String
    rprintf_ :: p str -> FormatArgs ps -> String

instance (Listify str lst, ffs ~ ParseFmtStr_ lst, RFormat ffs ps) => RPrintf str ps where
    rprintf_ _ = ($ "") . rformat (Proxy @ffs)

-- | The typeclass supporting polyarity used by
-- 'GHC.TypeLits.Printf.printf'. It works in mostly the same way as
-- 'P.PrintfType' from "Text.Printf", and similar the same as
-- 'Data.Symbol.Examples.Printf.FormatF'.
--
-- Ideally, you will never have to run into this typeclass or have to deal
-- with it.  It will come up if you ask for the type of
-- 'GHC.TypeLits.Printf.printf', or sometimes if you give the wrong number
-- or type of arguments to it.
--
-- >>> :t printf @"You have %.2f dollars, %s"
-- FormatFun '[ Right ..., 'Left " dollars ", 'Right ...] fun => fun
--
-- Every item in the first argument of 'FormatFun' is a chunk of the
-- formatting string, split between format holes ('Right') and string
-- chunks ('Left').  You can successively "eliminate" them by providing
-- more arguments that implement each hole:
--
-- >>> :t printf @"You have %.2f dollars, %s" 3.62
-- FormatFun '[ Right ...] fun => fun
--
-- Until you you finally fill all the holes:
--
-- >>> :t printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- FormatFun '[] t => t
--
-- at which point you may use it as a 'String' or @'IO' ()@, in the same
-- way that "Text.Printf" works.  We also support using strict 'T.Text'
-- lazy 'TL.Text' as well.
--
-- So, while it's possible to reason with this using the types, it's
-- usually more difficult than with 'pprintf' and 'rprintf'.
--
-- This is why, instead of reasoning with this using its types, it's easier
-- to reason with it using the errors instead:
--
-- >>> printf @"You have %.2f dollars, %s"
-- -- ERROR: Call to printf missing argument fulfilling "%.2f"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
--
-- >>> printf @"You have %.2f dollars, %s" 3.62
-- -- ERROR: Call to printf missing argument fulfilling "%s"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
--
-- >>> printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
--
-- >>> printf @"You have %.2f dollars, %s" 3.62 "Luigi" 72
-- -- ERROR: An extra argument of type Integer was given to a call to printf
-- -- Either remove the argument, or rewrite the format string to include the
-- -- appropriate hole.
--
-- If you're having problems getting the error messages to give helpful
-- feedback, try using 'pHelp':
--
-- >>> pHelp $ printf @"You have %.2f dollars, %s" 3.62
-- -- ERROR: Call to printf missing argument fulfilling "%s"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
--
-- 'pHelp' can give the type system the nudge it needs to provide good
-- errors.
class FormatFun (ffs :: [Either Symbol FieldFormat]) fun where
    formatFun :: p ffs -> String -> fun

-- | A useful token for helping the type system give useful errors for
-- 'printf':
--
-- >>> printf @"You have ".2f" dollars, %s" 3.26 :: PHelp
-- -- ERROR: Call to printf missing argument fulfilling "%s"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
--
-- Usually things should work out on their own without needing this ... but
-- sometimes the type system could need a nudge.
--
-- See also 'pHelp'
newtype PHelp = PHelp {
    -- | A useful helper function for helping the type system give useful
    -- errors for 'printf':
    --
    -- >>> pHelp $ printf @"You have %.2f dollars, %s" 3.62
    -- -- ERROR: Call to printf missing argument fulfilling "%s"
    -- -- Either provide an argument or rewrite the format string to not expect
    -- -- one.
    --
    -- Usually things would work out on their own without needing this ...
    -- but sometimes the type system could need a nudge.
    pHelp :: String
  }

instance {-# INCOHERENT #-} (a ~ String) => FormatFun '[] a where
    formatFun _ = id
instance (a ~ Char) => FormatFun '[] PHelp where
    formatFun _ = PHelp
instance (a ~ Char) => FormatFun '[] T.Text where
    formatFun _ = T.pack
instance (a ~ Char) => FormatFun '[] TL.Text where
    formatFun _ = TL.pack
instance (a ~ ()) => FormatFun '[] (IO a) where
    formatFun _ = putStr

instance TypeError ( 'Text "Result type of a call to printf not sufficiently inferred."
               ':$$: 'Text "Please provide an explicit type annotation or other way to help inference."
                   )
      => FormatFun '[] () where
    formatFun _ = error

instance TypeError ( 'Text "An extra argument of type "
               ':<>: 'ShowType a
               ':<>: 'Text " was given to a call to printf."
               ':$$: 'Text "Either remove the argument, or rewrite the format string to include the appropriate hole"
                   )
      => FormatFun '[] (a -> b) where
    formatFun _ = error

instance (KnownSymbol str, FormatFun ffs fun) => FormatFun ('Left str ': ffs) fun where
    formatFun _ str = formatFun (Proxy @ffs) (str ++ symbolVal (Proxy @str))

instance {-# INCOHERENT #-} (afun ~ (arg -> fun), Reflect ff, ff ~ 'FF f w p m c, FormatType c arg, FormatFun ffs fun) => FormatFun ('Right ff ': ffs) afun where
    formatFun _ str x = formatFun (Proxy @ffs) (str ++ formatArg (Proxy @c) x ff "")
      where
        ff = reflect (Proxy @ff)

type family MissingError ff where
    MissingError ff = 'Text "Call to printf missing an argument fulfilling \"%"
                ':<>: 'Text (ShowFormat ff)
                ':<>: 'Text "\""
                ':$$: 'Text "Either provide an argument or rewrite the format string to not expect one."

instance TypeError (MissingError ff) => FormatFun ('Right ff ': ffs) String where
    formatFun _ = error
instance TypeError (MissingError ff) => FormatFun ('Right ff ': ffs) () where
    formatFun _ = error
instance TypeError (MissingError ff) => FormatFun ('Right ff ': ffs) T.Text where
    formatFun _ = error
instance TypeError (MissingError ff) => FormatFun ('Right ff ': ffs) TL.Text where
    formatFun _ = error
instance TypeError (MissingError ff) => FormatFun ('Right ff ': ffs) PHelp where
    formatFun _ = error
instance TypeError (MissingError ff) => FormatFun ('Right ff ': ffs) (IO a) where
    formatFun _ = error

class Printf (str :: Symbol) fun where
    -- | A version of 'GHC.TypeLits.Printf.printf' taking an explicit
    -- proxy, which allows usage without /TypeApplications/
    --
    -- >>> putStrLn $ printf_ (Proxy :: Proxy "You have %.2f dollars, %s") 3.62 "Luigi"
    -- You have 3.62 dollars, Luigi
    printf_ :: p str -> fun

instance (Listify str lst, ffs ~ ParseFmtStr_ lst, FormatFun ffs fun) => Printf str fun where
    printf_ _ = formatFun (Proxy @ffs) ""

-- | Utility type powering 'pfmt'.  See dcumentation for 'pfmt' for more
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
mkPFmt_
    :: forall str lst ff f w q m c p. (Listify str lst, ff ~ ParseFmt_ lst, Reflect ff, ff ~ 'FF f w q m c)
    => p str
    -> PFmt c
mkPFmt_ _ = PFmt ff
  where
    ff = reflect (Proxy @ff)

-- | Useful for using 'pfmt' without /OverloadedLabels/, or also when
-- passing format specifiers that aren't currently allowed with
-- /OverloadedLabels/ until GHC 8.10+ (like @#.2f@).
--
-- >>> pfmt (mkPFmt @".2f") 3.6234124
-- "3.62"
mkPFmt
    :: forall str lst ff f w q m c. (Listify str lst, ff ~ ParseFmt_ lst, Reflect ff, ff ~ 'FF f w q m c)
    => PFmt c
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
-- Note that the format string does not include the leading @%@.
pfmt :: forall c a. FormatType c a => PFmt c -> a -> String
pfmt (PFmt ff) x = formatArg (Proxy @c) x ff ""
