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

module GHC.TypeLits.Printf (
  -- * Formattable things
    FormatChar(..)
  , SChar
  -- * Guarded polyarity
  , pprintf, pprintf_
  , PP(..)
  -- * List-based polyarity
  , Rec((:%), RNil), FormatArgs
  , rprintf, rprintf_
  -- * Unguarded polyarity
  , printf, printf_
  , FormatFun
  ) where

import           Data.Int
import           Data.Proxy
import           Data.Symbol.Utils
import           Data.Vinyl
import           Data.Vinyl.Curry
import           Data.Word
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
class FormatChar (t :: SChar) a where
    formatArg :: p t -> a -> P.FieldFormat -> ShowS

    default formatArg :: P.PrintfArg a => p t -> a -> P.FieldFormat -> ShowS
    formatArg _ = P.formatArg

instance FormatChar "c" Char
instance FormatChar "c" Word8
instance FormatChar "c" Word16

instance FormatChar "d" Char
instance FormatChar "d" Int
instance FormatChar "d" Int8
instance FormatChar "d" Int16
instance FormatChar "d" Int32
instance FormatChar "d" Int64
instance FormatChar "d" Integer
instance FormatChar "d" Natural
instance FormatChar "d" Word
instance FormatChar "d" Word8
instance FormatChar "d" Word16
instance FormatChar "d" Word32
instance FormatChar "d" Word64

instance FormatChar "o" Char
instance FormatChar "o" Int
instance FormatChar "o" Int8
instance FormatChar "o" Int16
instance FormatChar "o" Int32
instance FormatChar "o" Int64
instance FormatChar "o" Integer
instance FormatChar "o" Natural
instance FormatChar "o" Word
instance FormatChar "o" Word8
instance FormatChar "o" Word16
instance FormatChar "o" Word32
instance FormatChar "o" Word64

instance FormatChar "x" Int
instance FormatChar "x" Int8
instance FormatChar "x" Int16
instance FormatChar "x" Int32
instance FormatChar "x" Int64
instance FormatChar "x" Integer
instance FormatChar "x" Natural
instance FormatChar "x" Word
instance FormatChar "x" Word8
instance FormatChar "x" Word16
instance FormatChar "x" Word32
instance FormatChar "x" Word64

instance FormatChar "X" Char
instance FormatChar "X" Int
instance FormatChar "X" Int8
instance FormatChar "X" Int16
instance FormatChar "X" Int32
instance FormatChar "X" Int64
instance FormatChar "X" Integer
instance FormatChar "X" Natural
instance FormatChar "X" Word
instance FormatChar "X" Word8
instance FormatChar "X" Word16
instance FormatChar "X" Word32
instance FormatChar "X" Word64

instance FormatChar "b" Char
instance FormatChar "b" Int
instance FormatChar "b" Int8
instance FormatChar "b" Int16
instance FormatChar "b" Int32
instance FormatChar "b" Int64
instance FormatChar "b" Integer
instance FormatChar "b" Natural
instance FormatChar "b" Word
instance FormatChar "b" Word8
instance FormatChar "b" Word16
instance FormatChar "b" Word32
instance FormatChar "b" Word64

instance FormatChar "u" Char
instance FormatChar "u" Int
instance FormatChar "u" Int8
instance FormatChar "u" Int16
instance FormatChar "u" Int32
instance FormatChar "u" Int64
instance FormatChar "u" Integer
instance FormatChar "u" Natural
instance FormatChar "u" Word
instance FormatChar "u" Word8
instance FormatChar "u" Word16
instance FormatChar "u" Word32
instance FormatChar "u" Word64

instance FormatChar "f" Double
instance FormatChar "f" Float

instance FormatChar "F" Double
instance FormatChar "F" Float

instance FormatChar "g" Double
instance FormatChar "g" Float

instance FormatChar "G" Double
instance FormatChar "G" Float

instance FormatChar "e" Double
instance FormatChar "e" Float

instance FormatChar "E" Double
instance FormatChar "E" Float

instance FormatChar "s" String
instance FormatChar "s" T.Text where
    formatArg _ = P.formatArg . T.unpack
instance FormatChar "s" TL.Text where
    formatArg _ = P.formatArg . TL.unpack

-- | Required wrapper around inputs to 'pprintf' (guarded polyarity).  See
-- documentation for 'pprintf' for examples of usage.
--
-- You can "wrap" any value in 'PP' as long as it can be formatted as the
-- format type indicated.
--
-- For example, to make a @'PP' "f"@, you can use @'PP' 3.5@ or @'PP'
-- 94.2@, but not @'PP' (3 :: Int)@ or @'PP' "hello"@.  To make a value of
-- type @'PP' c@, you must wrap a value that can be formatted via @c@.
data PP (c :: SChar) = forall a. FormatChar c a => PP a

type FormatArgs = Rec PP

class RFormat (ffs :: [Either Symbol FieldFormat]) (ps :: [SChar]) | ffs -> ps where
    rformat :: p ffs -> FormatArgs ps -> ShowS

instance RFormat '[] '[] where
    rformat _ _ = id

instance (KnownSymbol str, RFormat ffs ps) => RFormat ('Left str ': ffs) ps where
    rformat _ r = showString (symbolVal (Proxy @str))
                . rformat (Proxy @ffs) r

instance (Reflect ff, ff ~ 'FF f w p m c, RFormat ffs ps) => RFormat ('Right ff ': ffs) (c ': ps) where
    rformat _ (x :% xs) = formatArg (Proxy @c) x ff
                        . rformat (Proxy @ffs) xs
      where
        ff = reflect (Proxy @ff)

class RPrintf (str :: Symbol) ps where
    rprintf_ :: p str -> FormatArgs ps -> String

instance (Listify str lst, 'Just ffs ~ ParseFmtStr lst, RFormat ffs ps) => RPrintf str ps where
    rprintf_ _ = ($ "") . rformat (Proxy @ffs)

-- | Type-safe printf with faked polyarity.  Pass in a "list" of arguments
-- (using ':%' and 'RNil'), instead of as multiple arguments.  Call it like
-- @'rprintf' @"you have %.02f dollars, %s"@.
--
-- >>> :t rprintf @"You have %.2f dollars, %s"
-- FormatArgs '["f", "s"] -> 'String'
--
-- This means that it is expecting something that can be printed with @f@
-- and something that can be printed with @s@.  We can provide a 'Double'
-- and a 'String':
--
-- >>> putStrLn $ 'rprintf' @"You have %.2f dollars, %s" (3.62 ':%' "Luigi" :% 'RNil')
-- You have 3.62 dollars, Luigi
--
-- See 'pprintf' for a version with true polyarity and good clear types,
-- but requires wrapping its arguments, and 'printf' for a version with
-- true polyarity but less clear types.
rprintf :: forall str ps. RPrintf str ps => FormatArgs ps -> String
rprintf = rprintf_ (Proxy @str)

pattern (:%) :: () => FormatChar c a => a -> FormatArgs cs -> FormatArgs (c ': cs)
pattern x :% xs = PP x :& xs
infixr 7 :%
{-# COMPLETE (:%) #-}

pprintf_ :: forall str ps p. (RPrintf str ps, RecordCurry ps) => p str -> CurriedF PP ps String
pprintf_ p = rcurry @ps (rprintf_ p)

-- | Type-safe printf with true guarded polyarity.  Call it like @'printf'
-- @"you have %.02f dollars, %s"@.
--
-- A call to printf on a valid string will /always/ give a well-defined
-- type for a function in return:
--
-- >>> :t pprintf @"You have %.2f dollars, %s"
-- 'PP' "f" -> 'PP' "s" -> 'String'
--
-- You can always query the type, and get a well-defined type back, which
-- you can utilize using typed holes or other type-guided development
-- techniques.
--
-- To give 'pprintf' its arguments, however, they must be wrapped in 'PP':
--
-- >>> putStrLn $ pprintf @"You have %.2f dollars, %s" (PP 3.62) (PP "Luigi")
-- You have 3.62 dollars, Luigi
--
-- See 'printf' for a polyariadic method that doesn't require 'PP' on its
-- inputs, but doesn't have as usable a type when queried before supplying
-- its arguments, and 'rprintf' for a fake-polyariadic method that doesn't
-- require 'PP', but requires arguments in a single list instead.
pprintf :: forall str ps. (RPrintf str ps, RecordCurry ps) => CurriedF PP ps String
pprintf = pprintf_ @str @ps (Proxy @str)

class FormatFun (ffs :: [Either Symbol FieldFormat]) fun where
    formatFun :: p ffs -> String -> fun

instance FormatFun '[] String where
    formatFun _ = id

instance (KnownSymbol str, FormatFun ffs fun) => FormatFun ('Left str ': ffs) fun where
    formatFun _ str = formatFun (Proxy @ffs) (str ++ symbolVal (Proxy @str))

instance (Reflect ff, ff ~ 'FF f w p m c, FormatChar c a, FormatFun ffs fun) => FormatFun ('Right ff ': ffs) (a -> fun) where
    formatFun _ str x = formatFun (Proxy @ffs) (str ++ formatArg (Proxy @c) x ff "")
      where
        ff = reflect (Proxy @ff)

class Printf (str :: Symbol) fun where
    printf_ :: p str -> fun

instance (Listify str lst, 'Just ffs ~ ParseFmtStr lst, FormatFun ffs fun) => Printf str fun where
    printf_ _ = formatFun (Proxy @ffs) ""

-- | Type-safe printf with true naked polyarity.  Call it like @'printf'
-- @"you have %.02f dollars, %s"@.
--
-- >>> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
--
-- If you what the type of this function unapplied (or with not enough
-- arguments), or try to use it with
-- typed holes or type-guided development, the type errors aren't going to
-- be pretty in most situations.  In addition, you always have to make sure
-- the result type can be inferred as 'String', which may require a type
-- annotation in some situations.
--
-- (Measures have been taken to make the error messages as helpful as
-- possible, but they're not going to be as pretty as for 'pprintf' or
-- 'rprintf')
--
-- However, if you use it properly with the right number of arguments,
-- everything should work and be type-safe: code with missing or badly
-- typed arguments will not compile.
--
-- See 'pprintf' for a version of this with much nicer types and type
-- errors, but requires wrapping arguments, and 'rprintf' for a version of
-- this with "fake" polyarity, taking a list as input instead.
printf :: forall str fun. Printf str fun => fun
printf = printf_ (Proxy @str)

