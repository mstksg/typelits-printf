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

module GHC.TypeLits.Printf.Internal (
    FormatChar(..)
  , PP(..)
  , RPrintf(..)
  , FormatArgs
  , RFormat(..)
  , Printf(..)
  , FormatFun(..)
  ) where

import           Data.Int
import           Data.Proxy
import           Data.Symbol.Utils
import           Data.Vinyl
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

-- | Treats as @c@
instance FormatChar "v" Char
-- | Treats as @d@
instance FormatChar "v" Int
-- | Treats as @d@
instance FormatChar "v" Int8
-- | Treats as @d@
instance FormatChar "v" Int16
-- | Treats as @d@
instance FormatChar "v" Int32
-- | Treats as @d@
instance FormatChar "v" Int64
-- | Treats as @d@
instance FormatChar "v" Integer
-- | Treats as @u@
instance FormatChar "v" Natural
-- | Treats as @u@
instance FormatChar "v" Word
-- | Treats as @u@
instance FormatChar "v" Word8
-- | Treats as @u@
instance FormatChar "v" Word16
-- | Treats as @u@
instance FormatChar "v" Word32
-- | Treats as @u@
instance FormatChar "v" Word64
-- | Treats as @g@
instance FormatChar "v" Double
-- | Treats as @g@
instance FormatChar "v" Float
-- | Treats as @s@
instance FormatChar "v" String
-- | Treats as @s@
instance FormatChar "v" T.Text where
    formatArg _ = P.formatArg . T.unpack
-- | Treats as @s@
instance FormatChar "v" TL.Text where
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
data PP (c :: SChar) = forall a. FormatChar c a => PP a

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

instance (Listify str lst, 'Just ffs ~ ParseFmtStr lst, RFormat ffs ps) => RPrintf str ps where
    rprintf_ _ = ($ "") . rformat (Proxy @ffs)

-- | The typeclass supporting polyarity used by
-- 'GHC.TypeLits.Printf.printf'. It works in mostly the same way as
-- 'P.PrintfType' from "Text.Printf", and similar the same as
-- 'Data.Symbol.Examples.Printf.FormatF'.
--
-- Ideally, you will never have to run into this typeclass or have to deal
-- with it.  It will come up if you ask for the type of
-- 'GHC.TypeLits.Printf.printf', or if you give the wrong number or type of
-- arguments to it.
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
-- at which point you may use it as a 'String' or @'IO' 'String'@, in the
-- same way that "Text.Printf" works.
--
-- Note that while it is somewhat possible to "reason with" or get type
-- feedback from this (as shown above), it is nowhere near the level of
-- clarity as 'GHC.TypeLits.Printf.pprintf' or
-- 'GHC.TypeLits.Printf.rprintf'.  And, if things go wrong (like passing
-- too many or too little arguments, or arguments of the wrong type), the
-- error messages are not going to be as useful.  However, there are some
-- measures put into place to make type errors more feasible for debugging,
-- and I'm open for suggestions for improvement methods, as well!
--
-- Some guidelines for making sure the type-checking and debugging story
-- goes as nicely as possible:
--
-- * Make sure the result type is always known monomorphically.  Sometimes
--   this means requiring an explicit annotation, like @printf ... ::
--   String@.
-- * Make sure all the values you give to this have known monomorphic
--   types, as well.
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
    -- | A version of 'GHC.TypeLits.Printf.printf' taking an explicit
    -- proxy, which allows usage without /TypeApplications/
    --
    -- >>> putStrLn $ printf_ (Proxy :: Proxy "You have %.2f dollars, %s") 3.62 "Luigi"
    -- You have 3.62 dollars, Luigi
    printf_ :: p str -> fun

instance (Listify str lst, 'Just ffs ~ ParseFmtStr lst, FormatFun ffs fun) => Printf str fun where
    printf_ _ = formatFun (Proxy @ffs) ""
