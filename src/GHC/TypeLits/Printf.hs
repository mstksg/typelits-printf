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
    FormatChar(..)
  , P(..)
  , pprintf, pprintf_
  , Rec((:%), RNil)
  , rprintf, rprintf_
  , HFormat
  , hprintf, hprintf_
  , printf, printf_
  , FormatF
  ) where

import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Symbol.Utils
import           Data.Vinyl
import           Data.Vinyl.Curry
import           Data.Vinyl.Functor
import           Data.Word
import           GHC.TypeLits
import           GHC.TypeLits.Printf.Parse
import           Numeric.Natural
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Text.Printf               as P

class FormatChar (t :: SChar) a where
    format :: p t -> a -> P.FieldFormat -> ShowS

    default format :: P.PrintfArg a => p t -> a -> P.FieldFormat -> ShowS
    format _ = P.formatArg

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
    format _ = P.formatArg . T.unpack
instance FormatChar "s" TL.Text where
    format _ = P.formatArg . TL.unpack

data P (c :: SChar) = forall a. FormatChar c a => P a

class RFormat (ffs :: [Either Symbol FieldFormat]) (ps :: [SChar]) | ffs -> ps where
    rformat :: p ffs -> Rec P ps -> ShowS

instance RFormat '[] '[] where
    rformat _ _ = id

instance (KnownSymbol str, RFormat ffs ps) => RFormat ('Left str ': ffs) ps where
    rformat _ r = showString (symbolVal (Proxy @str))
                . rformat (Proxy @ffs) r

instance (Reflect ff, ff ~ 'FF f w p m c, RFormat ffs ps) => RFormat ('Right ff ': ffs) (c ': ps) where
    rformat _ (x :% xs) = format (Proxy @c) x ff
                        . rformat (Proxy @ffs) xs
      where
        ff = reflect (Proxy @ff)

class RPrintf (str :: Symbol) ps where
    rprintf_ :: p str -> Rec P ps -> String

instance (Listify str lst, 'Just ffs ~ ParseFmtStr lst, RFormat ffs ps) => RPrintf str ps where
    rprintf_ _ = ($ "") . rformat (Proxy @ffs)

rprintf :: forall str ps. RPrintf str ps => Rec P ps -> String
rprintf = rprintf_ (Proxy @str)

pattern (:%) :: () => FormatChar c a => a -> Rec P cs -> Rec P (c ': cs)
pattern x :% xs = P x :& xs
infixr 7 :%
{-# COMPLETE (:%) #-}

pprintf_ :: forall str ps p. (RPrintf str ps, RecordCurry ps) => p str -> CurriedF P ps String
pprintf_ p = rcurry @ps (rprintf_ p)

pprintf :: forall str ps. (RPrintf str ps, RecordCurry ps) => CurriedF P ps String
pprintf = pprintf_ @str @ps (Proxy @str)

class HFormat (ps :: [SChar]) (as :: [Type]) where
    hformat :: HList as -> Rec P ps

instance HFormat '[] '[] where
    hformat _ = RNil

instance (FormatChar c a, HFormat cs as) => HFormat (c ': cs) (a ': as) where
    hformat (Identity x :& xs) = P x :& hformat xs

hprintf_ :: forall str ps as p. (RPrintf str ps, HFormat ps as) => p str -> HList as -> String
hprintf_ p = rprintf_ p . hformat @ps

hprintf :: forall str ps as. (RPrintf str ps, HFormat ps as) => HList as -> String
hprintf = hprintf_ @str @ps (Proxy @str)

class FormatF (ffs :: [Either Symbol FieldFormat]) fun where
    formatF :: p ffs -> String -> fun

instance FormatF '[] String where
    formatF _ = id

instance (KnownSymbol str, FormatF ffs fun) => FormatF ('Left str ': ffs) fun where
    formatF _ str = formatF (Proxy @ffs) (str ++ symbolVal (Proxy @str))

instance (Reflect ff, ff ~ 'FF f w p m c, FormatChar c a, FormatF ffs fun) => FormatF ('Right ff ': ffs) (a -> fun) where
    formatF _ str x = formatF (Proxy @ffs) (str ++ format (Proxy @c) x ff "")
      where
        ff = reflect (Proxy @ff)

class Printf (str :: Symbol) fun where
    printf_ :: p str -> fun

instance (Listify str lst, 'Just ffs ~ ParseFmtStr lst, FormatF ffs fun) => Printf str fun where
    printf_ _ = formatF (Proxy @ffs) ""

printf :: forall str fun. Printf str fun => fun
printf = printf_ (Proxy @str)

