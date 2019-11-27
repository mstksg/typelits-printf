{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module GHC.TypeLits.Printf where

import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Symbol.Utils
import           Data.Vinyl
import           Data.Vinyl.Curry
import           Data.Vinyl.Functor
import           Data.Word
import           GHC.Exts
import           GHC.TypeLits
import           GHC.TypeLits.Printf.Parse
import           Numeric.Natural
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Text.Printf               as P

-- data CType c = forall a. c a => C a

class FormatChar (t :: SChar) a where
    format :: p t -> a -> P.FieldFormat -> ShowS

    default format :: P.PrintfArg a => p t -> a -> P.FieldFormat -> ShowS
    format _ = P.formatArg

instance FormatChar "c" Char

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

class FormatF (ffs :: [Either Symbol FieldFormat]) fun | ffs -> fun where
    formatF :: p ffs -> String -> fun

instance FormatF '[] String where
    formatF _ = id

instance (KnownSymbol str, FormatF ffs fun) => FormatF ('Left str ': ffs) fun where
    formatF _ = formatF (Proxy @ffs)
                    . (<> symbolVal (Proxy @str))

instance (Reflect ff, ff ~ 'FF f w p m c, FormatF ffs fun) => FormatF ('Right ff ': ffs) (P c -> fun) where
    formatF _ str (P x) = formatF (Proxy @ffs)
                              $ str <> format (Proxy @c) x ff ""
        where
          ff = reflect (Proxy @ff)

class Printf (str :: Symbol) fun where
    printf_ :: p str -> fun

instance (Listify str lst, 'Just ffs ~ ParseFmtStr lst, FormatF ffs fun) => Printf str fun where
    printf_ _ = formatF (Proxy @ffs) ""

printf :: forall str fun. Printf str fun => fun
printf = printf_ (Proxy @str)

