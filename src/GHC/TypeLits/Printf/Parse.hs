{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NoStarIsType           #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module GHC.TypeLits.Printf.Parse (
    ParseFmtStr
  , ParseFmtStr_
  , FormatAdjustment(..)
  , FormatSign(..)
  , WidthMod(..)
  , Flags(..)
  , EmptyFlags
  , FieldFormat(..)
  , SChar
  , Demote
  , Reflect(..)
  ) where

import           Data.Proxy
import           Data.Text                           (Text)
import           GHC.TypeLits hiding                 (natVal)
import           GHC.TypeLits.Printf.Internal.Parser
import           GHC.TypeNats
import           Numeric.Natural
import           Text.Printf                         (FormatAdjustment(..), FormatSign(..))
import qualified Data.Text                           as T
import qualified Text.Printf                         as P

data Flags = Flags
    { fAdjust    :: Maybe FormatAdjustment
    , fSign      :: Maybe FormatSign
    , fAlternate :: Bool
    }

data WidthMod = WMhh
              | WMh
              | WMl
              | WMll
              | WML

data FieldFormat = FF
    { fmtFlags     :: Flags
    , fmtWidth     :: Maybe Nat
    , fmtPrecision :: Maybe Nat
    , fmtWidthMod  :: Maybe WidthMod
    , fmtChar      :: SChar
    }

type family Demote k = a | a -> k
type instance Demote FormatAdjustment = FormatAdjustment
type instance Demote FormatSign       = FormatSign
type instance Demote Bool             = Bool
type instance Demote (Maybe a)        = Maybe (Demote a)
type instance Demote Nat              = Natural
type instance Demote Symbol           = Text
type instance Demote Flags            = Flags
type instance Demote WidthMod         = WidthMod
type instance Demote FieldFormat      = P.FieldFormat

class Reflect (x :: a) where
    reflect :: p x -> Demote a

instance Reflect 'LeftAdjust where
    reflect _ = LeftAdjust
instance Reflect 'ZeroPad where
    reflect _ = ZeroPad
instance Reflect 'SignPlus where
    reflect _ = SignPlus
instance Reflect 'SignSpace where
    reflect _ = SignSpace
instance Reflect 'WMhh where
    reflect _ = WMhh
instance Reflect 'WMh where
    reflect _ = WMh
instance Reflect 'WMl where
    reflect _ = WMl
instance Reflect 'WMll where
    reflect _ = WMll
instance Reflect 'WML where
    reflect _ = WML
instance Reflect 'False where
    reflect _ = False
instance Reflect 'True where
    reflect _ = True
instance Reflect 'Nothing where
    reflect _ = Nothing
instance Reflect x => Reflect ('Just x) where
    reflect _ = Just (reflect (Proxy @x))
instance KnownNat n => Reflect (n :: Nat) where
    reflect = natVal
instance KnownSymbol n => Reflect (n :: Symbol) where
    reflect = T.pack . symbolVal
instance (Reflect d, Reflect i, Reflect l) => Reflect ('Flags d i l) where
    reflect _ = Flags (reflect (Proxy @d))
                      (reflect (Proxy @i))
                      (reflect (Proxy @l))
instance (Reflect flags, Reflect width, Reflect prec, Reflect mods, Reflect chr)
      => Reflect ('FF flags width prec mods chr) where
    reflect _ = P.FieldFormat{..}
      where
        Flags{..}    = reflect (Proxy @flags)
        fmtWidth     = fromIntegral <$> reflect (Proxy @width)
        fmtPrecision = fromIntegral <$> reflect (Proxy @prec)
        fmtAdjust    = fAdjust
        fmtSign      = fSign
        fmtAlternate = fAlternate
        fmtModifiers = foldMap modString (reflect (Proxy @mods))
        fmtChar      = T.head (reflect (Proxy @chr))


modString :: WidthMod -> String
modString = \case
    WMhh -> "hh"
    WMh  -> "h"
    WMl  -> "l"
    WMll -> "ll"
    WML  -> "L"

data FlagParser :: Parser Flags
type instance RunParser FlagParser str = 'Just (ProcessFlags EmptyFlags str)

type EmptyFlags = 'Flags 'Nothing 'Nothing 'False

type family ProcessFlags (f :: Flags) (str :: [SChar]) :: (Flags, [SChar]) where
    ProcessFlags ('Flags d i l) ("-" ': cs) = '( 'Flags ('Just (UpdateAdjust d 'LeftAdjust)) i l, cs)
    ProcessFlags ('Flags d i l) ("0" ': cs) = '( 'Flags ('Just (UpdateAdjust d 'ZeroPad   )) i l, cs)
    ProcessFlags ('Flags d i l) ("+" ': cs) = '( 'Flags d ('Just (UpdateSign i 'SignPlus )) l, cs)
    ProcessFlags ('Flags d i l) (" " ': cs) = '( 'Flags d ('Just (UpdateSign i 'SignSpace)) l, cs)
    ProcessFlags ('Flags d i l) ("#" ': cs) = '( 'Flags d i 'True, cs)
    ProcessFlags f              cs          = '(f, cs)

type family UpdateAdjust d1 d2 where
    UpdateAdjust 'Nothing            d2 = d2
    UpdateAdjust ('Just 'LeftAdjust) d2 = 'LeftAdjust
    UpdateAdjust ('Just 'ZeroPad   ) d2 = d2

type family UpdateSign i1 i2 where
    UpdateSign 'Nothing           i2 = i2
    UpdateSign ('Just 'SignPlus ) i2 = 'SignPlus
    UpdateSign ('Just 'SignSpace) i2 = i2


type WMParser = (Sym "h" *> (('WMhh <$ Sym "h") <|> Pure 'WMh))
            <|> (Sym "l" *> (('WMll <$ Sym "l") <|> Pure 'WMl))
            <|> ('WML <$ Sym "L")

type FFParser = Sym "%"
            *> ('FF <$> FlagParser
                    <*> Optional Number
                    <*> Optional (Sym "." *> Number)
                    <*> Optional WMParser
                    <*> AnySym
               )

type FmtStrParser = Many ( ('Left  <$> Cat (Some (NotSym "%")))
                       <|> ('Right <$> FFParser               )
                         )

type ParseFmtStr  str = EvalParser  FmtStrParser str
type ParseFmtStr_ str = EvalParser_ FmtStrParser str
