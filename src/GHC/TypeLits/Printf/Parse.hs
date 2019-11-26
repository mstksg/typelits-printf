{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.TypeLits.Printf.Parse (
    ParseFmtStr
  , ParseFmtStr_
  , FormatAdjustment(..)
  , FormatSign(..)
  , WidthMod(..)
  , Flags(..)
  , FieldFormat(..)
  ) where

import           GHC.TypeLits.Printf.Internal.Parser
import           GHC.TypeLits

data FormatAdjustment = LeftAdjust | ZeroPad
data FormatSign       = SignPlus   | SignSpace

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


data FlagParser :: Parser Flags
type instance RunParser FlagParser str = 'Just (ProcessFlags ('Flags 'Nothing 'Nothing 'False) str)

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
