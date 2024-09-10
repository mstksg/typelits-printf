{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Typelits.Printf where

-- module GHC.Typelits.Printf (
--   ) where

-- import           Data.SOP
import Data.Kind
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Curry
import Data.Vinyl.Functor
import GHC.Exts
import GHC.TypeLits

-- so it looks like there are basically three ways of going about this:
--
--

-- | Method | Polymorphic | Type-Determined | Naked types
-- |--------+-------------+-----------------+-------------
-- | A      | x           | x               |
-- | B      |             | x               | x
-- | C      | x           |                 | x
--
-- Ideally we would want all three, but I can't think of a way.  I can only
-- get two out of three.
--
-- The only way maybe would be some sort of "limited" fundep, where ffs
-- determines the *length* or number of arguments of a list but not its
-- contents.
--
-- Of these, I really do like to prioritize polymorphism for usability
-- (using %d for Word), so that leaves A and C: type determined, or naked
-- types.
--
-- Type-determined would guide "type-driven" or "whole-driven" development,
-- and type errors would be a lot nicer.  Naked types would still be type
-- safe! But type errors a little worse.  But, we can maybe make this
-- better by using the custom type error system.
--
-- Ah, there is yet another way: Non-Naked HList, which asks you to feed
-- the items in an HList.  This can be:
--
-- | Method | Polymorphic | Type-Determined | Naked inputs | True polyarity
-- |--------+-------------+-----------------+--------------+----------------
-- | A      | x           | x               |              | x
-- | B      |             | x               | x            | x
-- | C      | x           |                 | x            | x
-- | D      | x           | x               | x            |
--
-- Essentially we hide the non-nakedness by requiring everyone to pass
-- a single "hlist" of items.  This one might actually be better for
-- usability too maybe.  idk.
--
-- So if the main priority is polymorphic, we essentially have three
-- methods now: A (non-naked), C (undetermined), D (not true polyarity).
--
-- Maybe one module with each is better.
data FieldFormat = FF
  { fmtWidth :: Maybe Nat
  , fmtChar :: Symbol
  }

newtype Formatter a = Formatter {runFormatter :: a -> ShowS}

data CType c = forall a. c a => C a

class Format (ff :: FieldFormat) c | ff -> c where
  format :: p ff -> Formatter (CType c)

instance Format ('FF w "d") Integral where
  format _ = Formatter $ \case C x -> shows (fromIntegral x :: Integer)

class RunFormatters (ffs :: [FieldFormat]) fun | ffs -> fun where
  runFormatters :: p ffs -> String -> fun

instance RunFormatters '[] String where
  runFormatters _ = id

instance (Format ff c, RunFormatters ffs fun) => RunFormatters (ff : ffs) (CType c -> fun) where
  runFormatters _ str x =
    runFormatters (Proxy @ffs)
      . runFormatter (format (Proxy @ff)) x
      $ str

class RunFormatters' (ffs :: [FieldFormat]) fun where
  runFormatters' :: p ffs -> String -> fun

--  Maybe this can be generalized to some sort of final context situation
instance IsString a => RunFormatters' '[] a where
  runFormatters' _ = fromString

-- instance TypeError ('Text "runFormatters' should ultimately return a String, but type inference inferred the wrong thing.  Please use a manual type annotation.")
--       => RunFormatters' ffs () where
--     runFormatters' _ = undefined

instance
  TypeError ('Text "runFormatters' received an extra argument: unexpected " ':<>: 'ShowType a) =>
  RunFormatters' '[] (a -> b)
  where
  runFormatters' _ = undefined

instance
  ( Format ff c
  , TypeError ('Text "Missing an argument: expecting some instance of " ':<>: 'ShowType c)
  ) =>
  RunFormatters' (ff ': ffs) String
  where
  runFormatters' _ = undefined

instance (Format ff c, c a, RunFormatters' ffs fun) => RunFormatters' (ff : ffs) (a -> fun) where
  runFormatters' _ str x =
    runFormatters' (Proxy @ffs)
      . runFormatter (format (Proxy @ff)) (C x)
      $ str

-- rf :: RunFormatters ffs fun => p ffs -> String -> fun
-- rf = _

-- class DeC fun1 fun2 where
--     deC :: fun1 -> fun2

-- instance DeC String String where
--     deC = id

-- instance (c a, DeC r s) => DeC (CType c -> r) (a -> s) where
--     deC = (deC .) . reC

-- instance (Format c a, RunFormatters cs fun) => RunFormatters (c ': cs) (a -> fun) where
--     runFormatters _ str x = runFormatters (Proxy @cs)
--                           . runFormatter (format (Proxy @c)) x
--                           $ str

-- instance (Integral a, Show a) => Format ('FF w "d") a where
--     format _ = Formatter shows

-- type family ToFormatters (cs :: [FieldFormat]) (as :: [Type]) :: Constraint where
--     ToFormatters '[] '[] = () :: Constraint
--     ToFormatters (c ': cs) (a ': as) = (Format c a, ToFormatters cs as)

-- toFormatters :: ToFormatters cs as => Rec Proxy cs -> Rec Formatter as
-- toFormatters = \case
--     RNil -> RNil

-- class ToFormatters (cs :: [FieldFormat]) (as :: [Type]) where
--     toFormatters :: Rec Proxy cs -> Rec Formatter as

-- instance ToFormatters '[] '[] where
--     toFormatters = \case RNil -> RNil

-- instance (Format c a, ToFormatters cs as) => ToFormatters (c ': cs) (a ': as) where
--     toFormatters = \case p :& ps -> format p :& toFormatters ps

-- formatters :: Rec Formatter as -> HList as -> ShowS
-- formatters = \case
--     RNil -> \case
--       RNil -> id
--     f :& fs -> \case
--       Identity x :& xs -> runFormatter f x . formatters fs xs

-- runFormatters :: Rec Formatter as -> HList as -> String
-- runFormatters fs = ($ "") . formatters fs

-- runSpec :: ToFormatters cs as => Rec Proxy cs -> HList as -> String
-- runSpec = runFormatters . toFormatters

-- runSpecC :: ToFormatters cs as => Rec Proxy cs -> Curried as String
-- runSpecC ps = rcurry' (runSpec ps)

-- class RunFormatters (cs :: [FieldFormat]) fun where
--     runFormatters :: p cs -> String -> fun

-- instance RunFormatters '[] String where
--     runFormatters _ = id

-- instance (Format c a, RunFormatters cs fun) => RunFormatters (c ': cs) (a -> fun) where
--     runFormatters _ str x = runFormatters (Proxy @cs)
--                           . runFormatter (format (Proxy @c)) x
--                           $ str

reC :: (CType c -> b) -> (forall a. c a => a -> b)
reC f x = f (C x)

-- asInt :: CType ((~) Int) -> Int
-- asInt (CType x) = x

-- disadvantage: we can't get :t runFormatters to be nice for some given
-- input.  but it will still fail to typecheck if it doesn't work.
--
-- if we use the fundep then we can get the type to look nice. but
-- unfortunately that means we can only use Int for '%d', etc.
--
-- maybe we can have a typeclass for re-mapping values?

-- toFormatters :: Rec Proxy cs -> Rec Formatter as
-- toFormatters = _

-- data Formatter a = Formatter
--     { runFormatter :: a -> ShowS
--     }

-- data Formatters :: [Type] -> Type where
--     FNil  :: Formatters '[]
--     FStr  :: ShowS -> Formatters as -> Formatters as
--     FForm :: Formatter a -> Formatters as -> Formatters (a ': as)

-- class Parser cs

-- class ToFormatters (cs :: [FieldFormat]) (as :: [Type]) where
--     toFormatters :: p cs -> Formatters as

-- instance ToFormatters '[] '[] where
--     toFormatters _ = FNil

-- runFormatters
--     :: Formatters

-- data FieldFormat = FF
--     { fmtWidth     :: Maybe Nat
--     , fmtChar      :: Symbol
--     , fmtGoal      :: Type
--     }

-- class Formattable (c :: FieldFormat) where
--     toFormat :: p ('FF w q a) -> Formatter a

-- instance (Integral a, Show a) => Formattable ('FF w "d" a) where
--     toFormat _ = Formatter shows

-- class FormatF (format :: [FieldFormat]) fun | format -> fun where
--     formatF :: p format -> String -> fun

-- instance FormatF '[] String where
--     formatF _ = id

-- instance (Formattable ('FF w q a), FormatF cs fun) => FormatF (('FF w q a) ': cs) (a -> fun) where
--     formatF _ str x = formatF (Proxy @cs)
--                     . runFormatter (toFormat (Proxy @('FF w q a))) x
--                     $ str

-- instance (Integral a, Show a) => Formattable "d" a where
--     toFormat = Formatter show

-- runFormatter ::

-- class Format
