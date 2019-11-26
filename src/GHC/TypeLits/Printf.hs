{-# LANGUAGE ConstraintKinds        #-}
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

-- module GHC.Typelits.Printf (
--   ) where

-- import           Data.SOP
import           Data.Kind
import           Data.Proxy
import           Data.Vinyl
import           Data.Vinyl.Curry
import           Data.Vinyl.Functor
import           GHC.Exts
import           GHC.TypeLits
import           GHC.TypeLits.Printf.Parse

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
--

-- data FieldFormat = FF
--     { fmtWidth     :: Maybe Nat
--     , fmtChar      :: Symbol
--     }

-- newtype Formatter a = Formatter { runFormatter :: a -> ShowS }

-- data CType c = forall a. c a => C a

-- class Format (ff :: FieldFormat) c | ff -> c where
--     format :: p ff -> Formatter (CType c)

-- instance Format ('FF w "d") Integral where
--     format _ = Formatter $ \case C x -> shows (fromIntegral x :: Integer)

-- class RunFormatters (ffs :: [FieldFormat]) fun | ffs -> fun where
--     runFormatters :: p ffs -> String -> fun

-- instance RunFormatters '[] String where
--     runFormatters _ = id

-- instance (Format ff c, RunFormatters ffs fun) => RunFormatters (ff : ffs) (CType c -> fun) where
--     runFormatters _ str x = runFormatters (Proxy @ffs)
--                           . runFormatter (format (Proxy @ff)) x
--                           $ str
