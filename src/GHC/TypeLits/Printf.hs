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

-- |
-- Module      : GHC.TypeLits.Printf
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- An extensible and type-safe printf from parsing GHC TypeLits Symbol
-- literals, matching the semantics of 'P.printf' from "Text.Printf" in
-- /base/.  The difference is that the variants here will always fail to
-- compile if given arguments of the wrong type (or too many or too little
-- arguments).  Most of the variants also provide useful type feedback,
-- telling you the type of arguments it expects and how many when queried
-- with @:t@ or with typed holes.  See documentation in "Text.Printf" for
-- details on how this formats items of various types, and the differences
-- with C @printf(3)@.
--
-- Comparing their usage/calling conventions:
--
-- >>> putStrLn $ pprintf @"You have %.2f dollars, %s" (PP 3.62) (PP "Luigi")
-- You have 3.62 dollars, Luigi
-- >>> putStrLn $ rprintf @"You have %.2f dollars, %s" (3.62 :% "Luigi" :% RNil)
-- You have 3.62 dollars, Luigi
-- >>> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
--
-- Now comparing their types:
--
-- >>> :t pprintf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- PP "f" -> PP "s" -> String
-- >>> :t rprintf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- FormatArgs '["f", "s"] -> String
-- >>> :t printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- FormatFun '[ .... ] fun => fun
--
-- * For 'pprintf', it shows you need two arguments: A @'PP' "f"@ (which is
--   a value that supports being formatted by @f@) like @PP 3.62@, and
--   a @'PP' "s"@, like @PP "Luigi"@.
-- * 'rprintf' tells you you need a two-item hlist (from
--   "Data.Vinyl.Core"), where the first item implements @f@ and the second
--   item implements @s@: @3.62 ':%' "Luigi" :% 'RNil'@ will do.
-- * The type of 'printf' is much less informative.  It's possible to see
--   what you need from the @...@ in 'FormatFun'...but it's basically
--   a situation that works fine when it does, but can be tricky if you
--   mess up. The up-side is that it is the cleanest to call if you already
--   know what you need: you can just give the arguments plainly, like
--   @3.62@ and @"Luigi"@.
--
--   Debugging it might not be so fun, but at least all debugging is
--   compile-time: you won't be able to compile-it until it is correct, so that
--   means you won't be dealing with run-time errors.
--
-- The following table summarizes the features and drawbacks of each
-- method:
--
-- +-----------+------------------+--------------------+--------------------------+
-- | Method    | True Polyarity   | Naked Arguments    | Type feedback            |
-- +===========+==================+====================+==========================+
-- | 'pprintf' | Yes              | No (requires 'PP') | Yes                      |
-- +-----------+------------------+--------------------+--------------------------+
-- | 'rprintf' | No (HList-based) | Yes                | Yes                      |
-- +-----------+------------------+--------------------+--------------------------+
-- | 'printf'  | Yes              | Yes                | No (Bad errors/guidance) |
-- +-----------+------------------+--------------------+--------------------------+
--
-- /Ideally/ we would have a solution that has all three.  However, as of
-- now, we have a "pick two" sort of situation.  Suggestions are definitely
-- welcome, however, if you find something that satisfies all three
-- benefits while still allowing for polymorphism!
--
-- You can extend functionality with formatting for your own types by
-- providing instances of @FormatType@.
--
-- Also in this module is 'pfmt', which allows you to format individual
-- items according to a single format specifier.

module GHC.TypeLits.Printf (
  -- * Formattable things
    FormatType(..)
  , SChar
  -- * Printf
  -- ** Guarded polyarity
  , pprintf
  , pprintf_
  , PP(..)
  -- ** List-based polyarity
  , rprintf, rprintf_
  , Rec((:%), RNil), FormatArgs
  -- ** Unguarded polyarity
  , printf, printf_
  , FormatFun
  -- * Single item
  , pfmt
  , PFmt
  , mkPFmt, mkPFmt_
  ) where

import           Data.Proxy
import           Data.Vinyl
import           Data.Vinyl.Curry
import           GHC.TypeLits.Printf.Internal

-- | Type-safe printf with faked polyarity.  Pass in a "list" of arguments
-- (using ':%' and 'RNil'), instead of as multiple arguments.  Call it like
-- @'rprintf' \@"you have %.02f dollars, %s"@.
--
-- >>> :t rprintf @"You have %.2f dollars, %s"
-- FormatArgs '["f", "s"] -> String
--
-- This means that it is expecting something that can be printed with @f@
-- and something that can be printed with @s@.  We can provide a 'Double'
-- and a 'String':
--
-- >>> putStrLn $ rprintf @"You have %.2f dollars, %s" (3.62 ':%' "Luigi" :% 'RNil')
-- You have 3.62 dollars, Luigi
--
-- See 'pprintf' for a version with true polyarity and good clear types,
-- but requires wrapping its arguments, and 'printf' for a version with
-- true polyarity but less clear types.  Also see top-level module
-- documentation  "GHC.TypeLits.Printf" for a more comprehensive summary.
rprintf :: forall str ps. RPrintf str ps => FormatArgs ps -> String
rprintf = rprintf_ (Proxy @str)

-- | Pattern and constructor allowing you to construct a 'FormatArgs'.
--
-- To construct a @'FormatArgs' '["f", "s"]@, for instance, you need to
-- give a value formattable by @f@ and a value formattable by @s@, given
-- like a linked list, with ':%' for cons and 'RNil' for nil.
--
-- @
-- 3.62 ':%' "Luigi" :% 'RNil'
-- @
--
-- (This should evoke the idea of of @3.62 : "Luigi" : []@, even though the
-- latter is not possible in Haskell)
pattern (:%) :: () => FormatType c a => a -> FormatArgs cs -> FormatArgs (c ': cs)
pattern x :% xs = PP x :& xs
infixr 7 :%
{-# COMPLETE (:%) #-}

-- | A version of 'pprintf' taking an explicit proxy, which allows usage
-- without /TypeApplications/
--
-- >>> :t pprintf_ (Proxy :: Proxy "You have %.2f dollars, %s")
-- PP "f" -> PP "s" -> String
pprintf_ :: forall str ps p. (RPrintf str ps, RecordCurry ps) => p str -> CurriedF PP ps String
pprintf_ p = rcurry @ps (rprintf_ p)

-- | Type-safe printf with true guarded polyarity.  Call it like @'pprintf'
-- \@"you have %.02f dollars, %s"@.
--
-- A call to printf on a valid string will /always/ give a well-defined
-- type for a function in return:
--
-- >>> :t pprintf @"You have %.2f dollars, %s"
-- PP "f" -> PP "s" -> String
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
-- require 'PP', but requires arguments in a single list instead. Also see
-- top-level module documentation  "GHC.TypeLits.Printf" for a more
-- comprehensive summary.
pprintf :: forall str ps. (RPrintf str ps, RecordCurry ps) => CurriedF PP ps String
pprintf = pprintf_ @str @ps (Proxy @str)

-- | Type-safe printf with true naked polyarity.  Call it like @'printf'
-- \@"you have %.02f dollars, %s"@.
--
-- >>> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
--
-- If you what the type of this function unapplied (or with not enough
-- arguments), or try to use it with typed holes or type-guided
-- development, the type errors aren't going to be pretty in most
-- situations.  In addition, you always have to make sure the result type
-- can be inferred as 'String', which may require a type annotation in some
-- situations.
--
-- (Measures have been taken to make the error messages as helpful as
-- possible, but they're not going to be as pretty as for 'pprintf' or
-- 'rprintf'.  See 'FormatFun' documentation for ways to get some limited
-- type feedback from this function.)
--
-- Essentially, this is easier to use if you have all the right arguments
-- in place...but more difficult to debug (at compiletime) if you mess up.
--
-- Some guidelines for making sure the type-checking and debugging story
-- goes as nicely as possible:
--
-- * Make sure the result type is always known monomorphically.  Sometimes
--   this means requiring an explicit annotation, like @printf ... ::
--   String@.
-- * Make sure all the values you give to this have known monomorphic
--   types, as well.
--
-- /However/, all debugging should still be only at compile-time.  Once it
-- compiles, it's safe --- code with missing or badly typed arguments will
-- not compile, and so won't give you any runtime errors.
--
-- See 'pprintf' for a version of this with much nicer types and type
-- errors, but requires wrapping arguments, and 'rprintf' for a version of
-- this with "fake" polyarity, taking a list as input instead. Also see
-- top-level module documentation  "GHC.TypeLits.Printf" for a more
-- comprehensive summary.
printf :: forall str fun. Printf str fun => fun
printf = printf_ (Proxy @str)

