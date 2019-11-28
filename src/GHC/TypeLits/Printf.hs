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
-- There are three main calling conventions supported:
--
-- >>> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
-- >>> putStrLn $ pprintf @"You have %.2f dollars, %s" (PP 3.62) (PP "Luigi")
-- You have 3.62 dollars, Luigi
-- >>> putStrLn $ rprintf @"You have %.2f dollars, %s" (3.62 :% "Luigi" :% RNil)
-- You have 3.62 dollars, Luigi
--
-- Now comparing their types:
--
-- >>> :t printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- FormatFun '[ .... ] fun => fun
-- >>> :t pprintf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- PP "f" -> PP "s" -> String
-- >>> :t rprintf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- FormatArgs '["f", "s"] -> String
--
-- * The type of `printf` doesn't tell you immediately what you
--   you need.  However, if you do try to use it, the type errors will guide you
--   along the way, iteratively.
--
--   >>> printf @"You have %.2f dollars, %s"
--   -- ERROR: Call to printf missing argument fulfilling "%.2f"
--   -- Either provide an argument or rewrite the format string to not expect
--   -- one.
--
--   >>> printf @"You have %.2f dollars, %s" 3.62
--   -- ERROR: Call to printf missing argument fulfilling "%s"
--   -- Either provide an argument or rewrite the format string to not expect
--   -- one.
--
--   >>> printf @"You have %.2f dollars, %s" 3.62 "Luigi"
--   You have 3.62 dollars, Luigi
--
--   >>> printf @"You have %.2f dollars, %s" 3.62 "Luigi" 72
--   -- ERROR: An extra argument of type Integer was given to a call to printf
--   -- Either remove the argument, or rewrite the format string to include the
--   -- appropriate hole.
-- * For 'pprintf', it shows you need two arguments: A @'PP' "f"@ (which is
--   a value that supports being formatted by @f@) like @PP 3.62@, and
--   a @'PP' "s"@, like @PP "Luigi"@.
-- * 'rprintf' tells you you need a two-item hlist (from
--   "Data.Vinyl.Core"), where the first item implements @f@ and the second
--   item implements @s@: @3.62 ':%' "Luigi" :% 'RNil'@ will do.
--
-- The following table summarizes the features and drawbacks of each
-- method:
--
-- +-----------+------------------+--------------------+----------------------+
-- | Method    | True Polyarity   | Naked Arguments    | Type feedback        |
-- +===========+==================+====================+======================+
-- | 'printf'  | Yes              | Yes                | Partial (via errors) |
-- +-----------+------------------+--------------------+----------------------+
-- | 'pprintf' | Yes              | No (requires 'PP') | Yes                  |
-- +-----------+------------------+--------------------+----------------------+
-- | 'rprintf' | No (HList-based) | Yes                | Yes                  |
-- +-----------+------------------+--------------------+----------------------+
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
  -- ** Unguarded polyarity
  , printf, printf_
  , PHelp, pHelp
  , FormatFun
  -- ** Guarded polyarity
  , pprintf
  , pprintf_
  , PP(..)
  -- ** List-based polyarity
  , rprintf, rprintf_
  , Rec((:%), RNil), FormatArgs
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
-- inputs, but with a less helpful type signature, and 'rprintf' for
-- a fake-polyariadic method that doesn't require 'PP', but requires
-- arguments in a single list instead. Also see top-level module
-- documentation  "GHC.TypeLits.Printf" for a more comprehensive summary.
pprintf :: forall str ps. (RPrintf str ps, RecordCurry ps) => CurriedF PP ps String
pprintf = pprintf_ @str @ps (Proxy @str)

-- | Type-safe printf with true naked polyarity.  Call it like @'printf'
-- \@"you have %.02f dollars, %s"@.
--
-- >>> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
--
-- While the type of @'printf' \@"my fmt string"@ isn't going to be very
-- helpful, the error messages should help guide you along the way:
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
--
-- See 'pprintf' for a version of this with nicer types and type errors,
-- but requires wrapping arguments, and 'rprintf' for a version of this
-- with "fake" polyarity, taking a list as input instead. Also see
-- top-level module documentation  "GHC.TypeLits.Printf" for a more
-- comprehensive summary.
--
-- Note that this also supports the "interpret as an IO action to print out
-- results" functionality that "Text.Printf" supports.  This also supports
-- returning strict 'Data.Text.Text' and lazy 'Data.Text.Lazy.Text' as
-- well.
printf :: forall str fun. Printf str fun => fun
printf = printf_ (Proxy @str)

