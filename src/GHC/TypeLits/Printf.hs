{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,20,0)
{-# LANGUAGE RequiredTypeArguments #-}
#endif

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
-- /base/.  The difference is that your 'printf's will always fail to
-- compile if given arguments of the wrong type (or too many or too little
-- arguments).  It also allows you to use types to help your development,
-- by telling you the type of arguments it expects and how many when
-- queried with @:t@ or with typed holes. See documentation in
-- "Text.Printf" for details on how this formats items of various types,
-- and the differences with C @printf(3)@.
--
-- See 'printf' for the main function, or 'printf'' if you are on GHC 9.10 and
-- later.
--
-- You can extend functionality with formatting for your own types by providing
-- instances of 'FormatType'.
--
-- Also in this module is 'pfmt', which allows you to format individual
-- items according to a single format specifier.
module GHC.TypeLits.Printf (
  -- * Printf
  printf,
  printf',
  printf_,
  PHelp,
  pHelp,
  FormatFun,

  -- * Formattable things
  FormatType (..),

  -- * Single item
  pfmt,
  PFmt,
  mkPFmt,
  mkPFmt_,
) where

import Data.Proxy
import GHC.TypeLits.Printf.Internal
import qualified Text.Printf as P

-- | "Type-safe printf". Call it like @'printf' "you have %.02f dollars, %s"@.
--
-- >>> putStrLn $ printf "You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
--
-- __NOTE__: This is only type-safe in GHC 9.10 and above. A compatible
-- version is provided in earlier versions that compiles (as long as you
-- provide a string literal). For a type-safe version before GHC 9.10, use
-- 'printf''.
--
-- Anyway, looking at its type:
--
-- >>> :t printf "You have %.2f dollars, %s"
-- (FormatType "f" arg1, FormatType "s" arg2)
--   => arg1 -> arg2 -> String
--
-- It tells you that the result is an @arg1 -> arg2 -> 'String'@: take two
-- arguments, and return a 'String'.  The first argument must be an instance of
-- @'FormatType' "f"@ (things that can be formatted by @%f@) and the second argument
-- must be an instance of @'FormatType' "s"@ (things that can be formatted by @%s@).
--
-- We can see this in action by progressively applying arguments:
--
-- >>> :t printf "You have %.2f dollars, %s" 3.62
-- FormatType "s" arg1 => arg1 -> String
-- >>> :t printf "You have %.2f dollars, %s" 3.62 "Luigi"
-- String
--
-- The type errors for forgetting to apply an argument (or applying too many
-- arguments) are pretty clear:
--
-- >>> putStrLn $ printf "You have %.2f dollars, %s"
-- -- ERROR: Call to printf missing argument fulfilling "%.2f"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
-- >>> putStrLn $ printf "You have %.2f dollars, %s" 3.62
-- -- ERROR: Call to printf missing argument fulfilling "%s"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
-- >>> putStrLn $ printf "You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
-- >>> putStrLn $ printf "You have %.2f dollars, %s" 3.62 "Luigi" 72
-- -- ERROR: An extra argument of type Integer was given to a call to printf
-- -- Either remove the argument, or rewrite the format string to include the
-- -- appropriate hole.
--
-- If you want to see some useful error messages for feedback, 'pHelp' can
-- be useful:
--
-- >>> pHelp $ printf "You have %.2f dollars, %s" 3.62
-- -- ERROR: Call to printf missing argument fulfilling "%s"
-- -- Either provide an argument or rewrite the format string to not expect
-- -- one.
--
-- Note that this also supports the "interpret as an IO action to print out
-- results" functionality that "Text.Printf" supports.  And, in GHC 9.10 and
-- higher, this also supports returning strict 'Data.Text.Text' and lazy
-- 'Data.Text.Lazy.Text' as well.
#if MIN_VERSION_base(4,20,0)
printf :: forall fun. forall str -> Printf str fun => fun
printf str = printf_ (Proxy @str)
#else
printf :: P.PrintfType r => String -> r
printf = P.printf
{-# WARNING printf "This only does type-safe printf GHC 9.10 or later, but will still compile (unsafely) in earlier GHC as long as you use a string literal. For a type-safe version before 9.10, use printf' instead." #-}
#endif

-- | A version of 'printf' that uses @-XTypeApplications@ syntax to provide
-- the type-level string. Available since GHC 9.2 and higher.
--
-- >>> putStrLn $ printf' @"You have %.2f dollars, %s" 3.62 "Luigi"
-- You have 3.62 dollars, Luigi
printf' :: forall str fun. Printf str fun => fun
printf' = printf_ (Proxy @str)
