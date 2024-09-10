{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | A partial mock-up of the proposed Unsatisfiable constraint.
module GHC.TypeLits.Printf.Internal.Unsatisfiable (
  Unsatisfiable,
  unsatisfiable,
) where

import GHC.TypeLits

class Bottom where
  unsatisfiable :: a

class (Bottom, TypeError e) => Unsatisfiable (e :: ErrorMessage)
instance (Bottom, TypeError e) => Unsatisfiable (e :: ErrorMessage)
