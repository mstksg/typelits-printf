{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

-- | A partial mock-up of the proposed Unsatisfiable constraint.
module GHC.TypeLits.Printf.Internal.Unsatisfiable
  ( Unsatisfiable
  , unsatisfiable
  ) where

import GHC.TypeLits

class Bottom where
  unsatisfiable :: a

class (Bottom, TypeError e) => Unsatisfiable (e :: ErrorMessage)
instance (Bottom, TypeError e) => Unsatisfiable (e :: ErrorMessage)
