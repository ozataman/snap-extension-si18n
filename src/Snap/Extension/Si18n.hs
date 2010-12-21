{-|
 
-}

module Snap.Extension.Si18n
  ( MonadSi18n(..)
  ) where

import Snap.Types

import Text.Si18n
import Data.Text (Text)


------------------------------------------------------------------------------
-- | The 'MonadSi18n' class. 
class MonadSnap m => MonadSi18n m where

  translate :: Text -> Text -> TokenMap -> m Text



