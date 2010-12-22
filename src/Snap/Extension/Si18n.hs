{-|
 
-}

module Snap.Extension.Si18n
  ( MonadSi18n(..)
  ) where

import Snap.Types

import Text.Si18n
import Data.ByteString (ByteString)
import Data.Text (Text)


------------------------------------------------------------------------------
-- | The 'MonadSi18n' class. Minimum complete definition: 'translate'
class MonadSnap m => MonadSi18n m where

  ----------------------------------------------------------------------------
  -- | Translate given key using the i18n data buried in app's state
  translate :: Text         -- ^ Locale
            -> Text         -- ^ Key
            -> TokenMap     -- ^ Substitution tokens
            -> m Text


  ----------------------------------------------------------------------------
  -- | Same as 'translate' but instead uses ByteString (UTF8 encoded) I/O.
  translate' :: ByteString                    -- ^ Locale
             -> ByteString                    -- ^ Key
             -> [(ByteString, ByteString)]    -- ^ Substitution tokens
             -> m ByteString



