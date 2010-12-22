module Snap.Extension.Si18n.Impl
  ( MonadSi18n(..)
  , Si18nState(..)
  , HasSi18nState(..)
  , si18nStateInitializer
  ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans
import qualified Data.Map as Map
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           System.Directory
import           System.FilePath.Posix

import           Snap.Extension
import           Snap.Types

import           Snap.Extension.Si18n

import qualified Text.Si18n as S
import           Text.Si18n (I18nData)


------------------------------------------------------------------------------
-- | 
data Si18nState = Si18nState
  { si18nData :: I18nData 
  , si18nDefLocale :: ByteString 
  }


------------------------------------------------------------------------------
-- |
class HasSi18nState s where
  getSi18nState :: s -> Si18nState


------------------------------------------------------------------------------
si18nStateInitializer :: FilePath 
                      -- ^ Path to directory containing locale files.
                      -> ByteString
                      -- ^ Default locale
                      -> Initializer Si18nState
si18nStateInitializer fp l = do
  fs <- liftIO $ getDirectoryContents fp
  let fs' = filter (`notElem` [".", ".."]) fs
  i18n <- foldM step Map.empty fs'
  let st = Si18nState i18n l
  mkInitializer st
  where
    step a x = do
      d <- liftIO $ S.loadI18nFile (fp </> x)
      return $ a `Map.union` d


------------------------------------------------------------------------------
-- | Register Si18nState as an Extension.
instance InitializerState Si18nState where
  extensionId = const "Si18n/Impl"
  mkCleanup = const $ return ()
  mkReload = const $ return ()


------------------------------------------------------------------------------
-- |
instance HasSi18nState s => MonadSi18n (SnapExtend s) where

  translate l s m = do
    i18n <- fmap si18nData $ asks getSi18nState
    return $ S.t i18n l s m

  translate' l s m = liftM encodeUtf8 $ translate l' s' m'
    where
      (l':s':_) = map decodeUtf8 [l, s]
      m' = map (\(k,v) -> (decodeUtf8 k, decodeUtf8 v)) m
    

