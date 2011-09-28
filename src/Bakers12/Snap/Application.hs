{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Bakers12.Snap.Application
  ( Application
  , applicationInitializer
  , devApplicationInitializer
  ) where

import           Snap.Extension
import           Snap.Extension.Heist.Impl
import           System.FilePath ((</>))


------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
type Application = SnapExtend ApplicationState


------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by the Snap
-- extensions we're using.  We're using Heist so we can easily render Heist
-- templates, and Timer simply to illustrate the config loading differences
-- between development and production modes.
data ApplicationState = ApplicationState
    { templateState :: HeistState Application
    }


------------------------------------------------------------------------------
instance HasHeistState Application ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }


------------------------------------------------------------------------------
-- | The 'Initializer' for ApplicationState. For more on 'Initializer's, see
-- the documentation from the snap package. Briefly, this is used to
-- generate the 'ApplicationState' needed for our application and will
-- automatically generate reload\/cleanup actions for us which we don't need
-- to worry about.
applicationInitializer :: FilePath -> Initializer ApplicationState
applicationInitializer resourceDir = do
        heist <- heistInitializer (resourceDir </> "templates") id
        return $ ApplicationState heist

devApplicationInitializer :: Initializer ApplicationState
devApplicationInitializer = applicationInitializer "./bakers12"

