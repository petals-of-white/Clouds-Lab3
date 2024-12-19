module Routes where

-- import Web.Scotty
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Persistence
import           Web.Scotty.Trans

allUsers :: (UserRepository m, MonadIO m) => ActionT m ()
allUsers = do
        users <- lift getUsers
        json users
