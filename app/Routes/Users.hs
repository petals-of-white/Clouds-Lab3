{-# LANGUAGE OverloadedStrings #-}

module Routes.Users where
import           Colog
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.Text                 (pack)
import           Data.UUID                 as UUID
import           Network.HTTP.Types
import           Persistence
import           Types
import           Web.Scotty.Trans


allUsers :: (UserRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
allUsers =
    get "/users" $ do
        lift $ logInfo "getting all users"
        users <- lift getUsers
        json users

userById :: (UserRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
userById =
    get "/users/:userId" $ do
        strUserId <- captureParam "userId"
        let maybeUUID = UUID.fromString strUserId
        case maybeUUID of
            Just uuid -> do
                user <- lift (getUserById (UserID uuid))
                lift $ logInfo $ pack ("Found user " ++ strUserId)
                json user
            Nothing -> raiseStatus badRequest400 "Invalid UUID"

addUser :: (UserRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
addUser =
    post "/users" $ do
        newUser :: User <- jsonData
        newUserId  <- lift (createUser newUser)
        lift $ logInfo $ pack ("New user with id " ++ show newUserId ++ " created")
        json newUserId
        status created201
