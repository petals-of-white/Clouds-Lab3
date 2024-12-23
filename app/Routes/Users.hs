{-# LANGUAGE OverloadedStrings #-}

module Routes.Users where
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.UUID                 as UUID
import           Network.HTTP.Types
import           Persistence
import           Types
import           Web.Scotty.Trans


allUsers :: (UserRepository m, MonadUnliftIO m) => ScottyT m ()
allUsers =
    get "/users" $ do
        liftIO $ putStrLn "getting all users"
        users <- lift getUsers
        json users

userById :: (UserRepository m, MonadUnliftIO m) => ScottyT m ()
userById =
    get "/users/:userId" $ do
        strUserId <- captureParam "userId"
        let maybeUUID = UUID.fromString strUserId
        case maybeUUID of
            Just uuid -> do
                user <- lift (getUserById (UserID uuid))
                json user
            Nothing -> raiseStatus badRequest400 "Invalid UUID"

addUser :: (UserRepository m, MonadUnliftIO m) => ScottyT m ()
addUser =
    post "/users" $ do
        newUser :: User <- jsonData
        newUserId  <- lift (createUser newUser)
        json newUserId
        status created201
