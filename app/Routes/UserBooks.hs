{-# LANGUAGE OverloadedStrings #-}

module Routes.UserBooks where
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.UUID                 as UUID
import           Network.HTTP.Types
import           Persistence
import           Routes.Common
import           Web.Scotty.Trans
import Colog
import Data.Text (pack)


usersBooks :: (UserBooksRepo m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
usersBooks =
    get "/users/:userId/books" $ do
        liftIO $ putStrLn "getting user books"
        strUserId <- captureParam "userId"
        lift $ logInfo ("Getting books for user " <> pack strUserId)
        let maybeUUID = UUID.fromString strUserId
        case maybeUUID of
            Just uuid -> do
                books <- lift (getUserBooks (UserID uuid))
                json books
                -- maybeBooks <- lift (getUserBooks (UserID uuid))
                -- case maybeBooks of
                --     Just books -> json books
                --     Nothing    -> raiseStatus notFound404 "User not found"
            Nothing -> do
                raiseStatus badRequest400 "Invalid user UUID"

setUserBookProgress :: (UserBooksRepo m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
setUserBookProgress =
    post "/users/:userId/books" $ do
        (ParsableUUID userId) <- captureParam "userId"
        BookProgress {bookId, pagesRead} <- jsonData
        lift $ logInfo $ pack ("Setting book " ++ show bookId ++ "(" ++ show pagesRead ++ ") for user " ++ show userId)
        lift $ Persistence.setUserBookProgress (UserID userId) bookId pagesRead
        status created201
        
