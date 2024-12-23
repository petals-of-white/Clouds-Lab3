{-# LANGUAGE OverloadedStrings #-}

module Routes.UserBooks where
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.UUID                 as UUID
import           Network.HTTP.Types
import           Persistence
import           Routes.Common
import           Web.Scotty.Trans


usersBooks :: (UserBooksRepo m, MonadUnliftIO m) => ScottyT m ()
usersBooks =
    get "/users/:userId/books" $ do
        strUserId <- captureParam "userId"
        let maybeUUID = UUID.fromString strUserId
        case maybeUUID of
            Just uuid -> do
                maybeBooks <- lift (getUserBooks (UserID uuid))
                case maybeBooks of
                    Just books -> json books
                    Nothing    -> raiseStatus notFound404 "User not found"
            Nothing -> do
                raiseStatus badRequest400 "Invalid user UUID"

setUserBookProgress :: (UserBooksRepo m, MonadUnliftIO m) => ScottyT m ()
setUserBookProgress =
    post "/users/:userId/books" $ do
        (ParsableUUID userId) <- captureParam "userId"
        BookProgress {bookId, pagesRead} <- jsonData
        maybeBooks <- lift $ Persistence.setUserBookProgress (UserID userId) bookId pagesRead
        case maybeBooks of
            Just books -> json books
            Nothing    -> raiseStatus notFound404 "User with not found"
