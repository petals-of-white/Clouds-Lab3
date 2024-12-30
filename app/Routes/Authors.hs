{-# LANGUAGE OverloadedStrings #-}
module Routes.Authors where

import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.UUID                 as UUID
import           Network.HTTP.Types
import           Persistence
import           Types
import           Web.Scotty.Trans
import Colog
import Data.Text (pack)

allAuthors :: (AuthorRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
allAuthors =
    get "/authors" $ do
        lift $ logInfo "Getting authors"
        authors <- lift getAuthors
        json authors


authorById :: (AuthorRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
authorById =
    get "/authors/:authorId" $ do
        strAuthorId <- captureParam "authorId"
        
        let maybeUUID = UUID.fromString strAuthorId
        case maybeUUID of
            Just uuid -> do
                perhapsAuthor <- lift (getAuthorById (AuthorID uuid))
                case perhapsAuthor of
                    Just author -> do 
                        lift $ logInfo ("Found author by id " <> pack strAuthorId)
                        json author
                    Nothing     -> raiseStatus notFound404 "Author not found"
            Nothing -> raiseStatus badRequest400 "Invalid author UUID"

addAuthor :: (AuthorRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
addAuthor =
    post "/authors" $ do
        newAuthor :: Author <- jsonData
        authorId <- lift (createAuthor newAuthor)
        lift $ logInfo ("New author created with ID " <> pack (show authorId))
        json authorId
        status created201