{-# LANGUAGE OverloadedStrings #-}

module Routes.Genres where

import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Network.HTTP.Types
import           Persistence
import           Routes.Common
import           Types
import           Web.Scotty.Trans
import Colog
import Data.Text (pack)

allGenres :: (GenreRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
allGenres =
    get "/genres" $ do
        lift $ logInfo "Getting all genres"
        genres <- lift getGenres
        json genres

addGenre :: (GenreRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
addGenre =
    post "/genres" $ do
        newGenre :: Genre <- jsonData
        genreId <- lift (createGenre newGenre)
        lift $ logInfo $ pack ("New genre with id " ++ show genreId ++ " created")
        json genreId
        status created201

genreById :: (GenreRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
genreById =
    get "/genres/:genreId" $ do
        (ParsableUUID uuid) <- captureParam "genreId"
        lift $ logInfo ("Getting books for user " <> pack (show uuid))
        maybeGenre <- lift (getGenreById (GenreID uuid))
        case maybeGenre of
            Just genre -> json genre
            Nothing    -> raiseStatus notFound404 "Genre not found"
