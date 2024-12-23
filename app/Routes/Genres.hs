{-# LANGUAGE OverloadedStrings #-}

module Routes.Genres where

import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Network.HTTP.Types
import           Persistence
import           Routes.Common
import           Types
import           Web.Scotty.Trans

allGenres :: (GenreRepository m, MonadUnliftIO m) => ScottyT m ()
allGenres =
    get "/genres" $ do
        genres <- lift getGenres
        json genres

addGenre :: (GenreRepository m, MonadUnliftIO m) => ScottyT m ()
addGenre =
    post "/genres" $ do
        newGenre :: Genre <- jsonData
        genreId <- lift (createGenre newGenre)
        json genreId
        status created201

genreById :: (GenreRepository m, MonadUnliftIO m) => ScottyT m ()
genreById =
    get "/genres/:genreId" $ do
        (ParsableUUID uuid) <- captureParam "genreId"
        maybeGenre <- lift (getGenreById (GenreID uuid))
        case maybeGenre of
            Just genre -> json genre
            Nothing    -> raiseStatus notFound404 "Genre not found"
