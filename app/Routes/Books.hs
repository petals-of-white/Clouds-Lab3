{-# LANGUAGE OverloadedStrings #-}
module Routes.Books where

import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Persistence
import           Routes.Common
import           Types
import           Web.Scotty.Trans

addBook :: (BookRepository m, MonadUnliftIO m) => ScottyT m ()
addBook =
    post "/books" $ do
        newBook :: Book GenreID AuthorID <- jsonData
        bookId <- lift (createBook newBook)
        json bookId


searchBooks :: (BookRepository m, MonadUnliftIO m) => ScottyT m ()
searchBooks =
    get "/books" $ do
        author :: Maybe ParsableUUID <- maybeQueryParam "author"
        genres :: Maybe [ParsableUUID] <- maybeQueryParam "genres"

        let authorID = AuthorID . unParsableUUID <$> author
            genreIDs =  fmap (fmap (GenreID . unParsableUUID)) genres

        books <- lift $ getBooks authorID genreIDs
        json books

bookById :: (BookRepository m, MonadUnliftIO m) => ScottyT m ()
bookById =
    get "/books/:bookId" $ do
        (ParsableUUID bookId) :: ParsableUUID <- captureParam "bookId"
        book <- lift $ getBookById (BookID bookId)
        json book

