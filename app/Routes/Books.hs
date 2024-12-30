{-# LANGUAGE OverloadedStrings #-}
module Routes.Books where

import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Persistence
import           Routes.Common
import           Types
import           Web.Scotty.Trans
import Colog
import Data.Text (pack)

addBook :: (BookRepository m, MonadUnliftIO m,  WithLog env Message m) => ScottyT m ()
addBook =
    post "/books" $ do
        newBook :: Book GenreID AuthorID <- jsonData
        bookId <- lift (createBook newBook)
        lift $ logInfo $ pack ("Created a book with id " ++ show bookId)
        json bookId


searchBooks :: (BookRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
searchBooks =
    get "/books" $ do
        author :: Maybe ParsableUUID <- maybeQueryParam "author"
        genres :: Maybe [ParsableUUID] <- maybeQueryParam "genres"

        let authorID = AuthorID . unParsableUUID <$> author
            genreIDs =  fmap (fmap (GenreID . unParsableUUID)) genres
        books <- lift $ getBooks authorID genreIDs
        lift $ logInfo $ pack ("Found " ++ show (length books) ++ " books")
        json books

bookById :: (BookRepository m, MonadUnliftIO m, WithLog env Message m) => ScottyT m ()
bookById =
    get "/books/:bookId" $ do
        (ParsableUUID bookId) :: ParsableUUID <- captureParam "bookId"
        book <- lift $ getBookById (BookID bookId)
        json book

