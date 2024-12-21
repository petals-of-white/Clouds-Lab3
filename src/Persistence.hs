{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Persistence where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.UUID       (UUID)
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)
import           Types


newtype UserID = UserID UUID deriving (Generic, Show)
newtype BookID = BookID UUID deriving (Generic, Show)
newtype GenreID = GenreID UUID deriving (Generic, Show)
newtype AuthorID = AuthorID UUID deriving (Generic, Show)

data UserRecord = UserRecord {id :: UserID, user :: User} deriving (Generic, Show)
data GenreRecord = GenreRecord {id :: GenreID, genre :: Genre} deriving (Generic, Show)
data BookRecord = BookRecord {id :: BookID, book :: Book GenreRecord AuthorRecord} deriving (Generic, Show)
data AuthorRecord = AuthorRecord {id :: AuthorID, author :: Author} deriving (Generic, Show)

data BookProgress = BookProgress {bookId :: BookID, pagesRead :: Natural} deriving (Generic, Show)

instance ToJSON UserID
instance ToJSON BookID
instance ToJSON GenreID
instance ToJSON AuthorID

instance FromJSON UserID
instance FromJSON BookID
instance FromJSON GenreID
instance FromJSON AuthorID
instance FromJSON BookProgress

instance ToJSON AuthorRecord
instance ToJSON GenreRecord
instance ToJSON BookRecord
instance ToJSON UserRecord

class Monad m => UserRepository m where
    getUserById :: UserID -> m (Maybe User)
    getUsers :: m [UserRecord]
    createUser :: User -> m UserID

class Monad m => GenreRepository m where
    getGenreById :: GenreID -> m (Maybe Genre)
    getGenres :: m [GenreRecord]
    createGenre :: Genre -> m GenreID

class Monad m => AuthorRepository m where
    getAuthorById :: AuthorID -> m (Maybe Author)
    getAuthors :: m [AuthorRecord]
    createAuthor :: Author -> m AuthorID

class Monad m => BookRepository m where
    getBookById :: BookID -> m (Maybe (Book GenreRecord AuthorRecord))
    getBooks :: Maybe AuthorID -> [GenreID] -> m [BookRecord]
    createBook :: Book GenreID AuthorID -> m BookID

class Monad m => UserBooksRepo m where
    getUserBooks :: UserID -> m (Maybe [BookRecord])
    setUserBookProgress :: UserID -> BookID -> Natural -> m (Maybe ())
