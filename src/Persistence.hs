{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Persistence where

import           Data.UUID       (UUID)
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)
import           Types


newtype UserID = UserID UUID deriving Generic
newtype BookID = BookID UUID deriving Generic
newtype GenreID = GenreID UUID deriving Generic
newtype AuthorID = AuthorID UUID deriving Generic

data UserRecord = UserRecord {id :: UserID, user :: User} deriving Generic
data GenreRecord = GenreRecord {id :: GenreID, genre :: Genre} deriving Generic
data BookRecord = BookRecord {id :: BookID, book :: Book GenreRecord AuthorRecord} deriving Generic
data AuthorRecord = AuthorRecord {id :: AuthorID, author :: Author} deriving Generic

class Monad m => UserRepository m where
    getUserById :: UserID -> m (Maybe User)
    getUsers :: m [User]
    createUser :: User -> m UserID

class Monad m => GenreRepository m where
    getGenres :: m [GenreRecord]
    createGenre :: Genre -> m GenreID

class Monad m => AuthorRepository m where
    getAuthorById :: AuthorID -> m (Maybe Author)
    getAuthors :: m [Author]

class Monad m => BookRepository m where
    getBookById :: BookID -> m (Maybe (Book GenreRecord AuthorRecord))
    getBooks :: AuthorID -> [GenreID] -> m [BookRecord]

class Monad m => UserBooksRepo m where
    getUserBooks :: UserID -> m (Maybe [BookRecord])
    setUserBookProgress :: UserID -> BookID -> Natural -> m (Maybe ())
