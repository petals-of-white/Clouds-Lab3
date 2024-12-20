{-# LANGUAGE OverloadedStrings #-}

module Routes where
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

allAuthors :: (AuthorRepository m, MonadUnliftIO m) => ScottyT m ()
allAuthors =
    get "/authors" $ do
        authors <- lift getAuthors
        json authors


authorById :: (AuthorRepository m, MonadUnliftIO m) => ScottyT m ()
authorById =
    get "/authors/:authorId" $ do
        strAuthorId <- captureParam "authorId"
        let maybeUUID = UUID.fromString strAuthorId
        case maybeUUID of
            Just uuid -> do
                perhapsAuthor <- lift (getAuthorById (AuthorID uuid))
                case perhapsAuthor of
                    Just author -> json author
                    Nothing -> raiseStatus notFound404 "Author not found"
            Nothing -> raiseStatus badRequest400 "Invalid author UUID"

addAuthor :: (AuthorRepository m, MonadUnliftIO m) => ScottyT m ()
addAuthor =
    post "/authors" $ do
        newAuthor :: Author <- jsonData
        authorId <- lift (createAuthor newAuthor)
        json authorId
        status created201

addBook :: (BookRepository m, MonadUnliftIO m) => ScottyT m ()
addBook =
    post "/books" $ do
        newBook :: Book GenreID AuthorID <- jsonData
        bookId <- lift (createBook newBook)
        json bookId

-- searchBooks :: (BookRepository m, MonadUnliftIO m) => ScottyT m ()
-- searchBooks =
--     get "/books" $ do
--         author <- queryParam "author"
--         genres <- queryParam "genres"

        
        