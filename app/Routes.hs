{-# LANGUAGE OverloadedStrings #-}

module Routes where
import           Control.Exception         (SomeException (SomeException))
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.Text.Lazy            (Text, unpack)
import           Data.UUID                 as UUID
import           Network.HTTP.Types
import           Persistence
import           Types
import           Web.Scotty.Trans


newtype ParsableUUID = ParsableUUID {unParsableUUID :: UUID}

instance Parsable ParsableUUID where
    parseParam textRepr =
        case UUID.fromString (unpack textRepr) of
            Just uuid -> Right (ParsableUUID uuid)
            Nothing   -> Left $ "Cannot represent " <> textRepr <> " as UUID"

maybeQueryParam :: (Parsable a, MonadUnliftIO m) => Text -> ActionT m (Maybe a)
maybeQueryParam p = rescue (Just <$> queryParam p) (\(SomeException _) -> return Nothing)


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

setUserBookProgress :: (UserBooksRepo m, MonadUnliftIO m) => ScottyT m ()
setUserBookProgress = 
    post "/users/:userId/books" $ do
        (ParsableUUID userId) <- captureParam "userId"
        BookProgress {bookId, pagesRead} <- jsonData
        maybeBooks <- lift $ Persistence.setUserBookProgress (UserID userId) bookId pagesRead
        case maybeBooks of
            Just books -> json books
            Nothing -> raiseStatus notFound404 "User with not found"

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
                    Nothing     -> raiseStatus notFound404 "Author not found"
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


searchBooks :: (BookRepository m, MonadUnliftIO m) => ScottyT m ()
searchBooks =
    get "/books" $ do
        author :: Maybe ParsableUUID <- maybeQueryParam "author"
        genres :: Maybe [ParsableUUID] <- maybeQueryParam "genres"

        let authorID = AuthorID . unParsableUUID <$> author
            genreIDs = maybe [] (fmap (GenreID . unParsableUUID)) genres

        books <- lift $ getBooks authorID genreIDs
        json books

bookById :: (BookRepository m, MonadUnliftIO m) => ScottyT m ()
bookById =
    get "/books/:bookId" $ do
        (ParsableUUID bookId) :: ParsableUUID <- captureParam "bookId"
        book <- lift $ getBookById (BookID bookId)
        json book

