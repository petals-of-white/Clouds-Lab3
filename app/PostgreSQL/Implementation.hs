module PostgreSQL.Implementation where

import           Control.Exception          (throwIO)
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Hasql.Connection           as Connection
import           Hasql.Session              as Session
import           Hasql.Statement            (Statement)
import           Persistence
import qualified PostgreSQL.Sessions        as Sessions
import           PostgreSQL.Statements



newtype PostgreSqlDB a = PostgreSqlDB {unPostgreApp :: ReaderT Settings IO a} deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance UserRepository PostgreSqlDB where
    getUsers = execSingleStatement () getAllUsers

    getUserById (UserID userId)  = execSingleStatement userId findUserById

    createUser user = UserID <$> execSingleStatement user insertUser


instance AuthorRepository PostgreSqlDB where
    getAuthorById (AuthorID authorId) = execSingleStatement authorId findAuthorById

    getAuthors = execSingleStatement () getAllAuthors

    createAuthor author = AuthorID <$> execSingleStatement author insertAuthor

instance GenreRepository PostgreSqlDB where
  getGenreById (GenreID genreId) = execSingleStatement genreId findGenreById

  getGenres = execSingleStatement () getAllGenres

  createGenre genre = GenreID <$> execSingleStatement genre insertGenre


instance BookRepository PostgreSqlDB where
    getBookById bookId = execSession  (Sessions.findBookById bookId)

    getBooks authorFilter genresFilter = execSingleStatement (fmap unAuthorID authorFilter, innermap unGenreID genresFilter) filterBooks
        where innermap f = fmap (fmap f)

    createBook book = execSession (Sessions.insertBookAddGenres book)

instance UserBooksRepo PostgreSqlDB where
  getUserBooks userId = execSession (Sessions.getUserBooks userId)

  setUserBookProgress userId bookId pgRead = execSingleStatement (userId,bookId,pgRead) insertUserBook 

    





execSession :: Session result -> PostgreSqlDB result
execSession sess = PostgreSqlDB $ do
    connStr <- ask
    eitherConn <- lift $ Connection.acquire connStr
    case eitherConn of
        Right conn -> do
            result <- lift $ Session.run sess conn
            case result of
                Right res -> return res
                Left what ->
                    lift $ throwIO what
        Left what -> lift $ error (show what)


execSingleStatement :: params -> Statement params result -> PostgreSqlDB result
execSingleStatement params statement = PostgreSqlDB $ do
    connStr <- ask
    eitherConn <- lift $ Connection.acquire connStr
    case eitherConn of
        Right conn -> do
            result <- lift $ Session.run (Session.statement params statement) conn
            case result of
                Right res -> return res
                Left what ->
                    lift $ throwIO what
        Left what -> lift $ error (show what)
