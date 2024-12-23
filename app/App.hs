module App where
import           Control.Monad.IO.Unlift
import           Data.Text.Lazy          (pack)
import           Hasql.Session
import           Network.HTTP.Types      (status500)
import           Persistence
import           Routes
import           Web.Scotty.Trans
-- import Control.Monad.Trans.Reader (ReaderT)


app ::
    (UserRepository m, AuthorRepository m, GenreRepository m, BookRepository m, UserBooksRepo m, MonadUnliftIO m)
    => ScottyT m ()
-- app :: ScottyT PostgreSqlDB ()
app = do

    defaultHandler (Handler (\qerr@(QueryError query params err) ->
        status status500 >> text (pack (show qerr)))
        )

    allUsers
    userById
    addUser

    allAuthors
    addAuthor
    authorById

    allGenres
    addGenre
    genreById

    searchBooks
    bookById
    addBook

    usersBooks
    Routes.setUserBookProgress

--     userById



