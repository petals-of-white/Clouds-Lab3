{-# LANGUAGE OverloadedStrings #-}
module App where
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Persistence
import           Routes
import           Web.Scotty.Trans



app :: (UserRepository m, BookRepository m, AuthorRepository m, GenreRepository m, UserBooksRepo m, MonadUnliftIO m) => ScottyT m ()
app = do
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



