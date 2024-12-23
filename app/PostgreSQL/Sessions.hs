{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
module PostgreSQL.Sessions where

import           Hasql.Session                    as Session
import           Persistence
import           PostgreSQL.Statements.BookGenres (addBookGenres, bookGenres)
import qualified PostgreSQL.Statements.Books      as B
import qualified PostgreSQL.Statements.UserBooks as UB
import           Types


findBookById :: BookID -> Session (Maybe (Book GenreRecord AuthorRecord))
findBookById (BookID bookId) =
    Session.statement bookId B.findBookById >>= (\case
        Nothing -> return Nothing
        Just b -> Session.statement bookId bookGenres >>= (\g ->
            return (Just b{genres=g})
            )
    )

insertBookAddGenres :: Book GenreID AuthorID -> Session BookID
insertBookAddGenres book = do
    newBookId <- Session.statement (book{genres=[()]}) B.insertBook
    Session.statement (newBookId, map unGenreID (genres book)) addBookGenres
    return (BookID newBookId)


getUserBooks :: UserID -> Session [BookRecord]
getUserBooks bookId = do
    uBooks <- Session.statement bookId UB.getUserBooks
    mapM pullGenres uBooks
    
    where
        pullGenres :: (BookID, Book () AuthorRecord) -> Session BookRecord
        pullGenres (bookId, book) =
            Session.statement (unBookID bookId) bookGenres >>= (\bGenres -> return (BookRecord bookId book{genres=bGenres}))

