{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
module PostgreSQL.Sessions where

import           Hasql.Session                    as Session
import           Persistence
import           PostgreSQL.Statements.BookGenres (bookGenres)
import qualified PostgreSQL.Statements.Books      as B
import           Types


findBookById :: BookID -> Session (Maybe (Book GenreRecord AuthorRecord))
findBookById (BookID bookId) =
    Session.statement bookId B.findBookById >>= (\case
        Nothing -> return Nothing
        Just b -> Session.statement bookId bookGenres >>= (\g ->
            return (Just b{genres=g})
            )
    )



