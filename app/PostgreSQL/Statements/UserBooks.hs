{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements.UserBooks where

import           Contravariant.Extras         (contrazip3)
import           Data.Functor.Contravariant   (contramap)
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Statement
import           Numeric.Natural              (Natural)
import           Persistence
import           PostgreSQL.Statements.Books  (bookDecoder)
import           PostgreSQL.Statements.Codecs
import           Types


getUserBooks :: Statement UserID [(BookID, Book () AuthorRecord)]
getUserBooks =
    Statement
        "select book.id, book.title, book.numberOfPages, author.id, author.firstName, author.lastName \
        \ from book inner join author on book.authorId = author.id \
        \ where book.id = $1 "
        (contramap unUserID uuidEncoder)
        (D.rowList (((,) . BookID <$> D.column (D.nonNullable D.uuid)) <*> bookDecoder))
        False

insertUserBook :: Statement (UserID, BookID, Natural) ()
insertUserBook =
    Statement
        "insert into user_books (userId, bookId, pagesRead) \
        \ values ($1, $2, $3)"
        insertBookEncoder
        D.noResult
        False

insertBookEncoder :: E.Params (UserID, BookID, Natural)
insertBookEncoder =
    contramap (\(userId, bookId, pgRead) -> (unUserID userId, unBookID bookId, fromIntegral pgRead))
    $ contrazip3
        (E.param (E.nonNullable E.uuid))
        (E.param (E.nonNullable E.uuid))
        (E.param (E.nonNullable E.int4))
