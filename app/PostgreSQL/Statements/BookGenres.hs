{-# LANGUAGE OverloadedStrings #-}
module PostgreSQL.Statements.BookGenres where

import           Data.UUID                    (UUID)
import qualified Hasql.Decoders               as D
import           Hasql.Statement
import           Persistence
import           PostgreSQL.Statements.Codecs
import           PostgreSQL.Statements.Genres (genreRecordDecoder)

bookGenres :: Statement UUID [GenreRecord]
bookGenres =
    Statement
        "select genre.id, genre.name from book_genres \
        \ inner join genre on book_genres.genreId = genre.id\
        \ where bookId = $1"
        uuidEncoder
        (D.rowList genreRecordDecoder)
        False
