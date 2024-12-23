{-# LANGUAGE OverloadedStrings #-}
module PostgreSQL.Statements.BookGenres where

import           Contravariant.Extras         (contrazip2)
import           Data.UUID                    (UUID)
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
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

addBookGenres :: Statement (UUID, [UUID]) ()
addBookGenres =
    Statement
        "with genre_list as ( \
            \ select unnest($2) as genre_id \
        \)\
        \insert into book_genres (bookId, genreId) \
        \select $1,genre_id from genre_list;"
        bookGenresEncoder
        D.noResult
        False


bookGenresEncoder :: E.Params (UUID, [UUID])
bookGenresEncoder =
    contrazip2
        uuidEncoder
        (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.uuid))))

