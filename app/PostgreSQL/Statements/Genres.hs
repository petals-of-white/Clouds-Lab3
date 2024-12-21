{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements.Genres where

import           Data.Functor.Contravariant   (contramap)
import           Data.Text                    (pack, unpack)
import           Data.UUID                    (UUID)
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Statement
import           Persistence
import           PostgreSQL.Statements.Codecs
import           Types


-- Statements
findGenreById :: Statement UUID (Maybe Genre)
findGenreById =
    Statement
        "select name from genre where id = $1"
        uuidEncoder
        (D.rowMaybe genreDecoder)
        False

getAllGenres :: Statement () [GenreRecord]
getAllGenres =
    Statement
        "select * from genre"
        E.noParams
        (D.rowList genreRecordDecoder)
        False

insertGenre :: Statement Genre ()
insertGenre =
    Statement
        "insert into genre (name) values ($1)"
        genreEncoder
        D.noResult
        False


-- Decoders
genreDecoder :: D.Row Genre
genreDecoder =
    Genre . unpack <$> D.column (D.nonNullable D.text)


genreRecordDecoder :: D.Row GenreRecord
genreRecordDecoder =
    pure GenreRecord
        <*> fmap GenreID (D.column (D.nonNullable D.uuid))
        <*> genreDecoder

-- Encoders
genreEncoder :: E.Params Genre
genreEncoder =
    contramap (pack . (\(Genre name) -> name)) (E.param (E.nonNullable E.text))



