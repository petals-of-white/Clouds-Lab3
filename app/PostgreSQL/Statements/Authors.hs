{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements.Authors where

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
getAllAuthors :: Statement () [AuthorRecord]
getAllAuthors =
    Statement
        "select firstName, lastName from author"
        E.noParams
        (D.rowList authorRecordDecoder)
        False


findAuthorById :: Statement UUID (Maybe Author)
findAuthorById =
    Statement
        "select firstName, lastName from author where id = $1"
        uuidEncoder
        (D.rowMaybe authorDecoder)
        False

insertAuthor :: Statement Author ()
insertAuthor =
    Statement
        "insert into author (firstName, lastName) values ($1, $2)"
        authorEncoder
        D.noResult
        False

-- Encoders
authorEncoder :: E.Params Author
authorEncoder =
    contramap (pack . (\Author {firstName} -> firstName)) (E.param (E.nonNullable E.text))
    <>
    contramap (pack . (\Author {lastName} -> lastName)) (E.param (E.nonNullable E.text))


authorDecoder :: D.Row Author
authorDecoder =
    fmap (\(fn, ln) -> Author (unpack fn) (unpack ln))
    $   pure (,) <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.text)


authorRecordDecoder :: D.Row AuthorRecord
authorRecordDecoder =
    pure AuthorRecord
        <*> fmap AuthorID (D.column (D.nonNullable D.uuid))
        <*> authorDecoder
