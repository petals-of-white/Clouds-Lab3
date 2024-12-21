{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements.Users where

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
findUserById :: Statement UUID (Maybe User)
findUserById =
    Statement
        "select firstName, lastName from users where id = $1"
        uuidEncoder
        (D.rowMaybe userDecoder)
        False

getAllUsers :: Statement () [UserRecord]
getAllUsers =
    Statement
        "select firstName, lastName from users"
        E.noParams
        (D.rowList userRecordDecoder)
        False

insertUser :: Statement User ()
insertUser =
    Statement
        "insert into user (firstName, lastName) values ($1, $2)"
        userEncoder
        D.noResult
        False


-- Decoders
userDecoder :: D.Row User
userDecoder =
    fmap (\(fn, ln) -> User (unpack fn) (unpack ln))
    $   pure (,) <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.text)


userRecordDecoder :: D.Row UserRecord
userRecordDecoder =
    pure UserRecord
        <*> fmap UserID (D.column (D.nonNullable D.uuid))
        <*> userDecoder

-- Encoders
userEncoder :: E.Params User
userEncoder =
    contramap (pack . (\User {firstName} -> firstName)) (E.param (E.nonNullable E.text))
    <>
    contramap (pack . (\User {lastName} -> lastName)) (E.param (E.nonNullable E.text))



