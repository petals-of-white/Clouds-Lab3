{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements.Codecs where
import qualified Hasql.Encoders             as E
import           Data.UUID                  (UUID)

uuidEncoder :: E.Params UUID
uuidEncoder = E.param (E.nonNullable E.uuid)