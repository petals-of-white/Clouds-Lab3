{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements where

import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text                  (Text, pack, unpack)
import           Data.UUID                  (UUID)
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Statement
import           Persistence
import           Types








-- createUsersTable :: Statement () ()
-- createUsersTable = let
--     sql =
--         "CREATE TABLE IF NOT EXISTS user ( \
--         \ id UUID PRIMARY KEY, \
--         \ firstName TEXT NOT NULL, \
--         \ lastName TEXT NOT NULL \
--         \)"
--     in Statement sql E.noParams D.noResult False







