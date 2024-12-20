{-# LANGUAGE OverloadedStrings #-}

module PostgreSQL where

import Hasql.Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders

createUsersTable :: Statement () ()
createUsersTable = let
    sql = 
        "CREATE TABLE IF NOT EXISTS users ( \
        \ id SERIAL PRIMARY KEY, \
        \ firstName TEXT NOT NULL, \
        \ lastName TEXT NOT NULL \
        \)"
    in Statement sql Encoders.noParams Decoders.noResult False

    
        