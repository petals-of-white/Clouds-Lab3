{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements.UserBooks where

import           Data.Functor.Contravariant   (contramap)
import           Data.Text                    (pack, unpack)
import           Data.UUID                    (UUID)
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Statement
import           Persistence
import           PostgreSQL.Statements.Codecs
import           Types


-- getUserBooks :: Statement UUID ([Book])
-- getUserBooks = 
--     Statement
--         "select bookId from book where bookId = $1"
--         uuidEncoder
--         (D.rowMaybe bookDecoder)
--         False
        


-- -- Decoders
-- bookDecoder :: D.Row Book
-- genreDecoder =
--     -- Genre . unpack <$> D.column (D.nonNullable D.text)


-- bookRecordDecoder :: D.Row BookRecord
-- bookRecordDecoder =
--     pure GenreRecord
--         <*> fmap GenreID (D.column (D.nonNullable D.uuid))
--         <*> genreDecoder

-- -- Encoders
-- bookEncoder :: E.Params Book
-- bookEncoder =
--     contramap (pack . (\(Genre name) -> name)) (E.param (E.nonNullable E.text))