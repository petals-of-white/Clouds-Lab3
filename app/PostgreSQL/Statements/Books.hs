{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgreSQL.Statements.Books where

import           Data.Functor.Contravariant   (contramap)
import           Data.Text                    (pack, unpack)
import           Data.UUID                    (UUID)
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Statement
import           Persistence
import           PostgreSQL.Statements.Codecs
import           Types
import Contravariant.Extras.Contrazip


-- findBookById :: Statement UUID (Maybe (Book GenreRecord AuthorRecord))
-- findBookById = 
--     Statement
--         "select * from book where bookId = $1"
--         uuidEncoder
--         (D.rowMaybe bookDecoder)
--         False


-- insertBook :: Statement (Book GenreID AuthorID) ()
-- insertBook = 
--     Statement
--         "insert into book (title, numberOfPages, authorId) values ($1, $2, $3)"
        

        
-- -- Decoders
-- bookDecoder :: D.Row (Book GenreRecord AuthorRecord)
-- bookDecoder =
--     fmap (\(t,pages,authorId) -> Book {title=t, numberOfPages=pages, author = authorId}) $
--     pure (,,) <*> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.int4) <*> D.column (D.nonNullable D.uuid)
--     -- Genre . unpack <$> D.column (D.nonNullable D.text)



-- bookRecordDecoder :: D.Row BookRecord
-- bookRecordDecoder =
--     pure BookRecord
--         <*> fmap BookID (D.column (D.nonNullable D.uuid))
--         <*> genreDecoder

-- -- Encoders
-- bookEncoder :: E.Params Book
-- bookEncoder =
--     contramap (\Book{title=t, numberOfPages=pages, author=a} -> (pack t, fromIntegral pages,)) 
--     $ contrazip3 (E.param (E.nonNullable E.text)) (E.param (E.nonNullable E.int4)) (E.param (E.nonNullable E.uuid))