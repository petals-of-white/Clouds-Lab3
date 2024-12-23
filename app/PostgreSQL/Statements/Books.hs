{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module PostgreSQL.Statements.Books where

import           Contravariant.Extras.Contrazip
import           Data.Functor.Contravariant     (contramap)
import           Data.Int                       (Int32)
import           Data.Text                      (Text, pack, unpack)
import           Data.UUID                      (UUID)
import qualified Hasql.Decoders                 as D
import qualified Hasql.Encoders                 as E
import           Hasql.Statement
import           Persistence
import           PostgreSQL.Statements.Authors  (authorRecordDecoder)
import           PostgreSQL.Statements.Codecs
import           Types


findBookById :: Statement UUID (Maybe (Book () AuthorRecord))
findBookById =
    Statement
        "select title, numberOfPages, author.id, author.firstName, author.lastName \
        \ from book where bookId = $1 \
        \ inner join author on book.authorId = author.id"
        uuidEncoder
        (D.rowMaybe bookDecoder)
        False

-- findBookGenres :: Statement UUID [GenreRecord]
-- findBookGenres =
--     Statement
--         "select genre.id, genre.name from book_genres \
--         \ inner join genre on book_genres.genreId = genre.id \
--         \ where bookId = $1"
--         uuidEncoder
--         (D.rowList genreRecordDecoder)
--         False

insertBook :: Statement (Book () AuthorID) UUID
insertBook =
    Statement
        "insert into book (title, numberOfPages, authorId) \
        \ values ($1, $2, $3) returning id"
        bookEncoder
        (head <$> D.rowList (D.column (D.nonNullable D.uuid)))
        False

filterBooks :: Statement (Maybe UUID, Maybe [UUID]) [BookRecord]
filterBooks =
    let sql =
            "select book.id, book.title, book.numberOfPages, \
            \ author.id, author.firstName, author.lastName, genre.id, genre.name \
            \ from book inner join author on book.authorId = author.id \
            \ inner join book_genres on book.id = book_genres.bookId  \
            \ inner join genre on book_genres.genreId = genre.id \
            \ where (author.id = $1 OR $1 is null) \
            \ and (genre.id = any($2) OR $2 is null)"

        collectGenres [] FlatBookInfo{..} =
            [BookRecord {
                id=BookID id,
                book= Book {
                    title=unpack title,
                    numberOfPages=fromIntegral numberOfPages,
                    genres=[GenreRecord (GenreID genreId) (Genre (unpack genreName))],
                    author=AuthorRecord (AuthorID authorId) (Author (unpack authorFirstName) (unpack authorLastName))}
                }]

        collectGenres
            (x@BookRecord{id=(BookID currentBookId), book=currentBook@Book{genres}}:xs)
            fb@FlatBookInfo {..}
                | currentBookId == id = (x {book = currentBook {genres=flatBookGenre fb : genres}}):xs
                | otherwise = collectGenres [] fb ++ (x:xs)

    in
    Statement
        sql
        bookFilterEncoder
        (D.foldlRows collectGenres [] flatBookDecoder)
        False








-- Decoders
bookDecoder :: D.Row (Book () AuthorRecord)
bookDecoder =
    fmap (\(t,pages,authorRec) ->
        Book {title=unpack t, numberOfPages= fromIntegral pages, author = authorRec, genres=[()]}) $
    pure (,,) <*> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.int4) <*> authorRecordDecoder




flatBookDecoder :: D.Row FlatBookInfo
flatBookDecoder =
    FlatBookInfo <$> D.column (D.nonNullable D.uuid) <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.int4) <*> D.column (D.nonNullable D.uuid) <*> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.uuid) <*> D.column (D.nonNullable D.text)

-- fullBookDecoder :: D.Row (Book GenreRecord AuthorRecord)
-- fullBookDecoder =
--     fmap (\(t,pages,authorRec,genreRec) ->
--         Book {title=unpack t, numberOfPages= fromIntegral pages, author = authorRec, genres=[()]}) $
--     pure (,,,) <*> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.int4)
--         <*> authorRecordDecoder <*> genreRecordDecoder

-- bookRecordDecoder :: D.Row BookRecord
-- bookRecordDecoder =
--     pure BookRecord
--         <*> fmap BookID (D.column (D.nonNullable D.uuid))
--         <*> fullBookDecoder

-- Encoders
bookEncoder :: E.Params (Book () AuthorID)
bookEncoder =
    contramap (\Book{title=t, numberOfPages=pages, author=(AuthorID uuid)} -> (pack t, fromIntegral pages, uuid))
    $ contrazip3 (E.param (E.nonNullable E.text)) (E.param (E.nonNullable E.int4)) uuidEncoder

bookFilterEncoder :: E.Params (Maybe UUID, Maybe [UUID])
bookFilterEncoder =
    contrazip2
        (E.param (E.nullable E.uuid))
        (E.param (E.nullable (E.foldableArray (E.nonNullable E.uuid))))

data FlatBookInfo = FlatBookInfo {
    id :: UUID, title :: Text, numberOfPages :: Int32,
    authorId :: UUID, authorFirstName :: Text, authorLastName :: Text,
    genreId :: UUID, genreName :: Text}

flatBookAuthor :: FlatBookInfo -> AuthorRecord
flatBookAuthor FlatBookInfo{authorId, authorFirstName, authorLastName} = AuthorRecord (AuthorID authorId) (Author (unpack authorFirstName) (unpack authorLastName))

flatBookGenre :: FlatBookInfo -> GenreRecord
flatBookGenre FlatBookInfo {genreId, genreName} = GenreRecord (GenreID genreId) (Genre (unpack genreName))
