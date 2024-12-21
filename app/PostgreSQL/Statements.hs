
module PostgreSQL.Statements (module Users, module Authors, module Books, module Genres, module UserBooks) where
import           PostgreSQL.Statements.Authors   as Authors
import           PostgreSQL.Statements.Books     as Books
import           PostgreSQL.Statements.Genres    as Genres
import           PostgreSQL.Statements.UserBooks as UserBooks
import           PostgreSQL.Statements.Users     as Users
