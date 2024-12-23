module Routes (module Authors, module Books, module Genres, module Users, module UserBooks) where
import           Routes.Authors   as Authors
import           Routes.Books     as Books
import           Routes.Genres    as Genres
import           Routes.UserBooks as UserBooks
import           Routes.Users     as Users
