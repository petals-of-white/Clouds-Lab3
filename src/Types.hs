{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import           Data.Aeson      (FromJSON, ToJSON)
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)


data User = User {firstName :: String, lastName :: String} deriving (Generic, Show)
data Book genre author = Book {title :: String, numberOfPages :: Natural, genres :: [genre], author :: author} deriving (Generic, Show)
newtype Genre = Genre {name :: String} deriving (Generic, Show)
data Author = Author {firstName :: String, lastName :: String} deriving (Generic, Show)

instance ToJSON User
instance FromJSON User
instance (ToJSON genre, ToJSON author) => ToJSON (Book genre author)
instance (FromJSON genre, FromJSON author) => FromJSON (Book genre author)
instance ToJSON Genre
instance FromJSON Genre
instance ToJSON Author
instance FromJSON Author
