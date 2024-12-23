{-# LANGUAGE OverloadedStrings #-}

module Routes.Common where

import           Control.Exception       (SomeException (SomeException))
import           Control.Monad.IO.Unlift
import           Data.Text.Lazy          (Text, unpack)
import           Data.UUID               as UUID
import           Web.Scotty.Trans


newtype ParsableUUID = ParsableUUID {unParsableUUID :: UUID}

instance Parsable ParsableUUID where
    parseParam textRepr =
        case UUID.fromString (unpack textRepr) of
            Just uuid -> Right (ParsableUUID uuid)
            Nothing   -> Left $ "Cannot represent " <> textRepr <> " as UUID"
            
-- This is potentially bad, as it catches all exceptions :(
maybeQueryParam :: (Parsable a, MonadUnliftIO m) => Text -> ActionT m (Maybe a)
maybeQueryParam p = rescue (Just <$> queryParam p) (\(SomeException _) -> return Nothing)
