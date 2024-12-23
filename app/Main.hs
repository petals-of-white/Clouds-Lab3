{-# LANGUAGE OverloadedStrings #-}
module Main where
import           App                        (app)
import           Web.Scotty.Trans
-- import Control.Monad.Reader (ReaderT(runReaderT))
import           Control.Monad.Trans.Reader (ReaderT (runReaderT))
import           Data.ByteString.Char8
import           Hasql.Connection           (Settings)
import           PostgreSQL.Implementation  (PostgreSqlDB (unPostgreApp))
import           System.Environment         (getEnv)

main :: IO ()
main = do
    settings :: Settings <- pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
    scottyT 8000 (flip runReaderT settings . unPostgreApp) app
