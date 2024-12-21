module PostgreSQL.Implementation where

import           Control.Monad.Trans.Reader 
import           Hasql.Connection            as Connection
import qualified Hasql.Session               as Session
import           Persistence
import           PostgreSQL.Statements.Users
import Control.Monad.Trans.Class (lift)
import Control.Exception (throwIO)
import Hasql.Statement (Statement)



newtype PostgreSqlDB a = PostgreSqlDB {unPostgreApp :: ReaderT Settings IO a} deriving (Functor, Applicative, Monad)

instance UserRepository (PostgreSqlDB) where
    getUsers = execSingleStatement () getAllUsers

    getUserById (UserID userId)  = execSingleStatement userId findUserById
    -- createUser user = execSingleStatement user insertUser


execSingleStatement :: params -> Statement params result -> PostgreSqlDB result
execSingleStatement params statement = PostgreSqlDB $ do
    connStr <- ask
    eitherConn <- lift $ Connection.acquire connStr
    case eitherConn of
        Right conn -> do
            result <- lift $ Session.run (Session.statement params statement) conn
            case result of
                Right res -> return res
                Left what ->
                    lift $ throwIO what
        Left what -> lift $ error (show what)
