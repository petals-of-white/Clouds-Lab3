module Env where
import           Control.Monad.Reader
import qualified Hasql.Connection     as Connection



newtype Env = Env {envConnectionString :: Connection.Settings}

newtype App a = App {unApp :: ReaderT Env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runApp :: Env -> App a -> IO a
runApp env app = runReaderT (unApp app) env
