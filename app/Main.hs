{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Web.Scotty
import Web.Scotty.Trans (scottyT)
import App (app)
import qualified Hasql.Connection as Connection
main :: IO ()
main = do
    
    putStrLn "no app"
    -- scottyT 3000 id app