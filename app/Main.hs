{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Snap
import Snap.Util.FileServe
import Control.Applicative ((<|>))

main :: IO ()
-- main = putStrLn "545"
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param