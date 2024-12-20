{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Web.Scotty

main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        beam <- captureParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
