module Common where

class Has field env where
    obtain :: env -> field