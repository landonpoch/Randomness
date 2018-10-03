module Types.Config
  ( Config(..)
  ) where

data Config = Config
    { rootUrl     :: !String
    , environment :: !String
    , platform    :: !String
    } deriving Show
