module Types.Config
  ( Config(..)
  ) where

import qualified Data.Text as T

data Config = Config
    { rootUrl     :: !T.Text
    , environment :: !T.Text
    , platform    :: !T.Text
    } deriving Show
