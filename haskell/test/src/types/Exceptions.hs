module Types.Exceptions
  ( CustomException(..)
  ) where

import           Control.Exception (Exception)
import           Data.Typeable     (Typeable)

data CustomException = JsonParseError String
  | KeyNotFoundError String
  | HttpBadStatusCode Int
  deriving (Show, Typeable)

instance Exception CustomException
