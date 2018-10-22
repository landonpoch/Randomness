module Types.Exceptions
  ( CustomException(..)
  ) where

import           Control.Exception (Exception)
import qualified Data.Text         as T
import           Data.Typeable     (Typeable)

data CustomException = JsonParseError T.Text
  | KeyNotFoundError T.Text
  | HttpBadStatusCode Int
  | RandomException
  deriving (Show, Typeable)

instance Exception CustomException
