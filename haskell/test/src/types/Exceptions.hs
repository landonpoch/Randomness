module Types.Exceptions
  ( Error(..)
  , MyHttpException(..)
  ) where

import           Control.Exception (Exception)
import           Data.Typeable     (Typeable)

type ErrorMsg = String
data Error = JsonParseError ErrorMsg
           | KeyNotFoundError ErrorMsg deriving (Show)

data MyHttpException = HttpBadStatusCode Int | HttpUnknownException
 deriving (Show, Typeable)
instance Exception MyHttpException
