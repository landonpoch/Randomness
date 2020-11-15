module Types.Exceptions
  ( CustomException(..)
  )
where

import Protolude ( Show, Typeable, Int, Exception )
import qualified Data.Text                     as T

data CustomException = JsonParseError T.Text
  | YamlParseError T.Text
  | KeyNotFoundError T.Text
  | HttpBadStatusCode Int
  | CryptoException T.Text
  | RandomException
  deriving (Show, Typeable)

instance Exception CustomException
