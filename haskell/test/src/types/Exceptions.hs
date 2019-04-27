module Types.Exceptions
  ( CustomException(..)
  )
where

import           Control.Exception              ( Exception )
import qualified Data.Text                     as T
import           Data.Typeable                  ( Typeable )

data CustomException = JsonParseError T.Text
  | YamlParseError T.Text
  | KeyNotFoundError T.Text
  | HttpBadStatusCode Int
  | CryptoException T.Text
  | RandomException
  deriving (Show, Typeable)

instance Exception CustomException
