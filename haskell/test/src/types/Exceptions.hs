module Types.Exceptions
  (Error(..)) where

type ErrorMsg = String
data Error = JsonParseError ErrorMsg
           | KeyNotFoundError ErrorMsg
