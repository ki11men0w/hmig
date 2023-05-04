module Utils
       ( toUri
       , toPath
       , error'
       , showProgess
       ) where

import Network.URI (URI, parseRelativeReference, parseURI)
import Network.URI.Encode (encode)
import Data.List (intercalate)

error' :: String -> a
error' = errorWithoutStackTrace

toPath :: [String] -> URI
toPath ss =
  let path_ = intercalate "/" (encode <$> ss)
  in
    case parseRelativeReference path_ of
      Just u -> u
      _ -> error $ "Invalid relative URI: " <> path_

toUri :: String -> URI
toUri uri =
  case parseURI uri of
    Just u -> u
    _ -> error $ "Invalid URI: " <> uri

showProgess :: Int -> Int -> String
showProgess no count = "(" <> show no <> "/" <> show count <> ")"