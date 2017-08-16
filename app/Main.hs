{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson              (FromJSON, ToJSON, Value (Number),
                                          decode, encode)
import           Data.Aeson.Lens
import           Data.Map                as Map
import           Data.Maybe
import           Data.Scientific
import           Data.String.Conv
import           Data.Text
import qualified Data.Text               as T
import           Data.Time
import           Data.Time.Locale.Compat
import           GHC.Generics
import           Lib
import           Network.API.TheMovieDB
import           Network.Wreq            as Wreq
import           Text.Printf
import           Web.Scotty              as Scotty

main :: IO ()
main =
  scotty 3000 $
   Scotty.get "/" $ do
    responseFromAPI <- liftIO $ Wreq.get "https://api.themoviedb.org/3/search/movie?api_key=443faaae5d25a64487005863edc6c726&query=The+Avengers"
    let movieIdFromSearch = responseFromAPI ^? responseBody  . (key "results") . (nth 0) . (key "id") ::Maybe Value
    case movieIdFromSearch of
      Just (Number id) -> do
        raw $ toS (show $ truncate id)
      Nothing -> raw ("Could not get a valid movie ID from the search query")
    -- raw $ toS (show movieIdFromSearch)
