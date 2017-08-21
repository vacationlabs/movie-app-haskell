{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson              (FromJSON, ToJSON, Value (Number),
                                          decode, encode)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
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
-- import           Network.API.TheMovieDB
import qualified Data.Map                as Map
import           Data.Text.Internal.Lazy
import           Data.Text.Lazy
import           Lucid
import           Lucid.Base
import           Lucid.Html5
import           Network.Wreq            as Wreq
import           Text.Printf
import           Web.Scotty              as Scotty

data ProductionCompany = ProductionCompany {
 pc_name :: String, pc_id :: Integer
} deriving (Show, Generic)
instance ToJSON ProductionCompany
-- where
--  toJSON = genericToJSON (defaultOptions {constructorTagModifier = Prelude.drop 3} )
instance FromJSON ProductionCompany where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = Prelude.drop 3})

data Genre = Genre {
 id :: Integer, name :: String
} deriving (Show, Generic)
instance ToJSON Genre
instance FromJSON Genre

data MovieInfo = MovieInfo {
               original_title       :: String,
               overview             :: String,
               production_companies :: [ProductionCompany],
               release_date         :: String,
               genres               :: [Genre],
               tagline              :: String
} deriving (Show, Generic)
instance ToJSON MovieInfo
instance FromJSON MovieInfo

replaceSpace = Prelude.map (\c -> if c==' ' then '+' ; else c)

main :: IO ()
main =
  scotty 3000 $ do

   Scotty.get "/" $ do
     html . renderText $
      html_ $
       body_ $ do
         h1_ "Query The Movie Db with any movie name"
         let typeTextAttr = Lucid.Base.makeAttribute "type" "text"
         let typeSubmitAttr = Lucid.Base.makeAttribute "type" "submit"
         let inputBoxNameAttr = Lucid.Base.makeAttribute "name" "movieName"
         with form_ [method_ "post", action_ "/"] $ do
          input_ [typeTextAttr, inputBoxNameAttr]
          with button_ [typeSubmitAttr] "Search"

   Scotty.post "/" $ do
    -- html "Welcome to post!"
    movie <- Scotty.param "movieName"
    let movieString = replaceSpace $ Data.Text.Lazy.unpack movie
    html $ mconcat ["You just submitted: ", movie]
    generalMovieSearchResponseFromAPI <- liftIO $ Wreq.get ("https://api.themoviedb.org/3/search/movie?api_key=443faaae5d25a64487005863edc6c726&query="++movieString)
    let movieIdFromSearch = generalMovieSearchResponseFromAPI ^? responseBody  . (key "results") . (nth 0) . (key "id") ::Maybe Value
    case movieIdFromSearch of
      Just (Number id) -> do
        raw $ toS (show $ truncate id)
        let movieIdString = show $ truncate id
        movieInfo <- liftIO $ Wreq.get ("https://api.themoviedb.org/3/movie/"++ movieIdString ++"?api_key=443faaae5d25a64487005863edc6c726&language=en-US")
        let movieInfoDisplay = decode (movieInfo ^. responseBody) :: Maybe MovieInfo
        case movieInfoDisplay of
          Just movie ->
           html . renderText $
            html_ $
             body_ $ do
              h1_ $ toHtml $ original_title movie
              p_ $ toHtml $ "Plot Overview : " ++ overview movie
              p_ $ toHtml $ "Release Date : " ++ release_date movie
            -- html $ toS (show  movie) -- +"2nd call returned"
          Nothing -> raw "Could not parse Movie details into MovieInfo type"
      Nothing -> raw "Could not get a valid movie ID from the search query"
