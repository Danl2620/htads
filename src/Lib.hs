module Lib
    ( play
    ) where

import System.IO
import qualified Data.Map.Strict as Map
import Data.Text as Text
import GHC.Generics (Generic)
import Control.Monad
import Data.Yaml
--import Data.Yaml.Types
import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS

import Htads
import Alias

g_roomMap :: RoomMap
g_roomMap = Map.fromList
          [("start", Room "Outside cave" "You're standing in the bright sunlight just outside of a large, dark, foreboding cave, which lies to the north. " (Map.fromList [(North, "cave")]))
          ,("cave", Room "Cave" "You're inside a dark and musty cave. Sunlight pours in from a passage to the south." (Map.fromList [(South, "start")]))
          ]

g_itemMap :: ItemMap
g_itemMap = Map.fromList
          [("pedestal", Item "pedestal" ["pedestal"] [] "A pedestal" [Fixed] "cave")
          ,("skull", Item "skull" ["skull"] ["gold"] "A gold skull" [Score 10] "cave")
          ,("table", Item "table" ["table"] ["small"] "A small kitchen table" [Bulky] "cave")
          ,("largeSandbag", Item "largeSandbag" ["sandbag", "bag"] ["large"] "A large bag of sand" [Bulky] "cave")
          ,("smallSandbag1", Item "smallSandbag1" ["sandbag", "bag"] ["small", "red"] "A small red bag of sand" [] "cave")
          ,("smallSandbag2", Item "smallSandbag2" ["sandbag", "bag"] ["small", "blue"] "A small blue bag of sand" [] "cave")
          --,("knife", Item "knife" ["knife"] ["large"] "A large kitchen kife" [] "table")
          ]

data Link = Dir Compass String
  deriving (Generic, Show)

instance FromJSON Compass where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case (tag :: Text.Text) of
      "North" -> return North
      "South" -> return South
      "East" -> return East
      "West" -> return West
      _ -> mzero
  parseJSON _ = error "Can't parse Compass from YAML/JSON"

-- instance FromJSON Connection
instance FromJSON ItemAttribute
instance FromJSON Link


data RoomStage0 = RoomStage0 {
  name :: RoomName
  , summary :: String
  , description :: String
  , connections :: [Link]
  } deriving (Show, Generic)

instance FromJSON RoomStage0 where
    parseJSON (Object v) = RoomStage0 <$>
                           v .: "name" <*>
                           v .: "summary" <*>
                           v .: "description" <*>
                           v .: "connections"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Room from YAML/JSON"

instance FromJSON Item where
    parseJSON (Object v) = Item <$>
                           v .: "itemId" <*>
                           v .: "nouns" <*>
                           v .: "adjectives" <*>
                           v .: "itemDescription" <*>
                           v .: "itemAttributes" <*>
                           v .: "startLocation"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Item from YAML/JSON"


someFunc :: IO ()
someFunc = do
         ymlData <- BS.readFile "defs/defs.txt"
         let
           rooms = Data.Yaml.decode ymlData :: Maybe [RoomStage0]
           --rooms = decode "name: thing" :: Maybe RoomStage0
           -- items = Data.Yaml.decode ymlData :: Maybe [Item]
         -- Print it, just for show
         print rooms


play :: IO ()
play = someFunc
  -- do someFunc
  --    h <- openFile "defs/aliases.txt" ReadMode
  --    c <- hGetContents h
  --    let aliases = case parseAliases c of
  --          Left e -> Map.empty
  --          Right r -> Map.fromList r
  --    (endWs, res) <- runAdventure g_roomMap g_itemMap aliases
  --    putStrLn $ "Finished with score " ++ show (getScore endWs)
