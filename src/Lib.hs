module Lib
    ( play
    ) where

import System.IO
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text as Text
import GHC.Generics (Generic)
import Control.Monad
import Data.Yaml
--import Data.Yaml.Types
import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS

import qualified Htads as H
import Alias

g_roomMap :: H.RoomMap
g_roomMap = Map.fromList
          [("start", H.Room "Outside cave" "You're standing in the bright sunlight just outside of a large, dark, foreboding cave, which lies to the north. " (Map.fromList [(H.North, "cave")]))
          ,("cave", H.Room "Cave" "You're inside a dark and musty cave. Sunlight pours in from a passage to the south." (Map.fromList [(H.South, "start")]))
          ]

g_itemMap :: H.ItemMap
g_itemMap = Map.fromList
          [("pedestal", H.Item "pedestal" ["pedestal"] [] "A pedestal" [H.Fixed] "cave")
          ,("skull", H.Item "skull" ["skull"] ["gold"] "A gold skull" [H.Score 10] "cave")
          ,("table", H.Item "table" ["table"] ["small"] "A small kitchen table" [H.Bulky] "cave")
          ,("largeSandbag", H.Item "largeSandbag" ["sandbag", "bag"] ["large"] "A large bag of sand" [H.Bulky] "cave")
          ,("smallSandbag1", H.Item "smallSandbag1" ["sandbag", "bag"] ["small", "red"] "A small red bag of sand" [] "cave")
          ,("smallSandbag2", H.Item "smallSandbag2" ["sandbag", "bag"] ["small", "blue"] "A small blue bag of sand" [] "cave")
          --,("knife", H.Item "knife" ["knife"] ["large"] "A large kitchen kife" [] "table")
          ]

data Link = Dir H.Compass String
  deriving (Generic, Show)

instance FromJSON H.Compass where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case (tag :: Text.Text) of
      "North" -> return H.North
      "South" -> return H.South
      "East" -> return H.East
      "West" -> return H.West
      _ -> mzero
  parseJSON _ = error "Can't parse Compass from YAML/JSON"

-- instance FromJSON Connection
instance FromJSON H.ItemAttribute
instance FromJSON Link


data RoomStage0 = RoomStage0 {
  name :: H.RoomName
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

instance FromJSON H.Item where
    parseJSON (Object v) = H.Item <$>
                           v .: "itemId" <*>
                           v .: "nouns" <*>
                           v .: "adjectives" <*>
                           v .: "itemDescription" <*>
                           v .: "itemAttributes" <*>
                           v .: "startLocation"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Item from YAML/JSON"


convert :: RoomStage0 -> H.Room
convert r = H.Room (name r) (description r) (Map.fromList [])

readDefs :: BS.ByteString -> Maybe [RoomStage0]
readDefs contents = Data.Yaml.decode contents :: Maybe [RoomStage0]

someFunc :: IO ()
someFunc = do
  contents <- BS.readFile "defs/defs.txt"
  print $ case (readDefs contents) of
            Just rsl -> List.map convert rsl
            Nothing -> error $ "error reading defs.txt"


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
