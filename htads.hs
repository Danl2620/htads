
import System.IO
import qualified Data.Char as Char
import qualified Data.Map as Map

type RoomName = String
type ItemName = String
type LocationName = String

data Compass = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
             deriving (Ord, Eq, Show, Read)

data Verb = Look | Go Compass | Examine ItemName | Quit | Skip RoomName | Error String
           deriving (Show)

type Connection = Map.Map Compass String

data Room = Room {
      name :: RoomName
    , description :: String
    , connections :: Connection
    } deriving (Show)

data Item = Item {
      itemName :: ItemName
    , itemDescription :: String
    , startLocation :: String
      } deriving (Show)

data PlayerInfo = PlayerInfo {
      currentRoom :: Room
    , visitedRooms :: [RoomName]
      -- inventory and whatnot
      } deriving (Show)

data WorldState = WorldState {
      playerInfo :: PlayerInfo
    , itemMap :: Map.Map ItemName LocationName
    } deriving (Show)

g_roomMap = Map.fromList 
          [("cave", Room "The Cave" "A dank cave" (Map.fromList [(North, "tunnel")]))
          ,("tunnel", Room "The Tunnel" "A low tunnel" (Map.fromList [(South, "cave")]))
          ]

g_itemMap = Map.fromList
          [("table", Item "table" "A small kitchen table" "cave")
          ,("knife", Item "knife" "A large kitchen kife" "table")
          ]

unAbbrevCompass "n" = "north"
unAbbrevCompass "ne" = "northeast"
unAbbrevCompass "s" = "south"
unAbbrevCompass "e" = "east"
unAbbrevCompass "w" = "west"
unAbbrevCompass "nw" = "northwest"
unAbbrevCompass "sw" = "southwest"
unAbbrevCompass "se" = "southeast"
unAbbrevCompass dir = dir

parseCompass :: String -> Maybe Compass
parseCompass dir = case unAbbrevCompass dir of
                     "north" -> Just North
                     "northeast" -> Just NorthEast
                     "northwest" -> Just NorthWest
                     "east" -> Just East
                     "west" -> Just West
                     "south" -> Just South
                     "southeast" -> Just SouthEast
                     "southwest" -> Just SouthWest
                     _ -> Nothing

parseCommand :: String -> Verb
parseCommand cmdline = case cmd of
                         "go" -> case parseCompass obj of
                                   Just dir -> Go dir
                                   Nothing -> Error $ "Unrecognized direction " ++ obj
                         "look" -> Look
                         ":j" -> Skip obj
                         "quit" -> Quit
                         "examine" -> Examine obj
                         _ -> Error $ "Unrecognized command " ++ cmd
    where wordList = words $ map Char.toLower cmdline
          cmd = head wordList
          obj = head $ tail wordList

goToRoom ws roomName = case Map.lookup roomName g_roomMap of
                         Just newRoom -> let pi = (playerInfo ws) { currentRoom = newRoom } in
                                         (ws { playerInfo = pi }, Nothing)
                         Nothing -> error $ "missing room " ++ roomName

parseLine :: WorldState -> String -> (WorldState, Maybe String)
parseLine ws cmdline = case parseCommand cmdline of
                        Look -> (ws, Just $ description room)
                        Go dir -> case Map.lookup dir (connections room) of
                                    Just roomName -> goToRoom ws roomName
                                    Nothing -> (ws, Just "Can't go that direction")
                        Skip roomName -> goToRoom ws roomName
                        Examine itemName -> (ws, Just $ "It looks like a " ++ itemName)
                        Quit -> error "done"
                        Error msg -> (ws, Just msg)
                      where room = currentRoom (playerInfo ws)

visitRoom :: WorldState -> IO WorldState
visitRoom ws = do putStr $ (name (currentRoom (playerInfo ws))) ++ "\n> "
                  hFlush stdout
                  inpStr <- getLine
                  let (newWorldState, maybeMsg) = parseLine ws inpStr
                  case maybeMsg of
                    Just msg -> putStrLn $ msg
                    Nothing -> putStrLn ""
                  visitRoom newWorldState

generateWorldItemMap :: Map.Map ItemName Item -> Map.Map ItemName LocationName
generateWorldItemMap itemMap = 
    Map.map (\item -> startLocation item) itemMap


main = 
    case (Map.lookup "cave" g_roomMap) of
      Just room -> visitRoom $ WorldState (PlayerInfo room []) (generateWorldItemMap g_itemMap)
      Nothing -> error "Asdf"
