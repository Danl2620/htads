
import System.IO
import qualified Data.Char as Char
import qualified Data.Map as Map
import Alias

type RoomName = String
type ItemName = String
type LocationName = String

data Compass = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
             deriving (Ord, Eq, Show, Read)

data Verb = Look | Go Compass | Examine ItemName | Quit | Skip RoomName | Error String
           deriving (Show)

type Connection = Map.Map Compass String
type Aliases = Map.Map String String

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
    , visitedRooms :: [LocationName]
      -- inventory and whatnot
      } deriving (Show)

data WorldState = WorldState {
      playerInfo :: PlayerInfo
    , itemMap :: Map.Map ItemName LocationName
    , aliases :: Aliases
    } deriving (Show)

g_nullRoom = Room "<none>" "<none>" Map.empty

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

translateCommand :: Aliases -> String -> String
translateCommand aliases cmd = 
    case Map.lookup cmd aliases of
      Just newCmd -> newCmd
      Nothing -> cmd

parseCommand :: WorldState ->String -> Verb
parseCommand ws cmdline = 
    case cmd of
      "go" -> case parseCompass obj of
                Just dir -> Go dir
                Nothing -> Error $ "Unrecognized direction " ++ obj
      "look" -> Look
      ":j" -> Skip obj
      "quit" -> Quit
      "examine" -> Examine obj
      _ -> Error $ "Unrecognized command " ++ cmd
    where wordList = words $ map Char.toLower (translateCommand (aliases ws) cmdline)
          cmd = head wordList
          obj = head $ tail wordList

combineVisitedRooms :: LocationName -> [LocationName] -> [LocationName]
combineVisitedRooms name nameList = 
    if name `elem` nameList 
       then nameList
       else name : nameList

goToRoom ws roomName = 
    case Map.lookup roomName g_roomMap of
      Just newRoom -> let oldPi = (playerInfo ws) 
                          pi = oldPi { currentRoom = newRoom
                                     , visitedRooms = combineVisitedRooms roomName (visitedRooms oldPi) } 
                          maybeMsg = if roomName `elem` (visitedRooms oldPi)
                                       then Just $ name newRoom
                                       else Just $ description newRoom in                        
                      (ws { playerInfo = pi }, maybeMsg)
      Nothing -> error $ "missing room " ++ roomName

parseLine :: WorldState -> String -> (WorldState, Maybe String)
parseLine ws cmdline = case parseCommand ws cmdline of
                        Look -> (ws, Just $ description room)
                        Go dir -> case Map.lookup dir (connections room) of
                                    Just roomName -> goToRoom ws roomName
                                    Nothing -> (ws, Just "Can't go that direction")
                        Skip roomName -> goToRoom ws roomName
                        Examine itemName -> (ws, Just $ "It looks like a " ++ itemName)
                        Quit -> error "done"
                        Error msg -> (ws, Just msg)
                      where room = currentRoom (playerInfo ws)

eval :: WorldState -> String -> IO WorldState
eval ws cmd = do let (newWorldState, maybeMsg) = parseLine ws cmd
                 case maybeMsg of
                   Just msg -> putStrLn $ msg
                   Nothing -> putStrLn ""
                 putStr "> "
                 hFlush stdout
                 inpStr <- getLine
                 eval newWorldState inpStr

generateWorldItemMap :: Map.Map ItemName Item -> Map.Map ItemName LocationName
generateWorldItemMap itemMap = 
    Map.map (\item -> startLocation item) itemMap


main = 
    do h <- openFile "aliases.txt" ReadMode
       c <- hGetContents h
       let aliasMap = case parseAliases c of
                        Left e -> Map.empty
                        Right r -> Map.fromList r
           ws = WorldState (PlayerInfo g_nullRoom []) (generateWorldItemMap g_itemMap) aliasMap
       eval ws ":j cave"
    