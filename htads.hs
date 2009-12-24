
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
      summary :: RoomName
    , description :: String
    , connections :: Connection
    } deriving (Show)

data Item = Item {
      itemName :: ItemName
    , itemDescription :: String
    , startLocation :: String
      } deriving (Show)

data PlayerInfo = PlayerInfo {
      currentRoom :: LocationName
    , visitedRooms :: [LocationName]
      -- inventory and whatnot
      } deriving (Show)

data WorldState = WorldState {
      playerInfo :: PlayerInfo
    , itemMap :: Map.Map LocationName [ItemName]
    , aliases :: Aliases
    } deriving (Show)

g_nullRoom = Room "<none>" "<none>" Map.empty

g_roomMap = Map.fromList 
          [("cave", Room "The Cave" "A dank cave" (Map.fromList [(North, "tunnel")]))
          ,("tunnel", Room "The Tunnel" "A low tunnel" (Map.fromList [(South, "cave")]))
          ]

g_itemMap = Map.fromList
          [("table", Item "table" "A small kitchen table" "cave")
          ,("sandbag", Item "sandbag" "A large bag of sand" "cave")
          ,("knife", Item "knife" "A large kitchen kife" "table")
          ]

lookupRoom :: LocationName -> Room
lookupRoom name = case Map.lookup name g_roomMap of
                    Just r -> r
                    Nothing -> error $ "missing room " ++ name

getRoomDescription :: WorldState -> LocationName -> String
getRoomDescription ws roomName = 
    "\n" ++ (description room) ++ itemDesc
    where room = lookupRoom roomName
          items = case Map.lookup roomName $ itemMap ws of
                    Just lst -> lst
                    Nothing -> []
          itemDesc = if null items
                     then ""
                     else "\n\nContains:\n" ++ concatMap (\item -> "\tA " ++ item ++ "\n") items

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
    let oldPi = (playerInfo ws) 
        pi = oldPi { currentRoom = roomName
                   , visitedRooms = combineVisitedRooms roomName (visitedRooms oldPi) } 
        msg = if roomName `elem` (visitedRooms oldPi)
              then summary newRoom
              else getRoomDescription ws roomName in
    (ws { playerInfo = pi }, Just msg)
    where newRoom = lookupRoom roomName

parseLine :: WorldState -> String -> (WorldState, Maybe String)
parseLine ws cmdline = case parseCommand ws cmdline of
                        Look -> (ws, Just $ getRoomDescription ws roomName)
                        Go dir -> case Map.lookup dir (connections room) of
                                    Just roomName -> goToRoom ws roomName
                                    Nothing -> (ws, Just "Can't go that direction")
                        Skip roomName -> goToRoom ws roomName
                        Examine itemName -> (ws, Just $ "It looks like a " ++ itemName)
                        Quit -> error "done"
                        Error msg -> (ws, Just msg)
                      where roomName = currentRoom (playerInfo ws)
                            room = lookupRoom roomName

eval :: WorldState -> String -> IO WorldState
eval ws cmd = do let (newWorldState, maybeMsg) = parseLine ws cmd
                 case maybeMsg of
                   Just msg -> putStrLn $ msg
                   Nothing -> putStrLn ""
                 putStr "> "
                 hFlush stdout
                 inpStr <- getLine
                 eval newWorldState inpStr

generateWorldItemMap :: Map.Map ItemName Item -> Map.Map LocationName [ItemName]
generateWorldItemMap itemMap = 
    Map.fromListWith (++) $ map (\pair -> (startLocation $ snd pair, [fst pair])) $ Map.assocs itemMap

main = 
    do h <- openFile "aliases.txt" ReadMode
       c <- hGetContents h
       let aliasMap = case parseAliases c of
                        Left e -> Map.empty
                        Right r -> Map.fromList r
           ws = WorldState (PlayerInfo "<none>" []) (generateWorldItemMap g_itemMap) aliasMap
       eval ws ":j cave"
    