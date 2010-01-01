
import System.IO
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Char (isSpace)
import Alias

type RoomName = String
type ItemName = String
type LocationName = String

data Compass = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
             deriving (Ord, Eq, Show, Read)

data Verb = Look | Go Compass | Examine ItemName | Get ItemName | Inventory | Quit | Skip RoomName | Error String
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
    , inventory :: [ItemName]
      } deriving (Show)

data WorldState = WorldState {
      playerInfo :: PlayerInfo
    , itemMap :: Map.Map LocationName [ItemName]
    , aliases :: Aliases
    } deriving (Show)

g_nullRoom = Room "<none>" "<none>" Map.empty

g_roomMap = Map.fromList 
          [("start", Room "Outside cave" "You're standing in the bright sunlight just outside of a large, dark, foreboding cave, which lies to the north. " (Map.fromList [(North, "cave")]))
          ,("cave", Room "Cave" "You're inside a dark and musty cave. Sunlight pours in from a passage to the south." (Map.fromList [(South, "start")]))
          ]

g_itemMap = Map.fromList
          [("pedestal", Item "pedestal" "pedestal" "cave")
          ,("goldSkull", Item "gold skull" "gold skull" "cave")
          ,("table", Item "table" "A small kitchen table" "cave")
          ,("sandbag", Item "sandbag" "A large bag of sand" "cave")
          ,("knife", Item "knife" "A large kitchen kife" "table")
          ]

wrap :: Int -> String -> String
wrap width str = 
    if length str <= width
    then str
    else let chop = words $ take width str 
             len = length chop
             pre = (unwords $ take (len - 1) chop) in
          pre ++ "\n" ++ (wrap width $ dropWhile isSpace $ drop (length pre) str)

lookupRoom :: LocationName -> Room
lookupRoom name = maybe (error $ "missing room " ++ name) id $ Map.lookup name g_roomMap
           
getRoomDescription :: WorldState -> LocationName -> String
getRoomDescription ws roomName = 
    "\n_" ++ (summary room) ++ "_\n" ++ wrap 60 (description room) ++ itemDesc
    where room = lookupRoom roomName
          items = maybe [] (map (\name -> maybe (error "missing item " ++ name) itemName $ Map.lookup name g_itemMap)) $ Map.lookup roomName $ itemMap ws
          itemDesc = if null items
                     then "\n"
                     else "\n\nYou see a " ++ List.intercalate ", a " items ++ ".\n"
--           itemDesc = if null items
--                      then ""
--                      else "\n\nContains:\n" ++ concatMap (\item -> "\tA " ++ item ++ "\n") items

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
translateCommand aliases cmd = maybe cmd id $ Map.lookup cmd aliases

parseCommand :: WorldState ->String -> Verb
parseCommand ws cmdline = 
    case cmd of
      "go" -> maybe (Error $ "Unrecognized direction " ++ obj) Go $ parseCompass obj
      "get" -> Get obj
      "inventory" -> Inventory
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

tryPickupItem :: WorldState -> ItemName -> (WorldState, Maybe String)
tryPickupItem ws itemName = 
    if itemName `elem` items
    then let pi = (playerInfo ws)
             newPi = pi { inventory = itemName : (inventory pi) } in
             -- todo: remove the item from the world itemmap
         (ws { playerInfo = newPi }, Just $ itemName ++ " picked up." )
    else (ws, Just $ "There is no " ++ itemName ++ " here.")
    where roomName = currentRoom (playerInfo ws)
          items = case Map.lookup roomName $ itemMap ws of
                    Just lst -> lst
                    Nothing -> []

showInventory :: WorldState -> (WorldState, Maybe String)
showInventory ws = let pi = (playerInfo ws) in
                   (ws, Just $ "You are carrying:\n  " ++ List.intercalate "\n  " (inventory pi) )

parseLine :: WorldState -> String -> (WorldState, Maybe String)
parseLine ws cmdline = case parseCommand ws cmdline of
                        Look -> (ws, Just $ getRoomDescription ws roomName)
                        Go dir -> case Map.lookup dir (connections room) of
                                    Just roomName -> goToRoom ws roomName
                                    Nothing -> (ws, Just "Can't go that direction")
                        Skip roomName -> goToRoom ws roomName
                        Examine itemName -> (ws, Just $ "It looks like a " ++ itemName)
                        Get itemName -> tryPickupItem ws itemName
                        Inventory -> showInventory ws
                        Quit -> error "done"
                        Error msg -> (ws, Just msg)
                      where roomName = currentRoom $ playerInfo ws
                            room = lookupRoom roomName

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

eval :: WorldState -> String -> IO WorldState
eval ws cmd = do let (newWorldState, maybeMsg) = parseLine ws cmd
                 case maybeMsg of
                   Just msg -> putStrLn $ msg
                   Nothing -> putStrLn ""
                 flushStr "> "
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
           ws = WorldState (PlayerInfo "<none>" [] []) (generateWorldItemMap g_itemMap) aliasMap
       eval ws ":j start"
    

Nothing >>? _ = Nothing
Just v  >>? f = f v
