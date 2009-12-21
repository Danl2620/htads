
import System.IO
import qualified Data.Char as Char
import qualified Data.Map as Map

type RoomName = String
type ItemName = String

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
      -- inventory and whatnot
      } deriving (Show)


roomMap = Map.fromList 
          [("cave", Room "The Cave" "A dank cave" (Map.fromList [(North, "tunnel")]))
          ,("tunnel", Room "The Tunnel" "A low tunnel" (Map.fromList [(South, "cave")]))
          ]

itemMap = Map.fromList
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

parseCompass :: String -> Compass
parseCompass dir = case unAbbrevCompass dir of
                     "north" -> North
                     "northeast" -> NorthEast
                     "northwest" -> NorthWest
                     "east" -> East
                     "west" -> West
                     "south" -> South
                     "southeast" -> SouthEast
                     "southwest" -> SouthWest
                     _ -> error $ "bad compass " ++ dir

parseCommand :: String -> Verb
parseCommand cmdline = case cmd of
                         "go" -> Go $ parseCompass obj
                         "look" -> Look
                         ":j" -> Skip obj
                         "quit" -> Quit
                         "examine" -> Examine obj
                         _ -> Error $ "Unrecognized command " ++ cmd
    where wordList = words $ map Char.toLower cmdline
          cmd = head wordList
          obj = head $ tail wordList

goToRoom pi roomName = case Map.lookup roomName roomMap of
                      Just newRoom -> (pi { currentRoom = newRoom }, Nothing)
                      Nothing -> error $ "missing room " ++ roomName

parseLine :: PlayerInfo -> String -> (PlayerInfo, Maybe String)
parseLine pi cmdline = case parseCommand cmdline of
                        Look -> (pi, Just $ description room)
                        Go dir -> case Map.lookup dir (connections room) of
                                    Just roomName -> goToRoom pi roomName
                                    Nothing -> (pi, Just "Can't go that direction")
                        Skip roomName -> goToRoom pi roomName
                        Examine itemName -> (pi, Just $ "It looks like a " ++ itemName)
                        Quit -> error "done"
                        Error msg -> (pi, Just msg)
                      where room = currentRoom pi

visitRoom :: PlayerInfo -> IO PlayerInfo
visitRoom playerInfo = do putStr $ (name (currentRoom playerInfo)) ++ "\n> "
                          hFlush stdout
                          inpStr <- getLine
                          let (newPlayerInfo, maybeMsg) = parseLine playerInfo inpStr
                          case maybeMsg of
                            Just msg -> putStrLn $ msg
                            Nothing -> putStrLn ""
                          visitRoom newPlayerInfo


main = 
    do isTerminal <- hIsTerminalDevice stdout
       print isTerminal
       case (Map.lookup "cave" roomMap) of
         Just room -> visitRoom $ PlayerInfo room
         Nothing -> error "Asdf"
