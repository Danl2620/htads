module Htads where

import System.IO
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

import Alias

-- string with no whitespace:
-- newtype NoWhitespace = NoWhitespace String; toString (NoWhitespace s) = s; fromString s | all (not . isSpace) s = NoWhiteSpace s | otherwise = error "no"

-- strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

type Word = String
type RoomName = Word
type ItemName = Word

data Compass = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
               deriving (Ord, Eq, Show, Read)

data Verb = Look | Go Compass | Examine ItemName | Get ItemName | Inventory | Quit | Skip RoomName | Error String
            deriving (Show)

data ItemAttribute = Fixed | Bulky
                     deriving (Show)

type Connection = Map.Map Compass String
type Aliases = Map.Map String String

data Room = Room {
      summary :: RoomName
    , description :: String
    , connections :: Connection
    } deriving (Show)

data Item = Item {
      nouns :: [Word]
    , adjectives :: [Word]
    , itemDescription :: String
    , itemAttributes :: [ItemAttribute]
    , startLocation :: String
      } deriving (Show)

itemName :: Item -> ItemName
itemName item = Text.unpack $ Text.strip $ Text.pack $ List.intercalate " " [adjective, noun]
                where adjective = if null $ adjectives item
                                  then ""
                                  else head $ adjectives item
                      noun = if null $ nouns item
                             then ""
                             else head $ nouns item

data PlayerInfo = PlayerInfo {
      currentRoom :: RoomName
    , visitedRooms :: [RoomName]
    , inventory :: [ItemName]
      } deriving (Show)

data WorldState = WorldState {
      playerInfo :: PlayerInfo
    , itemMap :: Map.Map RoomName [ItemName]
    , aliases :: Aliases
    } deriving (Show)

g_nullRoom = Room "<none>" "<none>" Map.empty

g_roomMap = Map.fromList
          [("start", Room "Outside cave" "You're standing in the bright sunlight just outside of a large, dark, foreboding cave, which lies to the north. " (Map.fromList [(North, "cave")]))
          ,("cave", Room "Cave" "You're inside a dark and musty cave. Sunlight pours in from a passage to the south." (Map.fromList [(South, "start")]))
          ]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

g_itemMap = Map.fromList
          [("pedestal", Item ["pedestal"] [] "pedestal" [Fixed] "cave")
          ,("skull", Item ["skull"] ["gold"] "gold skull" [] "cave")
          ,("table", Item ["table"] ["small"] "A small kitchen table" [Bulky] "cave")
          ,("largeSandbag", Item ["sandbag", "bag"] ["large"] "A large bag of sand" [Bulky] "cave")
          ,("smallSandbag1", Item ["sandbag", "bag"] ["small", "red"] "A small red bag of sand" [] "cave")
          ,("smallSandbag2", Item ["sandbag", "bag"] ["small", "blue"] "A small blue bag of sand" [] "cave")
          ,("Knife", Item ["knife"] ["large"] "A large kitchen kife" [] "table")
          ]

wrap :: Int -> String -> String
wrap width str =
    if length str <= width
    then str
    else let chop = words $ take width str
             len = length chop
             pre = (unwords $ take (len - 1) chop) in
          pre ++ "\n" ++ (wrap width $ dropWhile Char.isSpace $ drop (length pre) str)

lookupItem :: ItemName -> Item
lookupItem name = maybe (error $ "missing item " ++ name) id $ Map.lookup name g_itemMap

lookupRoom :: RoomName -> Room
lookupRoom name = maybe (error $ "missing room " ++ name) id $ Map.lookup name g_roomMap

getItemDescriptions :: Item -> [String]
getItemDescriptions item = [ a ++ " " ++ n | a <- adjectPhrases, n <- nounPhrases ]
    where adjectPhrases = List.map (List.intercalate " ") $ List.concatMap
                         genComb [1..(length adjects)]
          nounPhrases = nouns item
          adjects = adjectives item
          genComb n = combinations n adjects


getItemsFromRoom :: WorldState -> RoomName -> [Item]
getItemsFromRoom ws roomName = map lookupItem itemNames
    where itemNames = maybe [] id $ Map.lookup roomName $ itemMap ws

-- maybe [] (map (\name -> maybe (error "missing item " ++ name) id $ Map.lookup name g_itemMap)) id $ Map.lookup roomName $ itemMap ws

getRoomDescription :: WorldState -> RoomName -> String
getRoomDescription ws roomName =
    "\n_" ++ (summary room) ++ "_\n" ++ wrap 60 (description room) ++ itemDesc
    where room = lookupRoom roomName
          items = getItemsFromRoom ws roomName
          itemDesc = if null items
                     then "\n"
                     else "\n\nYou see a " ++ List.intercalate ", a " (map itemName items) ++ ".\n"
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

parseItemName :: WorldState -> [Word] -> ItemName
parseItemName ws itemDesc = undefined
-- parseItemName ws itemDesc = maybe (error $ "unknown item") id $ List.find matches $ map getItemDescriptions items
--      where currentRoomName = currentRoom $ playerInfo ws
--            items = getItemsFromRoom ws currentRoomName
--            matches name = name == itemDesc

parseCommand :: WorldState -> String -> Verb
parseCommand ws cmdline =
    case cmd of
      "go" -> maybe (Error $ "Unrecognized direction " ++ head rest) Go $ parseCompass $ head rest
      "get" -> Get $ parseItemName ws rest
      "inventory" -> Inventory
      "look" -> Look
      ":j" -> Skip $ head rest
      "quit" -> Quit
      "examine" -> Examine $ head rest
      _ -> Error $ "Unrecognized command " ++ cmd
    where wordList = words $ map Char.toLower (translateCommand (aliases ws) cmdline)
          cmd = head wordList
          rest = tail wordList

combineVisitedRooms :: RoomName -> [RoomName] -> [RoomName]
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
    case List.find itemMatches items of
      Just item -> let pi = (playerInfo ws)
                       newPi = pi { inventory = itemName : (inventory pi) }
                       newIm = Map.update removeItem roomName (itemMap ws) in
                   (ws { playerInfo = newPi,
                         itemMap = newIm
                       }, Just $ itemName ++ " picked up." )
      Nothing -> (ws, Just $ "There is no " ++ itemName ++ " here.")
    where roomName = currentRoom (playerInfo ws)
          items = case Map.lookup roomName $ itemMap ws of
                    Just lst -> lst
                    Nothing -> []
          itemMatches item = item == itemName
          removeItem itemList = Just $ List.delete itemName itemList

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
                        Examine itemName -> (ws, Just $ "It looks like a " ++ itemName) -- List.intercalate " " itemName)
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

generateWorldItemMap :: Map.Map ItemName Item -> Map.Map RoomName [ItemName]
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
