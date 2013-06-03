module Htads where

import System.IO
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

import Alias
import qualified Util as U

-- string with no whitespace:
-- newtype NoWhitespace = NoWhitespace String; toString (NoWhitespace s) = s; fromString s | all (not . isSpace) s = NoWhiteSpace s | otherwise = error "no"
-- strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

type Word = String
type RoomName = Word
type ItemName = Word
type ItemDesc = String

data Compass = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
               deriving (Ord, Eq, Show, Read)

data Verb = Look | Go Compass | Examine ItemDesc | Get ItemDesc | Inventory | Quit | Skip RoomName | Error String
            deriving (Show)

data ItemAttribute = Fixed | Bulky | Score Int
                     deriving (Show)

type Connection = Map.Map Compass String
type AliasMap = Map.Map String String

data Room = Room {
      summary :: RoomName
    , description :: String
    , connections :: Connection
    } deriving (Show)

data Item = Item {
  itemId :: Word
  , nouns :: [Word]
  , adjectives :: [Word]
  , itemDescription :: String
  , itemAttributes :: [ItemAttribute]
  , startLocation :: String
  } deriving (Show)

itemName :: Item -> ItemName
itemName item = Text.unpack $ Text.strip $ Text.pack $ adjs ++ " " ++ noun
                where adjs = if null $ adjectives item
                                  then ""
                                  else List.intercalate " " $ adjectives item
                      noun = head $ nouns item

data PlayerInfo = PlayerInfo {
      currentRoom :: RoomName
    , visitedRooms :: [RoomName]
    , inventory :: [ItemName]
      } deriving (Show)

type RoomMap = Map.Map RoomName Room
type ItemMap = Map.Map Word Item


data WorldDefinition = WorldDefinition {
  roomMap :: RoomMap
  , itemMap :: ItemMap
  } deriving (Show)

lookupItem :: WorldDefinition -> ItemName -> Item
lookupItem wd name = maybe (error $ "missing item " ++ name) id $ Map.lookup name $ itemMap wd

lookupRoom :: WorldDefinition -> RoomName -> Room
lookupRoom wd name = maybe (error $ "missing room " ++ name) id $ Map.lookup name $ roomMap wd

getItemDescriptions :: Item -> [String]
getItemDescriptions item = [ a ++ " " ++ n | a <- adjectPhrases, n <- nounPhrases ] ++ nounPhrases
    where adjectPhrases = List.map (List.intercalate " ") $ List.concatMap
                         genComb [1..(length adjects)]
          nounPhrases = nouns item
          adjects = adjectives item
          genComb n = U.combinations n adjects


itemByDesc :: [Item] -> String -> Maybe Item
itemByDesc items itemDesc = List.find matches items
    where matches item = itemDesc `elem` getItemDescriptions item

combineVisitedRooms :: RoomName -> [RoomName] -> [RoomName]
combineVisitedRooms name nameList =
    if name `elem` nameList
       then nameList
       else name : nameList


data WorldState = WorldState {
  worldDefinition :: WorldDefinition
  , playerInfo :: PlayerInfo
  , roomItemMap :: Map.Map RoomName [ItemName]
  , aliases :: AliasMap
  } deriving (Show)

makeWorldState :: WorldDefinition -> AliasMap -> WorldState
makeWorldState wd aliasMap = WorldState wd (PlayerInfo "<none>" [] []) (generateWorldItemMap wd) aliasMap
  where
    -- generateWorldItemMap :: WorldDefinition -> Map.Map RoomName [ItemName]
    generateWorldItemMap wd =
      Map.fromListWith (++) $ map (\pair -> (startLocation $ snd pair, [fst pair])) $ Map.assocs $ itemMap wd


getItemsFromRoom :: WorldState -> RoomName -> [Item]
getItemsFromRoom ws roomName = map (lookupItem $ worldDefinition ws) itemNames
    where itemNames = maybe [] id $ Map.lookup roomName $ roomItemMap ws


getRoomDescription :: WorldState -> RoomName -> String
getRoomDescription ws roomName =
    "\n_" ++ (summary room) ++ "_\n" ++ (description room) ++ "\n" ++ itemDesc
    where room = lookupRoom (worldDefinition ws) roomName
          items = getItemsFromRoom ws roomName
          itemDesc = if null items
                     then ""
                     else "You see a " ++ List.intercalate ", a " (map itemName items) ++ "."

parseCompass :: String -> Maybe Compass
parseCompass dir = case dir of
                     "north" -> Just North
                     "northeast" -> Just NorthEast
                     "northwest" -> Just NorthWest
                     "east" -> Just East
                     "west" -> Just West
                     "south" -> Just South
                     "southeast" -> Just SouthEast
                     "southwest" -> Just SouthWest
                     _ -> Nothing

parseCommand :: WorldState -> String -> Verb
parseCommand ws cmdline =
    case cmd of
      "go" -> maybe (Error $ "Unrecognized direction " ++ head rest) Go $ parseCompass $ head rest
      "get" -> Get restStr
      "inventory" -> Inventory
      "look" -> Look
      ":j" -> Skip $ head rest
      "quit" -> Quit
      "examine" -> Examine restStr
      _ -> Error $ "Unrecognized command " ++ cmd
    where wordList = words $ map Char.toLower (translateCommand (aliases ws) cmdline)
          cmd = head wordList
          rest = tail wordList
          restStr = List.intercalate " " rest
          translateCommand aliases cmd = maybe cmd id $ Map.lookup cmd aliases

goToRoom :: WorldState -> RoomName -> (WorldState, Maybe String)
goToRoom ws roomName =
    let oldPi = (playerInfo ws)
        pi = oldPi { currentRoom = roomName
                   , visitedRooms = combineVisitedRooms roomName (visitedRooms oldPi) }
        msg = if roomName `elem` (visitedRooms oldPi)
              then summary newRoom
              else getRoomDescription ws roomName in
    (ws { playerInfo = pi }, Just msg)
    where newRoom = lookupRoom (worldDefinition ws) roomName

tryPickupItem :: WorldState -> ItemDesc -> (WorldState, Maybe String)
tryPickupItem ws itemDesc =
    case maybeItem of
      Just item -> let pi = (playerInfo ws)
                       newPi = pi { inventory = name : (inventory pi) }
                       newIm = Map.update removeItem roomName (roomItemMap ws) in
                   (ws { playerInfo = newPi,
                         roomItemMap = newIm
                       }, Just $ name ++ " picked up." )
                   where removeItem itemList = Just $ List.delete (itemId item) itemList
                         name = itemName item
      Nothing -> (ws, Just $ "There is no " ++ itemDesc ++ " here.")
    where roomName = currentRoom (playerInfo ws)
          items = case Map.lookup roomName $ roomItemMap ws of
                    Just lst -> map (lookupItem $ worldDefinition ws) lst
                    Nothing -> []
          maybeItem = itemByDesc items itemDesc

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
                        Examine itemDesc -> (ws, Just $ "It looks like a " ++ itemDesc)
                        Get itemDesc -> tryPickupItem ws itemDesc
                        Inventory -> showInventory ws
                        Quit -> error "done"
                        Error msg -> (ws, Just msg)
                      where roomName = currentRoom $ playerInfo ws
                            room = lookupRoom (worldDefinition ws) roomName

eval :: WorldState -> String -> IO WorldState
eval ws cmd = do let (newWorldState, maybeMsg) = parseLine ws cmd
                 case maybeMsg of
                   Just msg -> putStrLn $ U.wrap 60 msg
                   Nothing -> putStrLn ""
                 flushStr "> "
                 inpStr <- getLine
                 eval newWorldState inpStr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String ->	IO String
readPrompt prompt = flushStr prompt >> getLine


evalString :: WorldState -> String -> IO (WorldState, Maybe String)
evalString ws expr = return $ parseLine ws expr

evalAndPrint :: WorldState -> String -> IO (WorldState)
evalAndPrint ws expr = do let (newWorldState, maybeMsg) = evalString ws expr
                          case maybeMsg of
                            Just msg -> putStrLn $ U.wrap 60 msg
                            Nothing -> putStrLn "<nothing>"
                          return newWorldState

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m (a)) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
  then return ()
  else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "> ") evalAndPrint

runAdventure :: RoomMap -> ItemMap -> AliasMap -> IO ()
runAdventure roomMap itemMap aliasMap =
  do let ws = makeWorldState (WorldDefinition roomMap itemMap) aliasMap
     ws <- eval ws ":j start"
     putStrLn $ "Finished with score " ++ show 0
