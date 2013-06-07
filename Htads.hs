module Htads where

import System.IO
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe

-- local module
import Alias
import qualified Util as U

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

type RoomMap = Map.Map RoomName Room
type ItemMap = Map.Map Word Item

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

itemIsFixed :: Item -> Bool
itemIsFixed = Maybe.isJust . List.find isFixed . itemAttributes
  where isFixed Fixed = True
        isFixed _ = False

getItemScore :: Item -> Int
getItemScore = maybe 0 exScore . List.find isScore . itemAttributes
    where isScore (Score _) = True
          isScore _ = False
          exScore (Score n) = n

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
    where wordList = words $ map Char.toLower $ translateCommand (aliases ws) cmdline
          cmd = translateCommand (aliases ws) $ head wordList
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

examineItem :: WorldState -> ItemDesc -> (WorldState, Maybe String)
examineItem ws itemDesc =
    case maybeItem of
      Just item -> (ws, Just $ "It looks like a " ++ itemDesc)
      Nothing -> (ws, Just $ "There is no " ++ itemDesc ++ " here.")
    where roomName = currentRoom (playerInfo ws)
          roomItems = case Map.lookup roomName $ roomItemMap ws of
                    Just lst -> map (lookupItem $ worldDefinition ws) lst
                    Nothing -> []
          playerItems = map (lookupItem (worldDefinition ws)) (inventory (playerInfo ws))
          maybeItem = itemByDesc (roomItems ++ playerItems) itemDesc

tryPickupItem :: WorldState -> ItemDesc -> (WorldState, Maybe String)
tryPickupItem ws itemDesc =
    case maybeItem of
      Just item ->
        if itemIsFixed item
        then (ws, Just $ desc ++ " cannot be picked up.")
        else let pi = (playerInfo ws)
                 newPi = pi { inventory = iId : (inventory pi) }
                 newIm = Map.update removeItem roomName (roomItemMap ws) in
             (ws { playerInfo = newPi,
                   roomItemMap = newIm
                 }, Just $ desc ++ " picked up." )
        where removeItem itemList = Just $ List.delete iId itemList
              name = itemName item
              desc = itemDescription item
              iId = itemId item
      Nothing -> (ws, Just $ "There is no " ++ itemDesc ++ " here.")
    where roomName = currentRoom (playerInfo ws)
          items = case Map.lookup roomName $ roomItemMap ws of
                    Just lst -> map (lookupItem $ worldDefinition ws) lst
                    Nothing -> []
          maybeItem = itemByDesc items itemDesc

showInventory :: WorldState -> (WorldState, Maybe String)
showInventory ws = (ws, Just $ "You are carrying:\n  " ++ List.intercalate "\n  " items )
    where items = map
                  (itemDescription . lookupItem (worldDefinition ws))
                  (inventory (playerInfo ws))

evalString :: WorldState -> String -> (WorldState, Maybe String)
evalString ws cmdline =
  case parseCommand ws cmdline of
    Look -> (ws, Just $ getRoomDescription ws roomName)
    Go dir -> case Map.lookup dir (connections room) of
      Just roomName -> goToRoom ws roomName
      Nothing -> (ws, Just "You can't go that direction")
    Skip roomName -> goToRoom ws roomName
    Examine itemDesc -> examineItem ws itemDesc
    Get itemDesc -> tryPickupItem ws itemDesc
    Inventory -> showInventory ws
    Quit -> error "done"
    Error msg -> (ws, Just msg)
  where roomName = currentRoom $ playerInfo ws
        room = lookupRoom (worldDefinition ws) roomName

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String ->	IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: WorldState -> String -> IO (WorldState)
evalAndPrint ws expr = do let (nws, maybeMsg) = evalString ws expr
                          case maybeMsg of
                            Just msg -> putStrLn $ U.wrap 60 msg
                            Nothing -> putStrLn "<nothing>"
                          return nws

until_ :: Monad m => (a -> Bool) -> m a -> (s -> a -> m s) -> s -> m s
until_ pred prompt action state =
  do result <- prompt
     if pred result
       then return state
       else action state result >>= until_ pred prompt action

runRepl :: WorldState -> IO WorldState
runRepl = until_  (== "quit") (readPrompt "> ") evalAndPrint

getScore :: WorldState -> Int
getScore ws =
  sum $ map (getItemScore . (lookupItem (worldDefinition ws))) (inventory (playerInfo ws))

runAdventure :: RoomMap -> ItemMap -> AliasMap -> IO WorldState
runAdventure roomMap itemMap aliasMap =
  do let ws = makeWorldState (WorldDefinition roomMap itemMap) aliasMap
     evalAndPrint ws ":j start" >>= runRepl
