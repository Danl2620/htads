module Htads where

import System.IO
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import GHC.Generics

-- local module
import Alias
import qualified Util as U

type TextWord = String
type RoomName = TextWord
type ItemName = TextWord
type ItemDesc = String
type WordSet = Set.Set TextWord

data Compass = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
               deriving (Ord, Eq, Show, Read, Generic)

data Verb = Look | Go Compass | Examine ItemDesc | Get ItemDesc | Inventory | Quit | Skip RoomName | Error String
            deriving (Show)

data Result = Message String | Start | End
  deriving (Show, Eq)

data ItemAttribute = Fixed | Bulky | Score Int
  deriving (Show, Generic)

type Connection = Map.Map Compass String
type AliasMap = Map.Map String String

type RoomMap = Map.Map RoomName Room
type ItemMap = Map.Map TextWord Item

data Room = Room {
  summary :: RoomName
  , description :: String
  , connections :: Connection
  } deriving (Show, Generic)

data Item = Item {
  itemId :: TextWord
  , nouns :: [TextWord]
  , adjectives :: [TextWord]
  , itemDescription :: String
  , itemAttributes :: [ItemAttribute]
  , startLocation :: String
  } deriving (Show, Generic)

data Object = ObjectRoom Room | ObjectItem Item
              deriving (Show)

itemName :: Item -> ItemName
itemName item = Text.unpack $ Text.strip $ Text.pack $ adjs ++ " " ++ noun
                where adjs = if null $ adjectives item
                                  then ""
                                  else List.intercalate " " $ adjectives item
                      noun = head $ nouns item

data PlayerInfo = PlayerInfo {
  currentRoom :: RoomName
  , visitedRooms :: WordSet
  , inventory :: WordSet
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

data WorldState = WorldState {
  worldDefinition :: WorldDefinition
  , playerInfo :: PlayerInfo
  , roomItemMap :: Map.Map RoomName [ItemName]
  , aliases :: AliasMap
  } deriving (Show)

makeWorldState :: WorldDefinition -> AliasMap -> WorldState
makeWorldState wd aliasMap = WorldState wd (PlayerInfo "<none>" Set.empty Set.empty) (generateWorldItemMap wd) aliasMap
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
          cmd = head wordList
          rest = tail wordList
          restStr = List.intercalate " " rest
          translateCommand aliases cmd = maybe cmd id $ Map.lookup cmd aliases

goToRoom :: WorldState -> RoomName -> (WorldState, Result)
goToRoom ws roomName =
    let oldPi = (playerInfo ws)
        pi = oldPi { currentRoom = roomName
                   , visitedRooms = Set.insert roomName (visitedRooms oldPi) }
        msg = if roomName `Set.member` (visitedRooms oldPi)
              then summary newRoom
              else getRoomDescription ws roomName in
    (ws { playerInfo = pi }, Message msg)
    where newRoom = lookupRoom (worldDefinition ws) roomName

examineItem :: WorldState -> ItemDesc -> (WorldState, Result)
examineItem ws itemDesc =
    case maybeItem of
      Just item -> (ws, Message $ "It looks like a " ++ itemDesc)
      Nothing -> (ws, Message $ "There is no " ++ itemDesc ++ " here.")
    where roomName = currentRoom (playerInfo ws)
          roomItems = case Map.lookup roomName $ roomItemMap ws of
                    Just lst -> map (lookupItem $ worldDefinition ws) lst
                    Nothing -> []
          playerItems = map (lookupItem (worldDefinition ws)) $ Set.elems (inventory (playerInfo ws))
          maybeItem = itemByDesc (roomItems ++ playerItems) itemDesc

tryPickupItem :: WorldState -> ItemDesc -> (WorldState, Result)
tryPickupItem ws itemDesc =
    case maybeItem of
      Just item ->
        if itemIsFixed item
        then (ws, Message $ desc ++ " cannot be picked up.")
        else let pi = (playerInfo ws)
                 newPi = pi { inventory = Set.insert iId (inventory pi) }
                 newIm = Map.update removeItem roomName (roomItemMap ws) in
             (ws { playerInfo = newPi,
                   roomItemMap = newIm
                 }, Message $ desc ++ " picked up." )
        where removeItem itemList = Just $ List.delete iId itemList
              name = itemName item
              desc = itemDescription item
              iId = itemId item
      Nothing -> (ws, Message $ "There is no " ++ itemDesc ++ " here.")
    where roomName = currentRoom (playerInfo ws)
          items = case Map.lookup roomName $ roomItemMap ws of
                    Just lst -> map (lookupItem $ worldDefinition ws) lst
                    Nothing -> []
          maybeItem = itemByDesc items itemDesc

showInventory :: WorldState -> (WorldState, Result)
showInventory ws = (ws, Message $ "You are carrying:\n  " ++ List.intercalate "\n  " items )
    where items =
            Set.elems $ Set.map
            (itemDescription . lookupItem (worldDefinition ws))
            (inventory (playerInfo ws))

evalString :: WorldState -> String -> (WorldState, Result)
evalString ws cmdline =
  case parseCommand ws cmdline of
    Look -> (ws, Message $ getRoomDescription ws roomName)
    Go dir -> case Map.lookup dir (connections room) of
      Just roomName -> goToRoom ws roomName
      Nothing -> (ws, Message "You can't go that direction")
    Skip roomName -> goToRoom ws roomName
    Examine itemDesc -> examineItem ws itemDesc
    Get itemDesc -> tryPickupItem ws itemDesc
    Inventory -> showInventory ws
    Quit -> (ws, End)
    Error msg -> (ws, Message msg)
  where roomName = currentRoom $ playerInfo ws
        room = lookupRoom (worldDefinition ws) roomName

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: (WorldState, Result) -> String -> IO (WorldState, Result)
evalAndPrint (ws, res) expr =
  do let (nws, result) = evalString ws expr
     case result of
       Message msg -> putStrLn $ U.wrap 60 msg
       End -> putStrLn "Exiting the game!"
     return (nws, result)

until_ :: Monad m => (s -> Bool) -> m a -> (s -> a -> m s) -> s -> m s
until_ pred prompt action state =
  do cmdline <- prompt
     res <- action state cmdline
     if pred res
       then return state
       else until_ pred prompt action res

done :: (WorldState, Result) -> Bool
done (ws, res) = res == End

runRepl :: (WorldState, Result) -> IO (WorldState, Result)
runRepl = until_ done (readPrompt "> ") evalAndPrint

getScore :: WorldState -> Int
getScore ws =
  sum $ Set.elems $ Set.map (getItemScore . (lookupItem (worldDefinition ws))) (inventory (playerInfo ws))

runAdventure :: RoomMap -> ItemMap -> AliasMap -> IO (WorldState, Result)
runAdventure roomMap itemMap aliasMap =
  do let ws = makeWorldState (WorldDefinition roomMap itemMap) aliasMap
     evalAndPrint (ws, Start) ":j start" >>= runRepl
