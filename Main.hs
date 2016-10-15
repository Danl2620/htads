module Main where

import System.IO
import qualified Data.Map as Map

import Htads
import Alias

g_roomMap :: RoomMap
g_roomMap = Map.fromList
          [("start", Room "Outside cave" "You're standing in the bright sunlight just outside of a large, dark, foreboding cave, which lies to the north. " (Map.fromList [(North, "cave")]))
          ,("cave", Room "Cave" "You're inside a dark and musty cave. Sunlight pours in from a passage to the south." (Map.fromList [(South, "start")]))
          ]

g_itemMap :: ItemMap
g_itemMap = Map.fromList
          [("pedestal", Item "pedestal" ["pedestal"] [] "A pedestal" [Fixed] "cave")
          ,("skull", Item "skull" ["skull"] ["gold"] "A gold skull" [Score 10] "cave")
          ,("table", Item "table" ["table"] ["small"] "A small kitchen table" [Bulky] "cave")
          ,("largeSandbag", Item "largeSandbag" ["sandbag", "bag"] ["large"] "A large bag of sand" [Bulky] "cave")
          ,("smallSandbag1", Item "smallSandbag1" ["sandbag", "bag"] ["small", "red"] "A small red bag of sand" [] "cave")
          ,("smallSandbag2", Item "smallSandbag2" ["sandbag", "bag"] ["small", "blue"] "A small blue bag of sand" [] "cave")
          --,("knife", Item "knife" ["knife"] ["large"] "A large kitchen kife" [] "table")
          ]




-- "item knife {}"
-- "item knife { nouns [knife] adjectives [large] desc \"A large kitchen kife\" startAt table }"
-- "item knife { nouns [knife] adjectives [large] desc \"A large kitchen kife\" startAt table }"
-- "item knife { nouns [knife] adjectives [large] desc \"A large kitchen kife\" startAt table }"
-- "item knife { nouns [knife] adjectives [large] desc \"A large kitchen kife\" startAt table }"


main :: IO ()
main =
  do h <- openFile "aliases.txt" ReadMode
     c <- hGetContents h
     let aliases = case parseAliases c of
           Left e -> Map.empty
           Right r -> Map.fromList r
     (endWs, res) <- runAdventure g_roomMap g_itemMap aliases
     putStrLn $ "Finished with score " ++ show (getScore endWs)
