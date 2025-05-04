-- Keep getting Could not find module ‘Data.List.Split’ error, so i am gonna use something else instead of splitOn
-- Not sure is this what we want, so please have a look at it

import Data.Char (isSpace)

parseCSV :: String -> [[String]]
parseCSV = map parseLine . lines
  where 
    parseLine = map trim . split ','
    split c s = case break (==c) s of 
                  (a, _:b) -> a : split c b
                  (a, []) -> [a]
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace