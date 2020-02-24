module UserInputHandler
        ( uncomment
        , format
        ) where

import Data.Char

-----------------------------------------------------------------------
------------------------ User Input Handler ---------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            Ugly?
            Yes!
        ---*-_-*-_-*-_-*---}

format :: String -> String
format = filter (not . isSpace) . uncomment

uncomment :: String -> String
uncomment []       = []
uncomment ('#':cs) = uncomment (dropUntil (\x -> x /= '\n' && x /= '#') cs)
uncomment (c  :cs) = c : uncomment cs

-- because reasons...
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] =  []
dropUntil p (x:xs)
            | p x = dropUntil p xs
            | otherwise =  xs

