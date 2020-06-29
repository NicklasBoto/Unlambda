module UserInputHandler
        ( uncomment
        , format
        ) where

-----------------------------------------------------------------------
------------------------ User Input Handler ---------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            Ugly?
            Yes!
        ---*-_-*-_-*-_-*---}

-- Filters away \n, \t, and " "
format :: String -> String
format = filter notSpace . uncomment

notSpace :: Char -> Bool
notSpace '\n' = False
notSpace '\t' = False
notSpace   _  = True

-- Removes full-line and in-line comments.
-- Note that Madore's Unlambda does not support multiline/inline commenting.
uncomment :: String -> String
uncomment []       = []
uncomment ('#':cs) = uncomment (dropUntil (\x -> x /= '\n' && x /= '#') cs)
uncomment (c  :cs) = c : uncomment cs

-- Like preludes dropWhile, but also drops the first element
-- where the predicate fails.
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] =  []
dropUntil p (x:xs)
            | p x = dropUntil p xs
            | otherwise =  xs

