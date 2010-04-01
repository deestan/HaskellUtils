module ListEx (endsWith) where

import List

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith x y = y `elem` (reverse . tails) x
