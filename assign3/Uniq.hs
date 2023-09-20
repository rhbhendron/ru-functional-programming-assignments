module Uniq where

import Data.List

--3.3.5
uniq :: (Eq a) => [a] -> [a]
uniq xs = map head $ group xs