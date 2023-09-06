> module SayTest where

Time to import some Haskell standard libraries, and of course the function 'say' from the Say module

> import Data.List
> import Say (say)

Define the type of the test set; this is a list of inputs and associated allowable outputs.

> type TestSet = [(Integer, [String])]

The following function compares the output of the 'say' function to the outcomes (within some tolerances),
and displays the first item that didn't match.

> testme :: IO ()
> testme = putStrLn $ if 
>                       null failed 
>                     then
>                       "Seems to work!" 
>                     else 
>                       show n ++ ": got '" ++ say n ++ "', but expected '" ++ intercalate "' or '" answers ++ "'"
>   where 
>     failed = findFailures say testSet
>     (n, answers) = head failed

It uses the function findFailures, which is defined below: it filters out all the non-matching entries of
the test set---i.e., those for which none of the allowable answers matches those computed by the function.
The match is loose: the word "and" is sometimes ignored.

> findFailures :: (Integer -> String) -> TestSet -> TestSet
> findFailures f = filter (\(n, answers)->all (not.match (f n)) answers)
>   where match s s' = smudge s == smudge s'
>         smudge     = replace "hundred and" "hundred" . replace "thousand and" "thousand"

This function replaces all occurences of a substring with another string (it is used above);
e.g. replace "o" "oo" "foobar" = "foooobar"

> replace :: String -> String -> String -> String
> replace sub rep = go
>  where n = length sub
>        go xs
>          | sub `isPrefixOf` xs = rep ++ go (drop n xs)
>          | length xs <= n      = xs
>          | otherwise           = take 1 xs ++ go (tail xs)

And finally, the test set itself:

> testSet :: TestSet
> testSet = [(12,["twelve"])
>           ,(10,["ten"])
>           ,(11,["eleven"])
>           ,(17,["seventeen"])
>           ,(18,["eighteen"])
>           ,(19,["nineteen"])
>           ,(14,["fourteen"])
>           ,(16,["sixteen"])
>           ,(13,["thirteen"])
>           ,(15,["fifteen"])
>           ,(52,["fifty two"])
>           ,(42,["forty two","the answer to life, the universe, and everything"])
>           ,(20,["twenty"])
>           ,(61,["sixty one"])
>           ,(27,["twenty seven"])
>           ,(48,["forty eight"])
>           ,(67,["sixty seven"])
>           ,(29,["twenty nine"])
>           ,(39,["thirty nine"])
>           ,(30,["thirty"])
>           ,(592,["five hundred and ninety two"])
>           ,(222,["two hundred and twenty two"])
>           ,(300,["three hundred"])
>           ,(821,["eight hundred and twenty one"])
>           ,(127,["one hundred and twenty seven"])
>           ,(588,["five hundred and eighty eight"])
>           ,(447,["four hundred and forty seven"])
>           ,(209,["two hundred and nine"])
>           ,(219,["two hundred and nineteen"])
>           ,(650,["six hundred and fifty"])
>           ,(1074,["one thousand and seventy four"])
>           ,(2392,["two thousand three hundred and ninety two","twenty three hundred and ninety two"])
>           ,(5622,["five thousand six hundred and twenty two","fifty six hundred and twenty two"])
>           ,(1200,["one thousand two hundred","twelve hundred"])
>           ,(8921,["eight thousand nine hundred and twenty one","eighty nine hundred and twenty one"])
>           ,(5527,["five thousand five hundred and twenty seven","fifty five hundred and twenty seven"])
>           ,(7788,["seven thousand seven hundred and eighty eight","seventy seven hundred and eighty eight"])
>           ,(9447,["nine thousand four hundred and forty seven","ninety four hundred and forty seven"])
>           ,(9209,["nine thousand two hundred and nine","ninety two hundred and nine"])
>           ,(5619,["five thousand six hundred and nineteen","fifty six hundred and nineteen"])
>           ,(7850,["seven thousand eight hundred and fifty","seventy eight hundred and fifty"])
>           ,(10005,["ten thousand and five"])
>           ,(209392,["two hundred and nine thousand three hundred and ninety two"])
>           ,(671622,["six hundred and seventy one thousand six hundred and twenty two"])
>           ,(919200,["nine hundred and nineteen thousand two hundred"])
>           ,(980921,["nine hundred and eighty thousand nine hundred and twenty one"])
>           ,(57000,["fifty seven thousand"])
>           ,(203527,["two hundred and three thousand five hundred and twenty seven"])
>           ,(205788,["two hundred and five thousand seven hundred and eighty eight"])
>           ,(324447,["three hundred and twenty four thousand four hundred and forty seven"])
>           ,(657209,["six hundred and fifty seven thousand two hundred and nine"])
>           ,(59619,["fifty nine thousand six hundred and nineteen"])
>           ,(889850,["eight hundred and eighty nine thousand eight hundred and fifty"])]
