> module CharTest where

Time to import some Haskell standard libraries, and of course the function 'caesar' from the Char module

> import Data.List
> import Data.Char
> import Char (caesar)

Define the type of the test set; this is a list of inputs and associated allowable outputs.

> type TestSet a b = [(a, [b])]

The following function compares the output of the function to the outcomes (within some tolerances),
and displays the first item that didn't match.

> testme :: IO ()
> testme = putStrLn $ if 
>                       null failed 
>                     then
>                       "Seems to work!" 
>                     else
>                       "caesar " ++ show n ++ " \"" ++ s ++ "\""
>                       ++ ": got '" ++ caesar n s
>                       ++ "', but expected '" ++ intercalate "' or '" answers ++ "'"
>   where 
>     failed = findFailures (\(n',s')->caesar n' s') testSet
>     ((n,s), answers) = head failed

It uses the function findFailures, which is defined below: it filters out all the non-matching entries of
the test set---i.e., those for which none of the allowable answers matches those computed by the function.
The match is loose: case distinction is ignored (see the "map toUpper").

> findFailures :: (a -> String) -> TestSet a String -> TestSet a String
> findFailures f = filter (\(n, answers)->all (not.match (f n)) answers)
>   where match s s' = map toUpper s == map toUpper s'

And finally, the test set itself. The last case tests how you handle other alphabets; you do not have to be very precise here.

> testSet :: TestSet (Int,String) String
> testSet = [((13,"ROTATE"), ["EBGNGR"])
>           ,(( 5,"SECRET"), ["XJHWJY"])
>           ,((17,"ALERTSTATUS"), ["RCVIKJKRKLJ"])
>           ,((17,"ALERT STATUS"), ["RCVIK JKRKLJ"])
>           ,((13,"ROT13"), ["EBG13"])
>           ,((17,"SeCrET//Noforn"), ["JVTIVK//EFWFIE"])
>           ,(( 2,"α is alpha, ω is omega"), ["Α KU CNRJC, Ω KU QOGIC", "α KU CNRJC, ω KU QOGIC"])
>           ]

                                               ^ Note, this is not the letter "A", it is the letter "Α".
