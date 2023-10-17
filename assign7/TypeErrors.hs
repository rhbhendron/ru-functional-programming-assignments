maxlist [] = 0
maxlist xs = foldl max xs

prepend xs [] = xs
prepend xs ys = xs : ys

double f = f f
