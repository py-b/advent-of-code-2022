x <- readLines("./inst/input10.txt")

(s1 <- solve10a(x))
s2 <- solve10b(x)

stopifnot(s1 == aoc_solutions$day10a)
stopifnot(s2 == aoc_solutions$day10b)
