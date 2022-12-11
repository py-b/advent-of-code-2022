x <- readLines("./inst/input11.txt")

(s1 <- solve11a(x))

options(scipen = 100000)
(s2 <- solve11b(x))

stopifnot(s1 == aoc_solutions$day11a)
stopifnot(s2 == aoc_solutions$day11b)
