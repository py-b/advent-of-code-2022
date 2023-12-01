x <- readLines("./inst/input25.txt")

p1 <- solve25a(x)

stopifnot(p1 == aoc_solutions$day25a)
