# library(adventofcode22)
x <- readLines("./inst/input05.txt")

(p1 <- f05a(x))
(p2 <- f05b(x))

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)

# microbenchmark::microbenchmark(
#   f05a(x),
#   f05a_old(x)
# )
#
# microbenchmark::microbenchmark(
#   f05b(x),
#   f05b_old(x)
# )
