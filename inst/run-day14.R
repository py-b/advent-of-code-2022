x <- readLines("./inst/input14.txt")

(p1 <- solve14a(x))
# p2 <- solve14b(x)

stopifnot(p1 == aoc_solutions$day14a)
# stopifnot(p2 == aoc_solutions$day14b)

# Visualizing caves ------------------------------------------------------------

cave_part1 <- Cave$new(x, infinite_floor = FALSE)
while (!cave_part1$full) cave_part1$add_sand()
sink("../day14_cave_part1.txt")
cave_part1
sink()
