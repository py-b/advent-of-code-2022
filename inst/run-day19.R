x <- readLines("./inst/input19.txt")

p1 <- solve19a(x)
# p2 <- solve19b(x)

# stopifnot(p1 == aoc_solutions$day19a)
# stopifnot(p2 == aoc_solutions$day19b)

blueprints <- parse19(example_data_19())

coll <- Collector$new(blueprints[[1]])

coll$
  print()$
  play_minute("none")$
  print()$
  play_minute("none")$
  print()$
  play_minute("clay")$
  print()$
  play_minute("none")$
  print()$
  play_minute("obsidian")$
  print()$
  play_minute("none")$
  print()
