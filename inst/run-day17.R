x <- readLines("./inst/input17.txt")

(p1 <- solve17a(x))
# (p2 <- solve17b(x))

# stopifnot(p1 == aoc_solutions$day17a)
# stopifnot(p2 == aoc_solutions$day17b)


# part 2 ------------------------------------------------------------------

# search if when goes back to starting point ++++++++++++++++++++++
# repeats for example data at shapes 30 + 0:25 * 35
# repeat pattern
#    #......
#    #......
#    #.#....
#    #.#....
#    ####...
#    ..#####

x <- example_data_17()
x <- readLines("./inst/input17.txt")

push_seq <- parse17(x)

chamber <- Chamber$new(push_seq)

repeat {
  shape <- shape_n(chamber$shape_count)
  chamber$drop(shape)
  # if (chamber$height() > 5 &&
  #     identical(
  #       chamber$grid[1:5, ],
  #       rbind(
  #         c(rep(F, 6), T),
  #         c(rep(F, 6), T),
  #         c(F, F, F, F, T, T, T),
  #         c(rep(F, 5), T, F),
  #         rep(T, 7)
  #       )
  #     )) {
  if (chamber$shape_count %in% (25 * 7:10 - 2)) {
    message("shape : ", shape)
    message("rock : ", chamber$shape_count)
    message("height : ", chamber$height())
    print(chamber)
    print(cumsum(chamber$push_seq[1:30]) |> paste(collapse = ""))
  }
  if (chamber$shape_count > 3000) break

}


# deduce height with repeat pattern +++++++++++++++++++++++++++++++

options(scipen = 10000)

# example
# start <- 30
# period <- 35
# start_height <- 51
# period_height <- 53

# input
start <- 223
period <- 1730
start_height <- 354
period_height <- 2644

target <- 1000000000000
times <- (target - start) %/% period

remaining <- target - (times * period + start)

# height from start to remaining
ch2 <- Chamber$new(push_seq)
while (ch2$shape_count < start + remaining) {
  shape <- shape_n(ch2$shape_count)
  ch2$drop(shape)
  if (ch2$shape_count == start) height0 <- ch2$height()
}
remaining_height <- ch2$height() - height0

times  * period_height + start_height + remaining_height
