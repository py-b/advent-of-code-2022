#' Day 18: Boiling Boulders
#'
#' [Boiling Boulders](https://adventofcode.com/2022/day/18)
#'
#' @name day18
#' @rdname day18
#' @param x some data
#' @export
#' @examples
#' solve18a(example_data_18())
#' solve18b(example_data_18())

solve18a <- function(x) {

  cubes <- parse18(x)

  res <- 0

  max_x <- dim(cubes)[1]
  max_y <- dim(cubes)[2]
  max_z <- dim(cubes)[3]

  for (x in 1:max_x)
    for (y in 1:max_y)
      for (z in 1:max_z) {
        if (cubes[x,y,z] == 1) {
          if (x == 1     || cubes[x - 1, y    , z    ] == 0) res <- res + 1
          if (x == max_x || cubes[x + 1, y    , z    ] == 0) res <- res + 1
          if (y == 1     || cubes[x    , y - 1, z    ] == 0) res <- res + 1
          if (y == max_y || cubes[x    , y + 1, z    ] == 0) res <- res + 1
          if (z == 1     || cubes[x    , y    , z - 1] == 0) res <- res + 1
          if (z == max_z || cubes[x    , y    , z + 1] == 0) res <- res + 1
        }
      }

  res

}

#' @rdname day18
#' @export

solve18b <- function(x) {

}

# parse -------------------------------------------------------------------

parse18 <- function(x) {

  res_num <-
    stringr::str_match(x, "(\\d+),(\\d+),(\\d+)")[, -1] |>
    apply(1:2, as.integer)

  res_num <- res_num + 1 # because coords possibly start at 0
  coords_plus1 <- apply(res_num, 1, paste, collapse =",")

  dim_ranges <- apply(res_num, 2, range)
  res <- array(0L, dim_ranges[2, ])
  for (coords in coords_plus1) eval(str2lang(sprintf("res[%s] <- 1L", coords)))

  res

}


# Example -----------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export

example_data_18 <- function(example = 1) {
  l <- list(
    a = c(
      "2,2,2",
      "1,2,2",
      "3,2,2",
      "2,1,2",
      "2,3,2",
      "2,2,1",
      "2,2,3",
      "2,2,4",
      "2,2,6",
      "1,2,5",
      "3,2,5",
      "2,1,5",
      "2,3,5"
    )
  )
  l[[example]]
}
