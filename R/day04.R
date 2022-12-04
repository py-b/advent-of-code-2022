#' Day 04: Camp Cleanup
#'
#' [Camp Cleanup](https://adventofcode.com/2022/day/4)
#'
#' @name day04
#' @rdname day04
#'
#' @param x some data
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b(example_data_04())

f04a <- function(x) {
  x |>
    read_values_04() |>
    apply(1, full_overlaps_v) |>
    sum()
}

#' @rdname day04
#' @export

f04b <- function(x) {
  x |>
    read_values_04() |>
    apply(1, overlaps_v) |>
    sum()
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export

example_data_04 <- function(example = 1) {
  l <- list(
    a = c(
      "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
    )
  )
  l[[example]]
}

read_values_04 <- function(x) {
  res <- stringr::str_match(x, "(\\d+)-(\\d+),(\\d+)-(\\d+)")[, -1]
  mode(res) <- "integer"
  res
}

full_overlaps <- function(a1, a2, b1, b2) (a1 >= b1 & a2 <= b2) | (b1 >= a1 & b2 <= a2)
full_overlaps_v <- function(x) full_overlaps(x[1], x[2], x[3], x[4])

overlaps <- function(a1, a2, b1, b2) (b1 <= a2) & (a1 <= b2)
overlaps_v <- function(x) overlaps(x[1], x[2], x[3], x[4])
