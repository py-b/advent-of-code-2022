#' Day 01: Calorie Counting
#'
#' [Calorie Counting](https://adventofcode.com/2022/day/1)
#'
#' @name day01
#' @rdname day01
#'
#' @param x some data
#' @export
#' @examples
#' f01a(example_data_01())
#' f01b(example_data_01())

f01a <- function(x) {

  x |>
    prepare_data_01() |>
    vapply(sum, numeric(1)) |>
    max()

}

#' @rdname day01
#' @export

f01b <- function(x) {

  x |>
    prepare_data_01() |>
    vapply(sum, numeric(1)) |>
    sort(decreasing = TRUE) |>
    head(3) |>
    sum()

}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export

example_data_01 <- function(example = 1) {
  l <- list(
    a = c("1000", "2000", "3000", "", "4000", "", "5000", "6000", "",
          "7000", "8000", "9000", "", "10000")
  )
  l[[example]]
}

# Utils ------------------------------------------------------------------------

prepare_data_01 <- function(x) {

  res <- split(as.integer(x), cumsum(x == ""))
  lapply(res, function(x) x[!is.na(x)])

}
