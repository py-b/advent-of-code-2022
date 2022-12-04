#' Day 03: Rucksack Reorganization
#'
#' [Rucksack Reorganization](https://adventofcode.com/2022/day/3)
#'
#' @name day03
#' @rdname day03
#'
#' @param x some data
#' @export
#' @examples
#' f03a(example_data_03())
#' f03b(example_data_03())

f03a <- function(x) {

  common_items <-
    x |>
    baggify() |>
    vapply(function(b) intersect(b$comp1, b$comp2), character(1))

  sum(priority[common_items])

}

#' @rdname day03
#' @export

f03b <- function(x) {

  common_items <-
    x |>
    strsplit("") |>
    split_by_3() |>
    vapply(function(x) Reduce(intersect, x), character(1))

  sum(priority[common_items])

}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export

example_data_03 <- function(example = 1) {
  l <- list(
    a = c(
      "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    )
  )
  l[[example]]
}

# utils ------------------------------------------------------------------------

baggify <- function(x) {

  res <- strsplit(x, "")
  lapply(
    res,
    function(s)
      list(
        comp1 = head(s, length(s) / 2),
        comp2 = tail(s, length(s) / 2)
      )
  )

}

priority <- setNames(c(1:52), c(letters, LETTERS))

split_by_3 <- function(x) split(x, (seq_along(x) - 1) %/% 3)
