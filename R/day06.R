#' Day 06: Tuning Trouble
#'
#' [Tuning Trouble](https://adventofcode.com/2022/day/6)
#'
#' @name day06
#' @rdname day06
#' @param x some data
#' @export
#' @examples
#' f06a(example_data_06())
#' f06b(example_data_06())

f06a <- function(x) f06(x, ndistinct = 4)

#' @rdname day06
#' @export

f06b <- function(x) f06(x, ndistinct = 14)


# Utils -------------------------------------------------------------------

f06 <- function(x, ndistinct) {

  y <- x |> strsplit("") |> unlist()

  for (i in seq(ndistinct, length(y))) {
    current <- y[seq(i - ndistinct + 1, i)]
    if (!anyDuplicated(current)) return(i)
  }

  NULL

}

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day06
#' @export
example_data_06 <- function(example = 1) {
  l <- list(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )
  l[[example]]
}
