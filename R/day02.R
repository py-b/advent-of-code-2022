#' Day 02: Rock Paper Scissors
#'
#' [Rock Paper Scissors](https://adventofcode.com/2022/day/2)
#'
#' @name day02
#' @rdname day02
#' @param x some data
#' @export
#' @examples
#' f02a(example_data_02())
#' f02b(example_data_02())

f02a <- function(x) x |> sapply(round_score_01) |> sum()

#' @rdname day02
#' @export

f02b <- function(x) x |> sapply(round_score_02) |> sum()

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export

example_data_02 <- function(example = 1) {
  l <- list(
    a = c("A Y", "B X", "C Z")
  )
  l[[example]]
}

# round score ------------------------------------------------------------------

round_score_01 <- function(played,
                           lose = 0, draw = 3, win = 6,
                           rock = 1, paper = 2, scissor = 3) {

  outcome <-
    switch(
      played,
      "A X" = draw,
      "A Y" = win,
      "A Z" = lose,
      "B X" = lose,
      "B Y" = draw,
      "B Z" = win,
      "C X" = win,
      "C Y" = lose,
      "C Z" = draw
    )

  shape <-
    switch(
      substr(played, 3, 3),
      X = rock,
      Y = paper,
      Z = scissor
    )

  outcome + shape

}

round_score_02 <- function(played,
                           lose = 0, draw = 3, win = 6,
                           rock = 1, paper = 2, scissor = 3) {

  outcome <-
    switch(
      substr(played, 3, 3),
      X = lose,
      Y = draw,
      Z = win
    )

  shape <-
    switch(
      played,
      "A X" = scissor,
      "A Y" = rock,
      "A Z" = paper,
      "B X" = rock,
      "B Y" = paper,
      "B Z" = scissor,
      "C X" = paper,
      "C Y" = scissor,
      "C Z" = rock
    )

  outcome + shape

}
