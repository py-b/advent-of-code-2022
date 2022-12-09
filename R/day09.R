#' Day 09: Rope Bridge
#'
#' [Rope Bridge](https://adventofcode.com/2022/day/9)
#'
#' @name day09
#' @rdname day09
#' @param x some data
#' @export
#' @examples
#' f09a(example_data_09())
#' f09b(example_data_09())

f09a <- function(x) {

 moves <- read_moves_09(x)

 r <- RopeMoves$new(0, 0, 0, 0)
 for (m in moves) r$move(m)

 r$visited |> unique() |> length()

}

#' @rdname day09
#' @export

f09b <- function(x) {

}

# RopeMoves --------------------------------------------------------------------

RopeMoves <- R6::R6Class("RopeMoves",

  public = list(

    visited = NULL,
    head = NULL,
    tail = NULL,

    initialize = function(xhead = 0, yhead = 0, xtail = 0, ytail = 0) {
      self$visited <- c()
      self$head <- list(x = xhead, y = yhead)
      self$tail <- list(x = xtail, y = ytail)
    },

    move = function(direction) {

      # move head

      if      (direction == "U") self$head$y <- self$head$y + 1
      else if (direction == "D") self$head$y <- self$head$y - 1
      else if (direction == "R") self$head$x <- self$head$x + 1
      else if (direction == "L") self$head$x <- self$head$x - 1

      # tail follows

      dx <- self$head$x - self$tail$x
      dy <- self$head$y - self$tail$y
      stopifnot(abs(dx) %in% 0:2, abs(dy) %in% 0:2)

      if (dx == 0) {
        if (abs(dy) == 2) self$tail$y <- self$tail$y + (dy / 2)
      } else if (dy == 0) {
        if (abs(dx) == 2) self$tail$x <- self$tail$x + (dx / 2)
      } else if (abs(dx) > 1 || abs(dy) > 1) { # not on same row nor column
        self$tail$x <- self$tail$x + dx / abs(dx)
        self$tail$y <- self$tail$y + dy / abs(dy)
      }

      # update visited

      self$visited <- c(self$visited, paste0(self$tail$x, ",", self$tail$y))

      invisible(self)

    },

    print = function() {
      cat("head", unlist(self$head), "\n")
      cat("tail", unlist(self$tail), "\n")
    }

  )

)

# read_data ---------------------------------------------------------------

read_moves_09 <- function(x) {

  data <- x |> strsplit(" ") |> unlist()
  directions <- data[c(TRUE, FALSE)]
  n <- as.integer(data[c(FALSE, TRUE)])
  rep(directions, n)

}

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export

example_data_09 <- function(example = 1) {
  l <- list(
    c(
      "R 4",
      "U 4",
      "L 3",
      "D 1",
      "R 4",
      "D 1",
      "L 5",
      "R 2"
    )
  )
  l[[example]]
}
