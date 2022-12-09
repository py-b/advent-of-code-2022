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

f09a <- function(x) solve(x, n_knots = 2)

#' @rdname day09
#' @export

f09b <- function(x) solve(x, n_knots = 10)


# Solution ---------------------------------------------------------------------

solve <- function(x, n_knots) {

  moves <- read_moves_09(x)

  r <- RopeMoves$new(n_knots)
  for (m in moves) r$move(m)

  r$visited |> unique() |> length()

}

# RopeMoves --------------------------------------------------------------------

RopeMoves <- R6::R6Class("RopeMoves",

  public = list(

    visited = NULL,
    knots = NULL,

    initialize = function(n_knots = 10) {
      self$visited <- c()
      self$knots <- vector(mode = "list", length = n_knots)
      for (k in 1:n_knots) self$knots[[k]] <- list(x = 0, y = 0)
    },

    move = function(direction) {

      n <- length(self$knots)

      # move head
      if      (direction == "U") self$knots[[1]]$y <- self$knots[[1]]$y + 1
      else if (direction == "D") self$knots[[1]]$y <- self$knots[[1]]$y - 1
      else if (direction == "R") self$knots[[1]]$x <- self$knots[[1]]$x + 1
      else if (direction == "L") self$knots[[1]]$x <- self$knots[[1]]$x - 1

      # tails reactions
      for (k in 2:n)
        self$knots[[k]] <-
          tail_reaction(
            self$knots[[k - 1]],
            self$knots[[k]]
          )

      # update visited
      self$visited <- c(
        self$visited,
        paste0(self$knots[[n]]$x, ",", self$knots[[n]]$y)
      )

      invisible(self)

    },

    print = function() {
      cat("head 1 :", unlist(head(self$knots, 1)), "\n")
      cat("tail", length(self$knots), ":", unlist(tail(self$knots, 1)), "\n")
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


# tail_reaction -----------------------------------------------------------

tail_reaction <- function(head, tail) {

  dx <- head$x - tail$x
  dy <- head$y - tail$y
  stopifnot(abs(dx) %in% 0:2, abs(dy) %in% 0:2)

  if (dx == 0) {
    if (abs(dy) == 2) tail$y <- tail$y + dy / abs(dy)
  } else if (dy == 0) {
    if (abs(dx) == 2) tail$x <- tail$x + dx / abs(dx)
  } else if (abs(dx) > 1 || abs(dy) > 1) { # not on same row nor column
    tail$x <- tail$x + dx / abs(dx)
    tail$y <- tail$y + dy / abs(dy)
  }

  tail

}

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export

example_data_09 <- function(example = 1) {
  l <- list(
    a = c(
      "R 4",
      "U 4",
      "L 3",
      "D 1",
      "R 4",
      "D 1",
      "L 5",
      "R 2"
    ),
    b = c(
      "R 5",
      "U 8",
      "L 8",
      "D 3",
      "R 17",
      "D 10",
      "L 25",
      "U 20"
    )
  )
  l[[example]]
}
