#' Day 17: Pyroclastic Flow
#'
#' [Pyroclastic Flow](https://adventofcode.com/2022/day/17)
#'
#' @name day17
#' @rdname day17
#' @param x some data
#' @export
#' @examples
#' solve17a(example_data_17())
#' solve17b(example_data_17())

solve17a <- function(x) {

  push_seq <- parse17(x)
  chamber <- Chamber$new(push_seq)
  while(chamber$shape_count < 2022) {
    shape <- shape_n(chamber$shape_count)
    chamber$drop(shape)
  }
  chamber$height()

}

#' @rdname day17
#' @export

solve17b <- function(x) {

}


# parse ------------------------------------------------------------------------

parse17 <- function(x) strsplit(x, "")[[1]] == "<"


# utils ------------------------------------------------------------------------

shape_n <- function(n)
  switch(
    as.character(n %% 5),
    `0` = "-",
    `1` = "+",
    `2` = "L",
    `3` = "I",
    `4` = "O"
  )

move_line_horizontally <- function(shape, line, left = TRUE) {

  right_wall <- length(line) + 1
  occupied <-
    setdiff(
      c(0, which(line), right_wall),
      shape
    )
  new_shape <- shape + ifelse(left, -1, 1)

  if (any(range(new_shape) %in% c(0, right_wall))) return(NULL)

  if (!any(range(new_shape) %in% occupied)) {
    line[shape] <- FALSE
    line[new_shape] <- TRUE
    return(line)
  }

  NULL

}

move_shape_horizontally <- function(shape, lines, left = TRUE) {

  for (i in seq(shape)) {
    new_line <- move_line_horizontally(shape[[i]], lines[i,], left = left)
    if (is.null(new_line)) return(NULL)
    lines[i, ] <- new_line
  }

  lines

}

move_line_down <- function(shape, line_from, line_to) {

  if (any(line_to[shape])) return(NULL) # impossible

  line_from[shape] <- FALSE
  line_to[shape]   <- TRUE

  rbind(line_from, line_to)

}

move_shape_down <- function(shape, lines) {

  stopifnot(nrow(shape) != nrow(lines) + 1)

  for (i in rev(seq(shape))) {
    replace <- move_line_down(shape[[i]], lines[i, ], lines[i + 1, ])
    if (is.null(replace)) return(NULL)  # impossible
    lines[i:(i+1), ] <- replace
  }

  lines

}

shape_to_lgl <- function(shape_symbol = c("-", "+", "L", "I", "O")) {

  shape_symbol <- match.arg(shape_symbol)

  switch(
    shape_symbol,

    "-" = rbind(c(F, F, T, T, T, T, F)), #  ..####.

    "+" = rbind(c(F, F, F, T, F, F, F),  #  ...#...
                c(F, F, T, T, T, F, F),  #  ..###..
                c(F, F, F, T, F, F, F)), #  ...#...

    "L" = rbind(c(F, F, F, F, T, F, F),  #  ....#..
                c(F, F, F, F, T, F, F),  #  ....#..
                c(F, F, T, T, T, F, F)), #  ..###..

    "I" = rbind(c(F, F, T, F, F, F, F),  #  ..#....
                c(F, F, T, F, F, F, F),  #  ..#....
                c(F, F, T, F, F, F, F),  #  ..#....
                c(F, F, T, F, F, F, F)), #  ..#....

    "O" = rbind(c(F, F, T, T, F, F, F),  #  ..##...
                c(F, F, T, T, F, F, F))  #  ..##...
  )

}

shape_to_num <- function(shape_lgl) apply(shape_lgl, 1, which, simplify = FALSE)

Chamber <- R6::R6Class("Chamber",

  public = list(

    grid = NULL,
    shape_num = NULL,
    push_seq = NULL,
    shape_count = NULL,
    width = NULL,

    initialize = function(push_seq, width = 7) {
      self$width <- width
      self$grid <- matrix(ncol = width, nrow = 0)
      self$push_seq <- push_seq
      self$shape_count <- 0L
    },

    print = function(nlines = 12) {
      nlines <- min(nrow(self$grid), nlines)
      m <- self$grid[1:nlines, ]
      m[self$grid[1:nlines, ]]  <- "#"
      m[!self$grid[1:nlines, ]] <- "."
      cat(t(m), sep = "", fill = self$width)
      invisible(self)
    },

    height = function() nrow(self$grid),

    drop = function(shape_symbol) {

      self$shape_count <- self$shape_count + 1L

      shape <- shape_to_lgl(shape_symbol)
      self$shape_num <- shape_to_num(shape)
      shape_height <- length(self$shape_num)

      empty_rows <- matrix(FALSE, nrow = 3, ncol = self$width)
      self$grid <- rbind(shape, empty_rows, self$grid)

      i <- 0 # first line of the shape

      repeat {

        i <- i + 1

        left <- self$push_seq[1]
        self$push_seq <- c(tail(self$push_seq, -1), left) # rotating

        rows <- seq(i, i + shape_height - 1)
        new_horiz <- move_shape_horizontally(
          self$shape_num,
          self$grid[rows, , drop = FALSE],
          left = left
        )

        if (!is.null(new_horiz)) {
          self$grid[rows, ] <- new_horiz
          self$shape_num <- lapply(
            self$shape_num,
            function(x) x + if(left) -1 else 1
          )
        }

        rows <- seq(i, i + shape_height)
        if (max(rows) > nrow(self$grid)) break
        new_down <- move_shape_down(self$shape_num, self$grid[rows, ])
        if (is.null(new_down)) break

        self$grid[rows, ] <- new_down

      }

      self$grid <- self$grid[as.logical(rowSums(self$grid)), , drop = FALSE]

      invisible(self)

    }

  )

)

# example ----------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day17
#' @export

example_data_17 <- function(example = 1) {
  l <- list(
    a = c(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
  )
  l[[example]]
}
