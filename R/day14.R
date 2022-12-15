#' Day 14: Regolith Reservoir
#'
#' [Regolith Reservoir](https://adventofcode.com/2022/day/14)
#'
#' @name day14
#' @rdname day14
#' @param x some data
#' @param quiet `FALSE` to print a log of sand movement.
#' @export
#' @examples
#' solve14a(example_data_14())
#' solve14b(example_data_14())

solve14a <- function(x, quiet = TRUE) {

  cave_part1 <- Cave$new(x, infinite_floor = FALSE)
  while (!cave_part1$no_space) cave_part1$add_sand(quiet = quiet)
  cave_part1$sand_count

}

#' @rdname day14
#' @export

solve14b <- function(x) {

}

# Parse input from string ------------------------------------------------------

parse14 <- function(x) {
  # get starting columns of occupied spaces

  segments <-
    x |>
    strsplit(" -> ") |>
    lapply(\(x) do.call(rbind, strsplit(x, ","))) |>
    lapply(apply, 1:2, as.integer)

  points <- do.call(
    rbind,
    lapply(segments, segments_to_points)
  )

  points <- points[!duplicated(points), ]
  rownames(points) <- NULL

  tapply(points$y, points$x, sort, simplify = FALSE)

}

segments_to_points <- function(segments) {

  list_df <-
    cbind(
      head(segments, -1),
      tail(segments, -1)
    ) |>
    apply(
      MARGIN = 1,
      function(coords)
        data.frame(
          x = do.call(seq, as.list(coords[c(1, 3)])),
          y = do.call(seq, as.list(coords[c(2, 4)]))
        )
    )

  res <- do.call(rbind, list_df)
  rownames(res) <- NULL
  res[!duplicated(res),]

}

# Cave --------------------------------------------------------------------

Cave <- R6::R6Class("Cave",

  public = list(

    occupied = NULL,
    occupied_by_rock = NULL,
    sand_count = NULL,
    bottom = NULL,
    infinite_floor = NULL,
    min_x = NULL,
    max_x = NULL,

    no_space = NULL, # part 1 end condition
    blocked = NULL,  # part 2 end condition

    initialize = function(rocks, infinite_floor = FALSE) {

      self$no_space <- FALSE
      self$blocked <- FALSE
      self$infinite_floor <- infinite_floor

      self$sand_count <- 0

      self$occupied <- parse14(rocks)

      occupied_x <- as.integer(names(self$occupied))
      self$min_x <- if (infinite_floor) -Inf else min(occupied_x)
      self$max_x <- if (infinite_floor)  Inf else max(occupied_x)

      self$bottom <- max(unlist(self$occupied))
      if (infinite_floor) self$bottom <- self$bottom + 2

      self$occupied_by_rock <- self$occupied

    },

    print = function() {
      nrow <- self$bottom + 1
      ncol <- length(self$occupied)
      grid <- matrix(".", nrow = nrow, ncol = ncol)
      sand <- self$occupied_by_sand()
      rock <- self$occupied_by_rock
      for (x in seq(ncol)) {
        if (length(rock[[x]])) grid[rock[[x]] + 1, x] <- "#"
        if (length(sand[[x]])) grid[sand[[x]] + 1, x] <- "O"
      }
      cat(t(grid), sep = "", fill = length(self$occupied))
      invisible(self)
    },

    occupied_by_sand = function() {
      mapply(
        setdiff,
        self$occupied,
        self$occupied_by_rock,
        SIMPLIFY = FALSE
      )
    },

    add_sand = function(x = 500, y = 0, quiet = TRUE) {

      if (!quiet) message("\nnew unit   (", x, ",", y, ")")

      if (self$no_space) {
        message(
          "Impossible to add sand, no more space left (", self$sand_count, ")."
        )
        return(invisible(self)) # do nothing
      }

      if (self$blocked) {
        message(
          "Impossible to add sand, source is blocked (", self$sand_count, ")."
        )
        return(invisible(self)) # do nothing
      }

      cave <- self$occupied

      while (TRUE) {

        x_chr <- as.character(x)

        if (!self$infinite_floor) {
          # fall in endless void ?
          if (!length(cave[[x_chr]]) || x <= self$min_x || x >= self$max_x) {
            self$no_space <- TRUE
            if (!quiet) message("falling forever...")
            return(invisible(self))
          }
        }

        y <- min(cave[[x_chr]]) - 1 # fall to something
        if (!quiet) message("falling to (", x, ",", y, ")")

        cave <- lapply(cave, \(depths) depths[depths > y])

        if (self$infinite_floor && y == bottom - 1) {

          under <- c(bottom, bottom) # always a rock on bottom

        } else {

          left_name <- as.character(x - 1)
          right_name <- as.character(x + 1)

          if (!length(cave[[left_name]])) {
            self$no_space <- TRUE
            return(invisible(self))
          } else if (!length(cave[[right_name]])) {
            self$no_space <- TRUE
            return(invisible(self))
          }

          under <-
            sapply(
              cave[c(left_name, right_name)],
              head, n = 1
            )

        }

        if (under[1] == y + 1 && under[2] == y + 1) {
          # rest
          if (!quiet) message("stops at   (", x, ",", y, ")")
          self$sand_count <- self$sand_count + 1
          self$occupied[[x_chr]] <- sort(c(y, self$occupied[[x_chr]]))
          return(invisible(self))
        }

        if (under[1] > y + 1)
          x <- x - 1 # go left
        else if (under[2] > y + 1)
          x <- x + 1 # go right

      }

      stop("This should never happen.")

    }

  )

)

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day14
#' @export

example_data_14 <- function(example = 1) {
  l <- list(
    c(
      "498,4 -> 498,6 -> 496,6",
      "503,4 -> 502,4 -> 502,9 -> 494,9"
    )
  )
  l[[example]]
}
