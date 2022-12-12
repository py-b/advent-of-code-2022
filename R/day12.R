#' Day 12: Hill Climbing Algorithm
#'
#' [Hill Climbing Algorithm](https://adventofcode.com/2022/day/12)
#'
#' @name day12
#' @rdname day12
#' @export
#' @examples
#' solve12a(example_data_12())
#' solve12b(example_data_12())

solve12a <- function(x) {

  grid <- parse11(x)
  start <- attr(grid, "start")
  end <- attr(grid, "end")

  graph <- mat_to_graph(grid)
  path <- igraph::shortest_paths(graph, start, end)$vpath[[1]]

  length(path) - 1

}

#' @rdname day12
#' @export

solve12b <- function(x) {

  grid <- parse11(x)
  end <- attr(grid, "end")

  starts <-
    which(grid == 1, arr.ind = TRUE) |>
    apply(1, \(x) paste(x[1], x[2], sep = "-"))

  graph <- mat_to_graph(grid)

  paths <-
    lapply(
      starts,
      function(start) shortest_path_silent(graph, start, end)
    )

  paths_lengths <- lengths(paths) - 1
  paths_lengths[paths_lengths > 0] |> sort() |> head(1) |> unname()

}

# Parse input-------------------------------------------------------------------

parse11 <- function(x) {

  m_letters <- do.call(rbind, strsplit(x, ""))

  start <- which(m_letters == "S", arr.ind = TRUE)
  end   <- which(m_letters == "E", arr.ind = TRUE)

  conv_num <- \(.) c(S = 1, E = 26, setNames(1:26, letters))[.]
  m_heights <- apply(m_letters, c(1, 2), conv_num)

  structure(
    m_heights,
    start = paste(start, collapse = "-"),
    end   = paste(end,   collapse = "-")
  )

}

# Utils ------------------------------------------------------------------------

mat_to_graph <- function(m) {

  relations_str <- c()

  for (i in seq(nrow(m)))
    for (j in seq(ncol(m))) {
      for (u in -1:1)
        for (v in -1:1)
          if (abs(u + v) == 1)
            try(
              if (m[i + u, j + v] - m[i, j] <= 1) {
                from <- paste(i, j, sep = "-")
                to <- paste(i + u, j + v, sep = "-")
                relations_str <- c(relations_str, paste(from, to, sep = "->"))
              },
              silent = TRUE
            )
    }

  relations <- do.call(rbind, strsplit(relations_str, "->"))

  igraph::graph_from_data_frame(relations, directed = TRUE)

}

shortest_path_silent <- function(graph, from, to) {

  tryCatch(
    igraph::shortest_paths(graph, from, to)$vpath[[1]],
    warning = function(w) {
      if (!grepl("Couldn't reach some vertices", w$message)) warning(w$message)
    }
  )

}

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day12
#' @export

example_data_12 <- function(example = 1) {
  l <- list(
    c(
      "Sabqponm",
      "abcryxxl",
      "accszExk",
      "acctuvwj",
      "abdefghi"
    )
  )
  l[[example]]
}
