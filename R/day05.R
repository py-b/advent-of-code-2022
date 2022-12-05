#' Day 05: Supply Stacks
#'
#' [Supply Stacks](https://adventofcode.com/2022/day/5)
#'
#' @name day05
#' @rdname day05
#'
#' @param x some data
#' @export
#' @examples
#' f05a(example_data_05())
#' f05b()

f05a <- function(x) {

}


#' @rdname day05
#' @export

f05b <- function(x) {

}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export

example_data_05 <- function(example = 1) {
  l <- list(
    a = c(
      "    [D]",
      "[N] [C]",
      "[Z] [M] [P]",
      " 1   2   3",
      "",
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    ),
    stacks_chr = c(
      "    [D]",
      "[N] [C]",
      "[Z] [M] [P]",
      " 1   2   3"
    ),
    stacks_chr_2 = c(
      "    [C]         [Q]         [V]",
      "    [D]         [D] [S]     [M] [Z]",
      "    [G]     [P] [W] [M]     [C] [G]",
      "    [F]     [Z] [C] [D] [P] [S] [W]",
      "[P] [L]     [C] [V] [W] [W] [H] [L]",
      "[G] [B] [V] [R] [L] [N] [G] [P] [F]",
      "[R] [T] [S] [S] [S] [T] [D] [L] [P]",
      "[N] [J] [M] [L] [P] [C] [H] [Z] [R]",
      " 1   2   3   4   5   6   7   8   9"
    ),
    moves_chr = c(
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    )
  )
  l[[example]]
}

read_stacks <- function(stacks_chr) {

  stacks_chr <- head(stacks_chr, -1) # supprime numéro de pile

  # complete pour avoir des chaînes de même taille
  nc <- nchar(stacks_chr)
  stacks_chr <- paste0(stacks_chr, strrep(" ", max(nc) - nc))

  stacks_chr_matrix <-
    stacks_chr |>
    strsplit("") |>
    unlist() |>
    matrix(ncol = max(nc), byrow = TRUE)

  apply(
    stacks_chr_matrix[, c(FALSE, TRUE, FALSE, FALSE)],
    MARGIN = 2,
    function(x) rev(x[x != " "])
  )

}

read_moves <- function(moves_chr) {

  res <- stringr::str_match(moves_chr, "move (\\d+) from (\\d+) to (\\d+)")[,-1]
  mode(res) <- "integer"
  colnames(res) <- c("n", "from", "to")
  res

}
