#' Day 10: Cathode-Ray Tube
#'
#' [Cathode-Ray Tube](https://adventofcode.com/2022/day/10)
#'
#' @name day10
#' @rdname day10
#' @param x some data
#' @export
#' @examples
#' solve10a(example_data_10())
#' solve10b(example_data_10())

solve10a <- function(x) {

  cmds <- read_commands_09(x)
  xh <- x_history(cmds)

  cycles <- c(20, 60, 100, 140, 180, 220)
  sum(xh[cycles] * cycles)

}

#' @rdname day10
#' @export

solve10b <- function(x) {

  cmds <- read_commands_09(x)
  xh <- x_history(cmds)

  res <- rep(".", 40 * 6)

  for (i in 1:240) {
    sprite <- seq(xh[i] - 1, xh[i] + 1)
    if ((i - 1) %% 40 %in% sprite) res[i] <- "#"
  }

  cat(res, sep = "", fill = 40)

  invisible(res)

}

# Utils ------------------------------------------------------------------------

read_commands_09 <- function(command) {

  lapply(
    strsplit(command, " "),
    function(.) {
      res <- list(f = .[1])
      if (length(.) > 1) res$v <- as.integer(.[2])
      res
    }
  )

}

x_history <- function(cmds) {

  x <- 1

  for (cmd in cmds) {
    last <- tail(x, 1)
    x <- c(x, last)
    if (cmd$f == "addx") x <- c(x, last + cmd$v)
  }

  x

}

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export

example_data_10 <- function(example = 1) {
  l <- list(
    a = c(
      "addx 15",
      "addx -11",
      "addx 6",
      "addx -3",
      "addx 5",
      "addx -1",
      "addx -8",
      "addx 13",
      "addx 4",
      "noop",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx -35",
      "addx 1",
      "addx 24",
      "addx -19",
      "addx 1",
      "addx 16",
      "addx -11",
      "noop",
      "noop",
      "addx 21",
      "addx -15",
      "noop",
      "noop",
      "addx -3",
      "addx 9",
      "addx 1",
      "addx -3",
      "addx 8",
      "addx 1",
      "addx 5",
      "noop",
      "noop",
      "noop",
      "noop",
      "noop",
      "addx -36",
      "noop",
      "addx 1",
      "addx 7",
      "noop",
      "noop",
      "noop",
      "addx 2",
      "addx 6",
      "noop",
      "noop",
      "noop",
      "noop",
      "noop",
      "addx 1",
      "noop",
      "noop",
      "addx 7",
      "addx 1",
      "noop",
      "addx -13",
      "addx 13",
      "addx 7",
      "noop",
      "addx 1",
      "addx -33",
      "noop",
      "noop",
      "noop",
      "addx 2",
      "noop",
      "noop",
      "noop",
      "addx 8",
      "noop",
      "addx -1",
      "addx 2",
      "addx 1",
      "noop",
      "addx 17",
      "addx -9",
      "addx 1",
      "addx 1",
      "addx -3",
      "addx 11",
      "noop",
      "noop",
      "addx 1",
      "noop",
      "addx 1",
      "noop",
      "noop",
      "addx -13",
      "addx -19",
      "addx 1",
      "addx 3",
      "addx 26",
      "addx -30",
      "addx 12",
      "addx -1",
      "addx 3",
      "addx 1",
      "noop",
      "noop",
      "noop",
      "addx -9",
      "addx 18",
      "addx 1",
      "addx 2",
      "noop",
      "noop",
      "addx 9",
      "noop",
      "noop",
      "noop",
      "addx -1",
      "addx 2",
      "addx -37",
      "addx 1",
      "addx 3",
      "noop",
      "addx 15",
      "addx -21",
      "addx 22",
      "addx -6",
      "addx 1",
      "noop",
      "addx 2",
      "addx 1",
      "noop",
      "addx -10",
      "noop",
      "noop",
      "addx 20",
      "addx 1",
      "addx 2",
      "addx 2",
      "addx -6",
      "addx -11",
      "noop",
      "noop",
      "noop"
    )
  )
  l[[example]]
}
