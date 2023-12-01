#' Day 25: Full of Hot Air
#'
#' [Full of Hot Air](https://adventofcode.com/2022/day/25)
#'
#' @name day25
#' @rdname day25
#' @param x some data
#' @export
#' @examples
#' solve25a(example_data_25())

solve25a <- function(x)
  x |> snafu_to_base10() |> sum() |> base10_to_base5() |> base5_to_snafu()


# Utils ------------------------------------------------------------------------

snafu_to_base10 <- function(snafu) {

  to_int <- c("0" = 0, "1" = 1, "2" = 2, "-" = -1, "=" = -2)

  sapply(
    strsplit(snafu, ""),
    function(x) sum(to_int[x] * rev(5 ^ (seq(x) - 1)))
  )

}

base10_to_base5 <- function(n) {

  d   <- ceiling(log(n, 5))
  vec <- numeric()
  val <- n

  while (d >= 0) {
    rem <- val %/% 5^d
    val <- val - rem * 5^d
    vec <- c(vec, rem)
    d <- d - 1
  }

  if (vec[1] == 0) vec <- vec[-1]
  vec

}

base5_to_snafu <- function(n) {

  n <- c(rev(n), 0)

  for (i in seq(n - 1))
    if (n[i] > 2) {
      n[i]     <- n[i] - 5
      n[i + 1] <- n[i + 1] + 1
    }

  n <- as.character(rev(n))

  if (n[1] == "0") n <- n[-1]
  n[n == "-2"] <- "="
  n[n == "-1"] <- "-"

  paste(n, collapse = "")

}

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name)
#' @rdname day25
#' @export

example_data_25 <- function(example = 1) {
  l <- list(
    a = c(
      "1=-0-2" ,#  1747
      "12111"  ,#   906
      "2=0="   ,#   198
      "21"     ,#    11
      "2=01"   ,#   201
      "111"    ,#    31
      "20012"  ,#  1257
      "112"    ,#    32
      "1=-1="  ,#   353
      "1-12"   ,#   107
      "12"     ,#     7
      "1="     ,#     3
      "122"     #    37
    )
  )
  l[[example]]
}
