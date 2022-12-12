#' Day 08: Treetop Tree House
#'
#' [Treetop Tree House](https://adventofcode.com/2022/day/8)
#'
#' @name day08
#' @rdname day08
#' @param x some data
#' @export
#' @examples
#' f08a(example_data_08())
#' f08b(example_data_08())

f08a <- \(x) x |> parse08() |> visible_from_anywhere() |> sum()

#' @rdname day08
#' @export

f08b <- function(x) {

  m <- parse08(x)

  res <- 0

  for (i in seq(2, nrow(m) - 1)) {
    for (j in seq(2, ncol(m) - 1)) {
      cur <- viewing_distance(m[i, ], j) * viewing_distance(m[, j], i)
      if (cur > res) res <- cur
    }
  }

  res

}


# Utils ------------------------------------------------------------------------

parse08 <- \(x) do.call(rbind, strsplit(x, "")) |> apply(1:2, as.integer)

visible <- function(heights, rev = FALSE) {
  if (rev) heights <- rev(heights)
  res <- !duplicated(cummax(heights))
  if (rev) rev(res) else res
}

visible_from_left   <- \(tm) t(apply(tm, 1, visible))
visible_from_top    <- \(tm) apply(tm, 2, visible)
visible_from_right  <- \(tm) t(apply(tm, 1, visible, rev = TRUE))
visible_from_bottom <- \(tm) apply(tm, 2, visible, rev = TRUE)

visible_from_anywhere <- function(tm) {
  visible_from_left(tm)   |
  visible_from_top(tm)    |
  visible_from_right(tm)  |
  visible_from_bottom(tm)
}

viewing_distance <- function(heights, i) {

  if (heights[i] == 0) return(1)

  front    <- heights |> tail(-i)
  opposite <- heights |> head(i - 1) |> rev()

  dist_front <- which(front >= heights[i])[1]
  if (is.na(dist_front)) dist_front <- length(front)

  dist_opposite <- which(opposite >= heights[i])[1]
  if (is.na(dist_opposite)) dist_opposite <- length(opposite)

  dist_front * dist_opposite

}


# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export

example_data_08 <- function(example = 1) {
  l <- list(
    c(
      "30373",
      "25512",
      "65332",
      "33549",
      "35390"
    )
  )
  l[[example]]
}
