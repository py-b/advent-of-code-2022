#' Day 15: Beacon Exclusion Zone
#'
#' [Beacon Exclusion Zone](https://adventofcode.com/2022/day/15)
#'
#' @name day15
#' @rdname day15
#' @param x some data
#' @param y0 row where to count (10 for example data, 2000000 for real input).
#' @export

solve15a <- function(x, y0 = 2e+06) {

  sensors <- parse15(x)
  intervals_range <- no_beacon(sensors, y0 = y0)

  intervals_range <- Filter(length, intervals_range)
  if (sum(lengths(intervals_range)) == 0) return(0)
  intervals_range <- do.call(rbind, intervals_range)

  res <-
    intervals_range |>
    intervals::Intervals() |>
    intervals::interval_union() |>
    intervals::size()

  sensors <- as.matrix(sensors)
  devices <- rbind(sensors[, 1:2], sensors[, 3:4])
  devices_on_y0 <- unique(devices[devices[, 2] == y0, 1])

  (res + 1) - length(devices_on_y0)

}

#' @rdname day15
#' @export

solve15b <- function(x) {

}

# Parse data from string -------------------------------------------------------

parse15 <- function(x) {

  res <-
    stringr::str_match(
      x,
      paste(
        "Sensor at x=(-?\\d+), y=(-?\\d+):",
        "closest beacon is at x=(-?\\d+), y=(-?\\d+)"
      )
    )[, -1]

  colnames(res) <- c("x", "y", "beacon_x", "beacon_y")

  res |> apply(1:2, as.integer) |> as.data.frame()

}

# utils ------------------------------------------------------------------------

no_beacon <- function(sensors, y0) {

  sensors$manhattan_distance <-
    with(
      sensors,
      abs(x - beacon_x) + abs(y - beacon_y)
    )

  apply(
    sensors,
    1,
    function(sensor) {
      dist_to_y0 <- abs(sensor["y"] - y0)
      if (dist_to_y0 > sensor["manhattan_distance"]) return(NULL)
      c(
        sensor["x"] - sensor["manhattan_distance"] + dist_to_y0,
        sensor["x"] + sensor["manhattan_distance"] - dist_to_y0
      )
    }
  )

}

# Example ----------------------------------------------------------------------

#' @param example Which example data to use (beacon_y position or name).
#' @rdname day15
#' @export

example_data_15 <- function(example = 1) {
  l <- list(
    c("Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
      "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
      "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
      "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
      "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
      "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
      "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
      "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
      "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
      "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
      "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
      "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
      "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
      "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
    )
  )
  l[[example]]
}
