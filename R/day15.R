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
  intervals <- sensors |> sensors_infos() |> lapply(no_beacon, y0 = y0)

  intervals <- Filter(length, intervals)
  if (sum(lengths(intervals)) == 0) return(0)
  intervals <- do.call(rbind, intervals)

  res <-
    intervals |>
    intervals::Intervals() |>
    intervals::interval_union() |>
    intervals::size()

  devices <- rbind(sensors[, 1:2], sensors[, 3:4])
  devices <- devices[devices[, 2] == y0, 1]

  (res + 1) - length(unique(devices))

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
  colnames(res) <- c("Sx", "Sy", "Bx", "By")
  apply(res, 1:2, as.integer)
}

# utils ------------------------------------------------------------------------

sensors_infos <- function(sensors) {

  apply(
    sensors,
    1,
    simplify = FALSE,
    function(sensor) {
      size_x <- abs(sensor["Bx"] - sensor["Sx"])
      size_y <- abs(sensor["By"] - sensor["Sy"])
      manhattan_distance <- size_x + size_y
      res <-
        list(
          manhattan_distance = manhattan_distance,
          Sx = sensor[1],
          Sy = sensor[2],
          top_bound = sensor["Sy"] - manhattan_distance,
          down_bound = sensor["Sy"] + manhattan_distance
        )
      lapply(res, unname)
    }
  )

}

no_beacon <- function(sensor_info, y0 = 10) {
  dist_to_y0 <- abs(sensor_info$Sy - y0)
  if (dist_to_y0 > sensor_info$manhattan_distance) return(integer(0))
  c(
    sensor_info$Sx - sensor_info$manhattan_distance + dist_to_y0,
    sensor_info$Sx + sensor_info$manhattan_distance - dist_to_y0
  )
}


# Example ----------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
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
