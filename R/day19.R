#' Day 19: Not Enough Minerals
#'
#' [Not Enough Minerals](https://adventofcode.com/2022/day/19)
#'
#' @name day19
#' @rdname day19
#' @param x some data
#' @export
#' @examples
#' solve19a(example_data_19())
#' solve19b()

solve19a <- function(x) {

}


#' @rdname day19
#' @export
solve19b <- function(x) {

}

# Parse -------------------------------------------------------------------

parse19 <- function(x) {

  pattern <- paste(
    "Blueprint (\\d+):",
    "Each ore robot costs (\\d+) ore.",
    "Each clay robot costs (\\d+) ore.",
    "Each obsidian robot costs (\\d+) ore and (\\d+) clay.",
    "Each geode robot costs (\\d+) ore and (\\d+) obsidian."
  )

  stringr::str_match(x, pattern)[,-1] |>
    apply(1:2, as.integer) |>
    apply(
      1,
      function(x)
        list(
          id                  = x[1],
          ore_robot_cost      = list(ore = x[2]),
          clay_robot_cost     = list(ore = x[3]),
          obsidian_robot_cost = list(ore = x[4], clay     = x[5]),
          geode_robot_cost    = list(ore = x[6], obsidian = x[7])
        )
    )

}

# R6 class ----------------------------------------------------------------

Collector <- R6::R6Class("Collector",

  public = list(

    ellapsed = NULL,
    waiting_robot = NULL,

    blueprint_id = NULL,

    ore = NULL,
    ore_robot = NULL,
    ore_robot_cost = NULL,

    clay = NULL,
    clay_robot = NULL,
    clay_robot_cost = NULL,

    obsidian = NULL,
    obsidian_robot = NULL,
    obsidian_robot_cost = NULL,

    geode = NULL,
    geode_robot = NULL,
    geode_robot_cost = NULL,

    initialize = function(blueprint, ore = 0) {
      self$blueprint_id <- blueprint$id
      self$ellapsed <- 0
      self$ore <- ore
      self$ore_robot <- 1
      self$clay <- 0
      self$clay_robot <- 0
      self$obsidian <- 0
      self$obsidian_robot <- 0
      self$geode <- 0
      self$geode_robot <- 0
      self$ore_robot_cost      <- blueprint$ore_robot_cost
      self$clay_robot_cost     <- blueprint$clay_robot_cost
      self$obsidian_robot_cost <- blueprint$obsidian_robot_cost
      self$geode_robot_cost    <- blueprint$geode_robot_cost
    },

    print = function() {
      cat("[ Minute", self$ellapsed, "]\n")
      cat(" In construction :", self$waiting_robot, "\n")
      data.frame(
        Material = c("ore", "clay", "obsidian", "geode"),
        Robots = c(self$ore_robot, self$clay_robot, self$obsidian_robot, self$geode_robot),
        Ressources = c(self$ore, self$clay, self$obsidian, self$geode)
      ) |> print(row.names = FALSE)
      invisible(self)
    },

    create_robot = function(robot = c("none", "ore", "clay", "obsidian", "geode")) {

      robot <- match.arg(robot)

      if (robot == "none") return(invisible(self))

      robot_name <- paste0(robot, "_robot")

      if (robot == "geode") {
        if (self$obsidian < self$geode_robot_cost$obsidian)
          stop("not enough obsidian")
        self$obsidian <- self$obsidian - self$geode_robot_cost$obsidian
      } else if (robot == "obsidian") {
        if (self$clay < self$obsidian_robot_cost$clay)
          stop("not enough clay")
        self$clay <- self$clay - self$obsidian_robot_cost$clay
      }

      if (self$ore < self$ore - self[[paste0(robot_name, "_cost")]]$ore)
        stop("not enough ore")
      self$ore <- self$ore - self[[paste0(robot_name, "_cost")]]$ore

      self$waiting_robot <- c(self$waiting_robot, robot)

      invisible(self)

    },

    receive_robot = function() {
      if (length(self$waiting_robot)) {
        robot_name <- paste0(self$waiting_robot, "_robot")
        self[[robot_name]] <- self[[robot_name]] + 1
        self$waiting_robot <- NULL
      }
    },

    collect = function() {
      self$ore      <- self$ore + self$ore_robot
      self$clay     <- self$clay + self$clay_robot
      self$obsidian <- self$obsidian + self$obsidian
      self$geode    <- self$geode + self$geode
      invisible(self)
    },

    play_minute = function(created_robot) {
      self$receive_robot()
      self$create_robot(created_robot)
      self$collect()
      self$ellapsed <- self$ellapsed + 1
      invisible(self)
    }

  )

)


# Example -----------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day19
#' @export

example_data_19 <- function(example = 1) {
  l <- list(
    a = c(
      paste(
        "Blueprint 1:",
        "Each ore robot costs 4 ore.",
        "Each clay robot costs 2 ore.",
        "Each obsidian robot costs 3 ore and 14 clay.",
        "Each geode robot costs 2 ore and 7 obsidian."
      ),
      paste(
        "Blueprint 2:",
        "Each ore robot costs 2 ore.",
        "Each clay robot costs 3 ore.",
        "Each obsidian robot costs 3 ore and 8 clay.",
        "Each geode robot costs 3 ore and 12 obsidian."
      )
    )
  )
  l[[example]]
}
