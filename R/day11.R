#' Day 11: Monkey in the Middle
#'
#' [Monkey in the Middle](https://adventofcode.com/2022/day/11)
#'
#' @name day11
#' @rdname day11
#' @param x some data
#' @export
#' @examples
#' solve11a(example_data_11())
#' solve11b(example_data_11())

solve11a <- \(x) solve11(x, rounds = 20, modulo = FALSE)

#' @rdname day11
#' @export

solve11b <- \(x) solve11(x, rounds = 10000, modulo = TRUE)

# Solve -------------------------------------------------------------------

solve11 <- function(x, rounds, modulo) {

  monkeys <- parse_monkeys(x)
  mod <- if (modulo) monkeys |> sapply(\(x) x$div) |> prod() else NULL

  for (i in seq(rounds)) play_round(monkeys, mod)
  monkeys |> sapply(\(x) x$inspected) |> sort() |> tail(2) |> prod()

}

# Monkeys -----------------------------------------------------------------

Monkey <- R6::R6Class("Monkey",

  public = list(

    id    = NULL,
    items = NULL,
    op    = NULL,
    div   = NULL,
    m1    = NULL,
    m2    = NULL,
    inspected = 0,

    initialize = function(id, items, op, div, m1, m2) {
      self$id    <- id
      self$items <- items
      self$op    <- op
      self$div   <- div
      self$m1    <- m1
      self$m2    <- m2
    },

    update_items = function(mod = NULL) {

      if (length(self$items)) {
        self$items <- eval(str2lang(paste("self$items", self$op)))
        if (is.null(mod))
          self$items <- self$items %/% 3 # (Part 1)
        else
          self$items <- self$items %% mod # (Part 2)
      }

      invisible(self)

    },

    items_repartition = function() {
      if (!length(self$items)) return(NULL)
      receivers <- ifelse(self$items %% self$div, self$m2, self$m1)
      tapply(self$items, receivers, c)
    },

    throw = function() {
      self$inspected <- self$inspected + length(self$items)
      self$items <- NULL
      invisible(self)
    },

    receive = function(new_items) {
      self$items <- c(self$items, new_items)
      invisible(self)
    },

    print = function()
      cat(
        "(R6) <Monkey> id", self$id, "\n",
        " Items:", self$items, "\n",
        " Operation:", self$op, "\n",
        " Test div:", self$div, "\n",
        " Throwing to:", self$m1, "|", self$m2
      )

  )

)

play_round <- function(monkeys, mod) {

  for (monkey in monkeys) {

    monkey$update_items(mod)

    given <- monkey$items_repartition()

    if (length(given)) {
      mapply(
        function(m, items) monkeys[[m]]$receive(items),
        names(given),
        given
      )
      monkey$throw()
    }

  }

  monkeys

}

# Parse--------------------------------------------------------------------

parse_monkeys <- function(x) {

  # create a list of <Monkey>s from string input
  # list names are Monkeys' ids

  df_monkeys <-
    stringr::str_match_all(
      paste(trimws(x), collapse = ""),
      paste(
        "Monkey ([0-9]):",
        "Starting items: ([^O]+)",
        "Operation: new = old (. [^T]+)",
        "Test: divisible by ([0-9]+)",
        "If true: throw to monkey ([0-9]+)",
        "If false: throw to monkey ([0-9]+)",
        sep = ""
      )
    )
  df_monkeys <- as.data.frame(df_monkeys[[1]][,-1])
  colnames(df_monkeys) <- c("id", "items", "op", "div", "m1", "m2")

  df_monkeys$items <- df_monkeys$items |> strsplit(", ") |> sapply(as.integer)
  df_monkeys$op[df_monkeys$op == "* old"] <- "^ 2"
  df_monkeys$div <- as.integer(df_monkeys$div)

  n <- nrow(df_monkeys)
  res <- vector(mode = "list", length = n)

  for (i in 1:n)
    res[[i]] <-
      Monkey$new(
        df_monkeys$id[i],
        df_monkeys$items[[i]],
        df_monkeys$op[i],
        df_monkeys$div[i],
        df_monkeys$m1[i],
        df_monkeys$m2[i]
      )

  setNames(res, df_monkeys$id)

}

# Examples ----------------------------------------------------------------

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day11
#' @export

example_data_11 <- function(example = 1) {
  l <- list(
    a = c(
      "Monkey 0:",
      "  Starting items: 79, 98",
      "Operation: new = old * 19",
      "Test: divisible by 23",
      "If true: throw to monkey 2",
      "If false: throw to monkey 3",
      "",
      "Monkey 1:",
      "  Starting items: 54, 65, 75, 74",
      "Operation: new = old + 6",
      "Test: divisible by 19",
      "If true: throw to monkey 2",
      "If false: throw to monkey 0",
      "",
      "Monkey 2:",
      "  Starting items: 79, 60, 97",
      "Operation: new = old * old",
      "Test: divisible by 13",
      "If true: throw to monkey 1",
      "If false: throw to monkey 3",
      "",
      "Monkey 3:",
      "  Starting items: 74",
      "Operation: new = old + 3",
      "Test: divisible by 17",
      "If true: throw to monkey 0",
      "If false: throw to monkey 1"
    )
  )
  l[[example]]
}
