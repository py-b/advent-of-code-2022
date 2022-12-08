#' Day 07: No Space Left On Device
#'
#' [No Space Left On Device](https://adventofcode.com/2022/day/7)
#'
#' @name day07
#' @rdname day07
#' @param x some data
#' @export
#' @examples
#' solve07a(example_data_07())
#' solve07b(example_data_07())

solve07a <- function(x) {

  sizes_x <- sizes(x)
  sum(sizes_x[sizes_x <= 100000])

}

#' @rdname day07
#' @export

solve07b <- function(x) {

  sizes_x <- sizes(x)

  total <- 70000000
  needed <- 30000000

  used <- sizes_x["./"]
  free <- total - used

  min_del <- needed - free
  sizes_x[sizes_x >= min_del] |> unname() |> sort() |> head(1)

}

# Utils ------------------------------------------------------------------------

sizes <- function(x) {

  # size of all dirs
  # helper used in solve07a an solve07b

  ds <- x |> files() |> dir_size()

  sapply(
    names(ds),
    dir_size_rec,
    dir_sizes = ds
  )

}

files <- function(histo, start = ".") {

  res <- NULL
  current <- start

  for (h in histo[-1]) {

    if (h == "$ cd ..")
      current <- dirname(current)
    else if (substr(h, 1, 4) == "$ cd")
      current <- file.path(current, substring(h, 6))
    else if (grepl("^[0-9]", h))
      res <- c(res, file.path(current, h))

  }

  res

}

all_dirs <- function(dir) {
  if (dir == ".") return(".")
  c(dir, all_dirs(dirname(dir)))
}

all_dirs_all <- function(files) {
  files |>
    dirname() |>
    unique() |>
    lapply(all_dirs) |>
    unlist()
}

dir_size <- function(files) {

  dirs_with_files <-
    tapply(
      files,
      paste0(dirname(files), "/"),
      \(f) basename(f) |> stringr::str_extract("[0-9]+") |> as.integer() |> sum()
    )

  all <- paste0(all_dirs_all(files), "/")
  dirs_without_files <- setdiff(all, names(dirs_with_files))
  n_without <- length(dirs_without_files)

  c(
    dirs_with_files,
    setNames(rep(0L, n_without), dirs_without_files)
  )

}

dir_size_rec <- function(dir, dir_sizes) {

  subdirs <- grepl(paste0("^", dir), names(dir_sizes))
  sum(dir_sizes[subdirs])

}

# Examples ---------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day07
#' @export

example_data_07 <- function(example = 1) {
  l <- list(
    c(
      "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k"
    )
  )
  l[[example]]
}
