test_that("modify_stacks", {

  expect_equal(
    example_data_05(2) |>
      read_stacks() |>
      modify_stacks(1, 2, 1) |>
      modify_stacks(3, 1, 3) |>
      modify_stacks(2, 2, 1) |>
      modify_stacks(1, 1, 2),
    list("C", "M", c("P", "D", "N", "Z"))
  )

  expect_equal(
    example_data_05(2) |>
      read_stacks() |>
      seq_modify_stacks(matrix(c(1, 3, 2, 1, 2, 1, 2, 1, 1, 3, 1, 2), ncol = 3)),
    list("C", "M", c("P", "D", "N", "Z"))
  )

})

test_that("f05", {

  expect_equal(
    f05a(example_data_05()),
    "CMZ"
  )

  expect_equal(
    f05b(example_data_05()),
    "MCD"
  )

})
