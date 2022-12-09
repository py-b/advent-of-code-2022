test_that("f09", {

  expect_equal(
    f09a(example_data_09("a")),
    13
  )

  expect_equal(
    f09b(example_data_09("a")),
    1
  )

  expect_equal(
    f09b(example_data_09("b")),
    36
  )

})
