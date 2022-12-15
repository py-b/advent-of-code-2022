test_that("solve15", {

  expect_equal(
    solve15a(example_data_15(), y0 = 10),
    26
  )

  # expect_equal(
  #   solve15b(example_data_15()),
  #   93
  # )

})
