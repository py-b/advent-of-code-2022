test_that("f01", {

  expect_equal(
    f01a(example_data_01()),
    24000
  )

  expect_equal(
    f01b(example_data_01()),
    45000
  )

})
