test_that("f06a", {

  expect_equal(f06a(example_data_06(1)), 7)
  expect_equal(f06a(example_data_06(2)), 5)
  expect_equal(f06a(example_data_06(3)), 6)
  expect_equal(f06a(example_data_06(4)), 10)
  expect_equal(f06a(example_data_06(5)), 11)

})

test_that("f06b", {

  expect_equal(f06b(example_data_06(1)), 19)
  expect_equal(f06b(example_data_06(2)), 23)
  expect_equal(f06b(example_data_06(3)), 23)
  expect_equal(f06b(example_data_06(4)), 29)
  expect_equal(f06b(example_data_06(5)), 26)

})
