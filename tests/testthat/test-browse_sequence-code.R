test_that("browse_function works properly", {
  expect_error(oeis_browse(4))
  expect_error(oeis_browse(NULL))
  expect_error(oeis_browse(TRUE))
  expect_error(oeis_browse("A000045"), regexp = NA)
})
