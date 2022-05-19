test_that("oeis_finder woks properly", {
  oeis_cache()
  expect_error(oeis_finder("1"))
  expect_type(oeis_finder(1:10),"list")

})
