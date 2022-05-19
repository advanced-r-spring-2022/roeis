test_that("oeis_retrieve works properly", {
  expect_error(oeis_retrieve(4))
  expect_error(oeis_retrieve(c("A000045","A000027")))
  expect_type(oeis_retrieve("A000045"),"double")
})
