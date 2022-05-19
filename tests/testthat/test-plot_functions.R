test_that("plot_sequence works properly", {
  expect_error(oeis_plot(1))
  expect_error(oeis_plot(c("A002024","A000027")))
  expect_error(oeis_plot("A002024", n="100"))
  expect_error(oeis_plot("A002024", n=1000))
  expect_error(oeis_plot("A000001"), regexp = NA)
  expect_error(oeis_plot("A000001",10), regexp = NA)
})

test_that("plot2_sequences works properly", {
  expect_error(oeis_plot2(A=c(1),B="A000027"))
  expect_error(oeis_plot2(A=c("A002024","A000027"),B="A000027"))
  expect_error(oeis_plot2(A="A000027",B=1))
  expect_error(oeis_plot2(A="A000027",B=c("A002024","A000027")))
  expect_error(oeis_plot2("A002024","A000027",n="100"))
  expect_error(oeis_plot2("A002024","A000027"), regexp = NA)
  expect_error(oeis_plot2("A002024","A000027"), regexp = NA)
})



