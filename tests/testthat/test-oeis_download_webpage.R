test_that("function returns error", {
  expect_error(oeis_download_webpage(4))
  expect_error(oeis_download_webpage("ABC0045"))
  expect_error(oeis_download_webpage("ABC0045", "references"))
  expect_error(oeis_download_webpage("A000045", "fake"))
})

test_that("function returns correct object", {
  expect_type(oeis_download_webpage("A000045", "all"), "character")
  expect_type(oeis_download_webpage("A000045", "links"), "NULL")
  expect_type(oeis_download_webpage("A000045", "formula"), "NULL")
  expect_type(oeis_download_webpage("A000045", "references"), "NULL")
  expect_type(oeis_download_webpage("A000045", "comments"), "NULL")
  expect_type(oeis_download_webpage("A000045", "examples"), "NULL")
  expect_type(oeis_download_webpage("A000045", "second"), "NULL")
  expect_type(oeis_download_webpage("A000045", "third"), "NULL")
  expect_type(oeis_download_webpage("A000045", "fourth"), "NULL")
})
