## test file
test_that("test function fars_read", {

  ret <- fars_read("accident_2013.csv.bz2")
  expect_s3_class(ret, "tbl_df")

  expect_error(fars_read("tmp"))

})

test_that("test function make_filename", {
  expect_equal(make_filename("2002"), "accident_2002.csv.bz2")
})

test_that("test function fars_read_years", {
  ret <- fars_read_years(c(2013, "hello"))
  expect_s3_class(ret[[1]], "tbl_df")
  expect_null(ret[[2]])
})

test_that("test function fars_summarize_years", {
  # one dataset
  ret <- fars_summarize_years(2013)
  expect_equal(unname(unlist(lapply(ret, class))),
               c("numeric", "integer"))
  expect_equal(names(ret), c("MONTH", "2013"))
  expect_equal(nrow(ret), 12)

  # two datasets
  ret2 <- fars_summarize_years(c(2013, 2014))
  expect_equal(unname(unlist(lapply(ret2, class))),
               c("numeric", "integer", "integer"))
  expect_equal(names(ret2), c("MONTH", "2013", "2014"))

  # error
  expect_error(fars_summarize_years("hello"))

  # only valid year
  ret3 <- fars_summarize_years(c(2013, "hello"))
  expect_equal(names(ret3), c("MONTH", "2013"))

})

test_that("test function fars_map_state", {
  # valid output
  fars_map_state(1, 2013)

  # test invalid state number
  expect_error(fars_map_state(100, 2013))

  # invalid year
  expect_error(fars_map_state(1, 2017))

})
