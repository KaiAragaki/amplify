test_that("no Sample XX returns input names", {
  expect_equal(pad_zero(c("cat", "dog")), c("cat", "dog"))
})

test_that("pads when supposed", {
  names <- c("Sample 1", "Sample 10")
  expect_equal(pad_zero(names), c("Sample 01", "Sample 10"))
})

test_that("does not pad when not needed", {
  names <- c("Sample 1", "Sample 9")
  expect_equal(pad_zero(names), names)
})

test_that("many same samples do not pad", {
  names <- c("Sample 1", "Sample 1", "Sample 1", "Sample 1", "Sample 1", "Sample 1",
             "Sample 2", "Sample 2", "Sample 2", "Sample 2", "Sample 2", "Sample 2")
  expect_equal(pad_zero(names), names)
})

test_that("does not pay attention to or modify nonstandard names", {
  names <- c("sample 10", "sample 1", "Sample 1")
  expect_equal(pad_zero(names), names)
})
