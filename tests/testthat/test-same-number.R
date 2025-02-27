test_that("same_number works with simple number lists", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result_num <- same_number(nums1, nums2, nums3)

  expect_s3_class(result_num, "same_number")
  expect_true(inherits(result_num, "same_similarity"))
  expect_true(is.list(result_num$data))
  expect_true("normalized" %in% names(result_num$methods))
})

test_that("same_number print method works", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)

  result <- same_number(nums1, nums2)

  expect_output(print(result), "same_number")
  expect_output(print(result), "method")
})

test_that("same_number summary method works", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)

  result <- same_number(nums1, nums2)

  expect_output(summary(result), "Summary")
  expect_output(summary(result), "similarity")
})

test_that("same_number works with multiple methods", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)

  result <- same_number(nums1, nums2,
    method = c("normalized", "fuzzy", "percent_diff")
  )

  expect_s3_class(result, "same_number")
  expect_true(all(c("normalized", "fuzzy", "percent_diff") %in% names(result$methods)))
})

test_that("same_number handles nested structures", {
  nested_nums1 <- list(
    weights = list(1.2, 2.5, 3.7),
    heights = list(160, 170, 180)
  )
  nested_nums2 <- list(
    weights = list(1.25, 2.45, 3.65),
    heights = list(162, 172, 178)
  )

  result <- same_number(nested_nums1, nested_nums2)

  expect_s3_class(result, "same_number")
  expect_true(is.list(result$data$weights))
  expect_true(is.list(result$data$heights))
})

test_that("same_number plotting works", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result <- same_number(nums1, nums2, nums3)

  expect_no_error(plot(result))
  expect_no_error(plot(result, type = "histogram"))
})
