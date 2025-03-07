test_that("same_number works with simple number lists", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result_num <- same_number(nums1, nums2, nums3)

  expect_true(inherits(result_num, "similar_number"))
  expect_true(inherits(result_num, "similar"))
  expect_true(is.list(result_num$scores))
  expect_true("normalized" %in% result_num$methods)
})

test_that("same_number print method works", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)

  result <- same_number(nums1, nums2)

  expect_error(print(result), NA)
})

test_that("same_number summary method works", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)

  result <- same_number(nums1, nums2)

  expect_error(summary(result), NA)
})

test_that("same_number works with multiple methods", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)

  result <- same_number(nums1, nums2,
    method = c("normalized", "fuzzy", "percent", "exp", "raw")
  )

  expect_true(inherits(result, "similar_number"))
  expect_true(all(c("normalized", "fuzzy", "percent", "exp", "raw") %in% result$methods))
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

  expect_true(inherits(result, "similar_number"))
  expect_true("weights_nested_nums1_nested_nums2" %in% names(result$scores$normalized))
  expect_true("heights_nested_nums1_nested_nums2" %in% names(result$scores$normalized))
})

test_that("same_number utility functions work", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result <- same_number(nums1, nums2, nums3)

  avg_sim <- average_similarity(result)
  expect_type(avg_sim, "double")
  expect_true(all(avg_sim >= 0 & avg_sim <= 1))

  pairs <- pair_averages(result)
  expect_s3_class(pairs, "data.frame")
  expect_named(pairs, c("method", "pair", "avg_score"))
  expect_true(all(pairs$pair %in% c("nums1_nums2", "nums1_nums3", "nums2_nums3")))
  expect_true(all(pairs$avg_score >= 0 & pairs$avg_score <= 1))
})

test_that("same_number works with exp method", {
  nums1 <- list(1, 2, 3)
  nums2 <- list(1.5, 2.5, 3.5)

  result <- same_number(nums1, nums2, method = "exp")

  expect_true(inherits(result, "similar_number"))
  expect_true(inherits(result, "similar"))
  expect_true("exp" %in% result$methods)

  scores <- result$scores$exp$nums1_nums2

  expect_equal(scores[1], scores[3], tolerance = 0.001)

  expect_true(all(scores >= 0 & scores <= 1))

  nums3 <- list(1, 2, 3)
  exact_result <- same_number(nums1, nums3, method = "exp")
  exact_scores <- exact_result$scores$exp$nums1_nums3
  expect_equal(exact_scores, c(1, 1, 1))
})

test_that("same_number works with raw method", {
  nums1 <- list(1, 2, 3)
  nums2 <- list(1.5, 2.5, 3.5)

  result <- same_number(nums1, nums2, method = "raw")

  expect_true(inherits(result, "similar_number"))
  expect_true(inherits(result, "similar"))
  expect_true("raw" %in% result$methods)

  diffs <- result$scores$raw$nums1_nums2
  expected_diffs <- c(0.5, 0.5, 0.5)
  expect_equal(diffs, expected_diffs)

  nums3 <- list(2, 4, 6)
  result2 <- same_number(nums1, nums3, method = "raw")
  diffs2 <- result2$scores$raw$nums1_nums3
  expect_equal(diffs2, c(1, 2, 3))
})
