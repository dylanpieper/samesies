library(testthat)

test_that("average_similarity works with same_text objects", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  avg <- average_similarity(result)
  expect_type(avg, "double")
  expect_true(all(avg >= 0 & avg <= 1))

  avg_jw <- average_similarity(result, method = "jw")
  expect_type(avg_jw, "double")
  expect_true(length(avg_jw) > 0)
  expect_true(all(avg_jw >= 0 & avg_jw <= 1))
})

test_that("average_similarity works with same_factor objects", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")
  fruits3 <- list("apple", "pineapple", "banana")

  result <- same_factor(fruits1, fruits2, fruits3,
    levels = c("apple", "orange", "banana")
  )

  avg <- average_similarity(result)
  expect_type(avg, "double")
  expect_true(all(avg >= 0 & avg <= 1))
})

test_that("average_similarity works with same_number objects", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result <- same_number(nums1, nums2, nums3)

  avg <- average_similarity(result)
  expect_type(avg, "double")
  expect_true(all(avg >= 0 & avg <= 1))
})

test_that("pair_averages works with same_text objects", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  pairs <- pair_averages(result)
  expect_s3_class(pairs, "data.frame")
  expect_named(pairs, c("method", "pair", "avg_score"))
  expect_true(all(pairs$avg_score >= 0 & pairs$avg_score <= 1))

  pairs_jw <- pair_averages(result, method = "jw")
  expect_s3_class(pairs_jw, "data.frame")
  expect_true(all(pairs_jw$method == "jw"))
})

test_that("pair_averages works with same_factor objects", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")
  fruits3 <- list("apple", "pineapple", "banana")

  result <- same_factor(fruits1, fruits2, fruits3,
    levels = c("apple", "orange", "banana")
  )

  pairs <- pair_averages(result)
  expect_s3_class(pairs, "data.frame")
  expect_named(pairs, c("method", "pair", "avg_score"))
  expect_true(all(pairs$avg_score >= 0 & pairs$avg_score <= 1))
})

test_that("pair_averages works with same_number objects", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result <- same_number(nums1, nums2, nums3)

  pairs <- pair_averages(result)
  expect_s3_class(pairs, "data.frame")
  expect_named(pairs, c("method", "pair", "avg_score"))
  expect_true(all(pairs$avg_score >= 0 & pairs$avg_score <= 1))
})
