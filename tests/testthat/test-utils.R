library(testthat)

test_that("average_similarity works with same_text objects", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  avg <- average_similarity(result)
  expect_type(avg, "double")
  expect_true(avg >= 0 && avg <= 1)

  avg_jw <- average_similarity(result, method = "jw")
  expect_type(avg_jw, "double")
  expect_true(avg_jw >= 0 && avg_jw <= 1)
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
  expect_true(avg >= 0 && avg <= 1)
})

test_that("average_similarity works with same_number objects", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result <- same_number(nums1, nums2, nums3)

  avg <- average_similarity(result)
  expect_type(avg, "double")
  expect_true(avg >= 0 && avg <= 1)
})

test_that("pair_averages works with same_text objects", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  pairs <- pair_averages(result)
  expect_type(pairs, "list")
  expect_true(all(names(pairs) %in% c("1-2", "1-3", "2-3")))
  expect_true(all(sapply(pairs, function(x) is.numeric(x) && x >= 0 && x <= 1)))

  pairs_jw <- pair_averages(result, method = "jw")
  expect_type(pairs_jw, "list")
  expect_true(all(names(pairs_jw) %in% c("1-2", "1-3", "2-3")))
})

test_that("pair_averages works with same_factor objects", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")
  fruits3 <- list("apple", "pineapple", "banana")

  result <- same_factor(fruits1, fruits2, fruits3,
    levels = c("apple", "orange", "banana")
  )

  pairs <- pair_averages(result)
  expect_type(pairs, "list")
  expect_true(all(names(pairs) %in% c("1-2", "1-3", "2-3")))
})

test_that("pair_averages works with same_number objects", {
  nums1 <- list(0.95, 1, 0.60)
  nums2 <- list(1, 1, 0.65)
  nums3 <- list(0.90, 1, 0.55)

  result <- same_number(nums1, nums2, nums3)

  pairs <- pair_averages(result)
  expect_type(pairs, "list")
  expect_true(all(names(pairs) %in% c("1-2", "1-3", "2-3")))
})
