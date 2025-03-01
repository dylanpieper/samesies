test_that("same_factor works with simple factor lists", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")
  fruits3 <- list("apple", "pineapple", "banana")

  result_factor <- same_factor(fruits1, fruits2, fruits3,
    levels = c("apple", "orange", "banana")
  )

  expect_true(inherits(result_factor, "samesies::similar_factor"))
  expect_true(inherits(result_factor, "samesies::similar"))
  expect_true(is.list(result_factor@scores))
  expect_true("exact" %in% result_factor@methods)
})

test_that("same_factor print method works", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    levels = c("apple", "orange", "banana")
  )

  expect_error(print(result), NA)
})

test_that("same_factor summary method works", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    levels = c("apple", "orange", "banana")
  )

  expect_error(summary(result), NA)
})

test_that("same_factor works with multiple methods", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    method = c("exact", "order"),
    levels = c("apple", "orange", "banana"),
    ordered = TRUE
  )

  expect_true(inherits(result, "samesies::similar_factor"))
  expect_true(all(c("exact", "order") %in% result@methods))
})

test_that("same_factor handles nested structures", {
  nested_cats1 <- list(
    fruits = list("apple", "orange", "banana"),
    colors = list("red", "blue", "green")
  )
  nested_cats2 <- list(
    fruits = list("apple", "grape", "banana"),
    colors = list("red", "blue", "yellow")
  )

  result <- same_factor(nested_cats1, nested_cats2,
    levels = c(
      "apple", "orange", "banana", "grape",
      "red", "blue", "green", "yellow"
    )
  )

  expect_true(inherits(result, "samesies::similar_factor"))
  expect_true("fruits_nested_cats1_nested_cats2" %in% names(result@scores$exact))
  expect_true("colors_nested_cats1_nested_cats2" %in% names(result@scores$exact))
})


test_that("average_similarity works for same_factor", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    levels = c("apple", "orange", "banana")
  )

  avgs <- average_similarity(result)
  expect_type(avgs, "double")
  expect_named(avgs, "exact") 
  expect_length(avgs, 1)
  expect_true(all(avgs >= 0 & avgs <= 1))
})

test_that("pair_averages works for same_factor", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    levels = c("apple", "orange", "banana")
  )

  pairs <- pair_averages(result)
  expect_s3_class(pairs, "data.frame")
  expect_named(pairs, c("method", "pair", "avg_score"))
  expect_true(all(pairs$method == "exact"))
  expect_true(all(pairs$avg_score >= 0 & pairs$avg_score <= 1))
})
