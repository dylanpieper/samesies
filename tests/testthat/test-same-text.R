test_that("same_text works with simple text lists", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result1 <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))
  result2 <- same_text(fruits1, fruits2)

  expect_true(inherits(result1, "samesies::similar_text"))
  expect_true(inherits(result2, "samesies::similar_text"))

  expect_true(inherits(result1, "samesies::similar"))
  expect_true(is.list(result1@scores))
  expect_true(all(c("jw", "lv") %in% result1@methods))
  expect_length(result2@methods, 10) # All methods used by default
})

test_that("same_text print method works", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")

  result <- same_text(fruits1, fruits2, method = c("jw", "lv"))

  # Instead of testing output, test that the function returns without error
  expect_error(print(result), NA)
})

test_that("same_text summary method works", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")

  result <- same_text(fruits1, fruits2, method = c("jw", "lv"))

  # Instead of testing output, test that the function returns without error
  expect_error(summary(result), NA)
})

test_that("same_text handles nested structures", {
  nested_fruits1 <- list(
    citrus = list("orange", "lemon", "lime"),
    berries = list("strawberry", "blueberry", "raspberry")
  )
  nested_fruits2 <- list(
    citrus = list("ornge", "lemmon", "lime"),
    berries = list("strawbery", "blueberry", "respberry")
  )
  nested_fruits3 <- list(
    citrus = list("oranges", "lemons", "limes"),
    berries = list("strawberries", "blueberries", "raspberries")
  )

  result3 <- same_text(nested_fruits1, nested_fruits2, method = "jw")
  result4 <- same_text(nested_fruits1, nested_fruits2, nested_fruits3,
    method = c("jw", "lv")
  )

  expect_true(inherits(result3, "samesies::similar_text"))
  expect_true(inherits(result4, "samesies::similar_text"))

  # Check that the scores and methods are present
  expect_true("jw" %in% names(result3@scores))
  expect_true(is.list(result3@scores$jw))
  expect_true(length(result3@scores$jw) > 0)
})

test_that("same_text works with method filtering", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  # Test method filtering with pair_averages
  jw_pairs <- pair_averages(result, method = "jw")
  expect_s3_class(jw_pairs, "data.frame")
  expect_true(all(jw_pairs$method == "jw"))
  # Check the pairs - format is now "fruits1_fruits2" etc.
  expect_true(all(jw_pairs$pair %in% c("fruits1_fruits2", "fruits1_fruits3", "fruits2_fruits3")))
  expect_true(all(jw_pairs$avg_score >= 0 & jw_pairs$avg_score <= 1))
})

test_that("same_text utility functions work with multiple methods", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  # Test average_similarity function
  avg_sim <- average_similarity(result)
  expect_type(avg_sim, "double")
  expect_named(avg_sim, c("jw", "lv"))
  expect_true(all(avg_sim >= 0 & avg_sim <= 1))

  # Test pair_averages function with all methods
  pairs <- pair_averages(result)
  expect_s3_class(pairs, "data.frame")
  expect_named(pairs, c("method", "pair", "avg_score"))
  expect_true(all(pairs$method %in% c("jw", "lv")))
  expect_true(all(pairs$avg_score >= 0 & pairs$avg_score <= 1))

  # Test pair_averages with specific method
  jw_pairs <- pair_averages(result, method = "jw")
  expect_s3_class(jw_pairs, "data.frame")
  expect_true(all(jw_pairs$method == "jw"))
  expect_true(all(jw_pairs$avg_score >= 0 & jw_pairs$avg_score <= 1))
})
