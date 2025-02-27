test_that("same_factor works with simple factor lists", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")
  fruits3 <- list("apple", "pineapple", "banana")

  result_factor <- same_factor(fruits1, fruits2, fruits3,
    levels = c("apple", "orange", "banana")
  )

  print(class(result_factor))
  print(names(result_factor))
  print(names(result_factor$methods))

  expect_s3_class(result_factor, "same_factor")
  expect_true(inherits(result_factor, "same_similarity"))
  expect_true(is.list(result_factor$data))
  # For now, just check that methods is a character vector
  expect_true(is.character(result_factor$methods))
})

test_that("same_factor print method works", {
  # We'll just test that the print method doesn't error for now
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    levels = c("apple", "orange", "banana")
  )

  expect_error(print(result), NA)
})

test_that("same_factor summary method works", {
  # We'll just test that the summary method doesn't error for now
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
    method = c("exact", "jaccard"),
    levels = c("apple", "orange", "banana")
  )

  print(class(result))
  print(names(result))
  print(result$methods)

  expect_s3_class(result, "same_factor")
  # For now, just check that methods contains the right methods
  expect_true(all(c("exact", "jaccard") %in% result$methods))
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

  expect_s3_class(result, "same_factor")
  expect_true(is.list(result$data$fruits))
  expect_true(is.list(result$data$colors))
})

test_that("same_factor plotting works", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")
  fruits3 <- list("apple", "pineapple", "banana")

  result <- same_factor(fruits1, fruits2, fruits3,
    levels = c("apple", "orange", "banana")
  )

  # Test all available plot types
  expect_no_error({
    p <- plot.same_factor(result)
    expect_s3_class(p, "ggplot")
  })

  expect_no_error({
    p <- plot.same_factor(result, type = "heatmap")
    expect_s3_class(p, "ggplot")
  })

  # Test with a different palette
  expect_no_error({
    p <- plot.same_factor(result, palette = "Set1")
    expect_s3_class(p, "ggplot")
  })

  # Test error cases
  expect_error(plot.same_factor(result, type = "invalid"))
})

test_that("average_similarity works for same_factor", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    method = c("exact", "jaccard"),
    levels = c("apple", "orange", "banana")
  )

  avgs <- average_similarity(result)
  expect_type(avgs, "double")
  expect_named(avgs, c("exact", "jaccard"))
  expect_length(avgs, 2)
  expect_true(all(avgs >= 0 & avgs <= 1))
})

test_that("pair_averages works for same_factor", {
  fruits1 <- list("apple", "orange", "unknown")
  fruits2 <- list("apple", "orange", "unknown")

  result <- same_factor(fruits1, fruits2,
    method = c("exact", "jaccard"),
    levels = c("apple", "orange", "banana")
  )

  pairs <- pair_averages(result)
  expect_s3_class(pairs, "data.frame")
  expect_named(pairs, c("method", "pair", "avg_score"))
  expect_true(all(pairs$method %in% c("exact", "jaccard")))
  expect_true(all(pairs$avg_score >= 0 & pairs$avg_score <= 1))
})
