test_that("same_text works with simple text lists", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result1 <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))
  result2 <- same_text(fruits1, fruits2)

  expect_s3_class(result1, "same_text")
  expect_s3_class(result2, "same_text")

  expect_true(inherits(result1, "same_similarity"))
  expect_true(is.list(result1$data))
  expect_true(all(c("jw", "lv") %in% names(result1$methods)))
  expect_length(result2$methods, 2)
})

test_that("same_text print method works", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")

  result <- same_text(fruits1, fruits2, method = c("jw", "lv"))

  expect_output(print(result), "same_text")
  expect_output(print(result), "method")
})

test_that("same_text summary method works", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")

  result <- same_text(fruits1, fruits2, method = c("jw", "lv"))

  expect_output(summary(result), "Summary")
  expect_output(summary(result), "similarity")
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

  expect_s3_class(result3, "same_text")
  expect_s3_class(result4, "same_text")
  expect_true(is.list(result4$data$citrus))
  expect_true(is.list(result4$data$berries))
})

test_that("same_text works with method filtering", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  jw_pairs <- pair_averages(result, method = "jw")
  expect_type(jw_pairs, "list")
  expect_true(all(names(jw_pairs) %in% c("1-2", "1-3", "2-3")))
  expect_true(all(sapply(jw_pairs, function(x) is.numeric(x) && x >= 0 && x <= 1)))
})

test_that("same_text plotting works", {
  fruits1 <- list("apple", "banana", "orange")
  fruits2 <- list("appel", "banana", "orange")
  fruits3 <- list("appels", "bananas", "oranges")

  result <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))

  expect_no_error(plot(result))
  expect_no_error(plot(result, palette = "Set1"))
  expect_no_error(plot(result, palette = "Dark2"))
})
