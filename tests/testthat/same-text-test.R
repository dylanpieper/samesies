library(testthat)

test_data <- list(
  fruits = list(
    list1 = list(
      "apple", "banana", "orange", "grapefruit", "pineapple",
      "mango", "strawberry", "blueberry", "raspberry", "blackberry",
      "kiwi", "lemon", "lime", "peach", "pear",
      "plum", "grape", "watermelon", "cantaloupe", "pomegranate"
    ),
    list2 = list(
      "appel", "bannana", "orange", "grape fruit", "pine apple",
      "mango", "strawbery", "blueberries", "rasberry", "blackberries",
      "kiwifruit", "lemmon", "lime", "peech", "pair",
      "plumb", "grapes", "water melon", "cantalope", "pomegranet"
    ),
    list3 = list(
      "appels", "banannas", "oranges", "grapefruits", "pineapples",
      "mangos", "strawberries", "blueberrys", "rasberries", "blackberrys",
      "kiwis", "lemons", "limes", "peaches", "pears",
      "plums", "graps", "watermelons", "cantaloupes", "pomegranates"
    )
  ),
  empty = list(
    list1 = list("", "text", ""),
    list2 = list("", "", "word"),
    list3 = list("", "text", "word")
  ),
  special = list(
    list1 = list("Hello!", "123", "Test-Case", "MultiWord String"),
    list2 = list("hello!", "1234", "testcase", "multi word string"),
    list3 = list("HELLO!", "12345", "test_case", "multiword_string")
  )
)

test_that("same_text validates input correctly", {
  expect_error(
    same_text("single_string"),
    "At least two inputs required"
  )

  expect_error(
    same_text(list("a"), list("b", "c")),
    "All lists must have same length"
  )

  expect_error(
    same_text(list("a"), list("b"), method = "invalid"),
    "All methods must be one of: osa, lv, dl, hamming, lcs, qgram, cosine, jaccard, jw, soundex"
  )
})

test_that("same_text handles multiple lists correctly", {
  list1 <- list("apple", "banana", "orange")
  list2 <- list("appel", "banana", "orange")
  list3 <- list("appels", "bananas", "oranges")

  result <- same_text(list1, list2, list3, method = "jw")

  expect_s3_class(result, "S7_object")
  expect_length(result@methods, 1)
  expect_length(result@scores$jw, 3)
  expect_length(result@summary$jw, 3)

  pair_names <- names(result@scores$jw)
  expect_equal(
    sort(pair_names),
    sort(c("list1_list2", "list1_list3", "list2_list3"))
  )

  result_named <- same_text(
    original = test_data$fruits$list1,
    typos = test_data$fruits$list2,
    plural = test_data$fruits$list3,
    method = "jw"
  )

  pair_names_named <- names(result_named@scores$jw)
  expect_equal(
    sort(pair_names_named),
    sort(c("original_typos", "original_plural", "typos_plural"))
  )

  multi_result <- same_text(
    list1,
    list2,
    list3,
    method = c("jw", "lv")
  )

  expect_length(multi_result@methods, 2)
  expect_length(multi_result@scores$jw, 3)
  expect_length(multi_result@scores$lv, 3)
})

test_that("same_text handles all stringdist methods correctly", {
  methods <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")

  for (method in methods) {
    result <- same_text(test_data$fruits$list1, test_data$fruits$list2, method = method)

    expect_s3_class(result, "S7_object")
    expect_true(method %in% names(result@scores))
    expect_true(method %in% names(result@summary))

    expect_true(all(sapply(result@scores[[method]], function(x) all(x >= 0 & x <= 1))))
    expect_true(all(sapply(result@summary[[method]], function(x) x$sd >= 0)))
    expect_true(all(sapply(result@summary[[method]], function(x) {
      x$mean >= 0 & x$mean <= 1
    })))
  }
})

test_that("same_text handles method-specific parameters correctly", {
  qgram_result <- same_text(
    test_data$fruits$list1,
    test_data$fruits$list2,
    method = "qgram",
    q = 2
  )
  expect_true(all(sapply(qgram_result@scores$qgram, function(x) all(x >= 0 & x <= 1))))

  jw_result <- same_text(
    test_data$fruits$list1,
    test_data$fruits$list2,
    method = "jw",
    p = 0.15
  )
  expect_true(all(sapply(jw_result@scores$jw, function(x) all(x >= 0 & x <= 1))))

  osa_result <- same_text(
    test_data$fruits$list1,
    test_data$fruits$list2,
    method = "osa",
    weight = c(d = 0.5, i = 0.5, s = 1, t = 1)
  )
  expect_true(all(sapply(osa_result@scores$osa, function(x) all(x >= 0 & x <= 1))))
})

test_that("same_text handles edge cases correctly", {
  empty_result <- same_text(test_data$empty$list1, test_data$empty$list2)
  expect_true(all(sapply(empty_result@scores[[1]], function(x) all(x >= 0 & x <= 1))))

  special_result <- same_text(test_data$special$list1, test_data$special$list2)
  expect_true(all(sapply(special_result@scores[[1]], function(x) all(x >= 0 & x <= 1))))

  multi_method_result <- same_text(
    test_data$fruits$list1,
    test_data$fruits$list2,
    method = c("jw", "lv", "cosine")
  )
  expect_equal(length(multi_method_result@methods), 3)
  expect_equal(length(multi_method_result@scores), 3)
})

test_that("same_text produces expected similarity scores", {
  result <- same_text(test_data$fruits$list1, test_data$fruits$list2,
    method = c("jw", "lv", "cosine")
  )

  pair_name <- names(result@summary$jw)[1]
  expect_equal(round(result@summary$jw[[pair_name]]$mean, 3), 0.955)
  expect_equal(round(result@summary$lv[[pair_name]]$mean, 3), 0.819)
  expect_equal(round(result@summary$cosine[[pair_name]]$mean, 3), 0.936)

  perfect_match <- same_text(list("test"), list("test"), method = c("jw", "lv", "cosine"))
  for (m in c("jw", "lv", "cosine")) {
    pair_name <- names(perfect_match@scores[[m]])[1]
    expect_equal(unname(perfect_match@scores[[m]][[pair_name]][1]), 1)
  }

  complete_mismatch <- same_text(list("abcd"), list("efgh"), method = c("jw", "lv", "cosine"))
  for (m in c("jw", "lv", "cosine")) {
    pair_name <- names(complete_mismatch@scores[[m]])[1]
    scores <- complete_mismatch@scores[[m]][[pair_name]]
    expect_true(all(unname(scores) == 0))
  }
})

test_that("same_text handles duplicates correctly", {
  dup_list1 <- list("test", "test", "test")
  dup_list2 <- list("test", "tset", "teste")
  dup_list3 <- list("test", "sets", "tester")

  dup_result <- same_text(dup_list1, dup_list2, dup_list3, method = c("jw", "lv"))

  pair_name_1_2 <- "dup_list1_dup_list2"
  expect_equal(unname(dup_result@scores$jw[[pair_name_1_2]][1]), 1)
  expect_equal(unname(dup_result@scores$lv[[pair_name_1_2]][1]), 1)
  expect_true(all(unname(dup_result@scores$jw[[pair_name_1_2]][2:3]) < 1))
  expect_true(all(unname(dup_result@scores$lv[[pair_name_1_2]][2:3]) < 1))

  pair_name_1_3 <- "dup_list1_dup_list3"
  expect_equal(unname(dup_result@scores$jw[[pair_name_1_3]][1]), 1)
  expect_equal(unname(dup_result@scores$lv[[pair_name_1_3]][1]), 1)
  expect_true(all(unname(dup_result@scores$jw[[pair_name_1_3]][2:3]) < 1))
  expect_true(all(unname(dup_result@scores$lv[[pair_name_1_3]][2:3]) < 1))
})
