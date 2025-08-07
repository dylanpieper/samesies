#' Generate All Pairwise Combinations
#'
#' @param n The number of items to generate combinations from
#'
#' @return A list containing two components:
#'   \item{first}{Numeric vector of first indices for each pair}
#'   \item{second}{Numeric vector of second indices for each pair}
#'
#' @noRd
get_pairwise_combinations <- function(n) {
  if (n < 2) cli::cli_abort("Need at least 2 lists to compare")
  combs <- utils::combn(n, 2)
  list(
    first = combs[1, ],
    second = combs[2, ]
  )
}

#' Calculate Mean Scores for Each Method
#'
#' @param scores A named list of scores from similarity methods
#' @param digits Number of digits to round results (default: 3)
#'
#' @return A named numeric vector of mean scores rounded to specified digits
#'
#' @noRd
mean_scores_by_method <- function(scores, digits = 3) {
  result <- purrr::map_dbl(names(scores), function(method) {
    method_scores <- scores[[method]]
    all_scores <- unlist(method_scores)
    mean(all_scores, na.rm = TRUE)
  })

  names(result) <- names(scores)
  round(result, digits)
}

#' Check if a value is valid
#'
#' @param x The value to check
#' @param type The expected type of the value: "text", "number", or "factor"
#' @param levels Optional character vector of allowed levels (for factor type)
#'
#' @return Logical indicating if the value is valid
#' @noRd
is_valid <- function(x, type = c("text", "number", "factor"), levels = NULL) {
  type <- match.arg(type)

  if (is.list(x)) {
    return(TRUE)
  }

  switch(type,
    "text" = {
      is.character(x) && length(x) == 1
    },
    "number" = {
      is.numeric(x) || is.na(x)
    },
    "factor" = {
      (is.character(x) || is.factor(x) || is.na(x))
    }
  )
}

#' Check if a list contains only valid values
#'
#' @param x The list to check
#' @param type The expected type of the values: "text", "number", or "factor"
#' @param levels Optional character vector of allowed levels (for factor type)
#'
#' @return Logical indicating if the list contains only valid values
#' @noRd
is_valid_list <- function(x, type = c("text", "number", "factor"), levels = NULL) {
  type <- match.arg(type)

  check_element <- function(y) {
    if (is.list(y)) {
      return(all(vapply(y, check_element, logical(1))))
    }

    switch(type,
      "text" = {
        is.character(y) && length(y) == 1
      },
      "number" = {
        is.numeric(y) || is.na(y)
      },
      "factor" = {
        (is.character(y) || is.factor(y) || is.na(y))
      }
    )
  }

  is.list(x) && all(vapply(x, check_element, logical(1)))
}

#' Check if a value is a character list
#'
#' @param x The value to check
#'
#' @return Logical indicating if the value is a list containing only character strings
#' @noRd
is_char_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  return(all(vapply(x, function(y) is.character(y) && length(y) == 1, logical(1))))
}

#' Validate Text Input Lists
#'
#' @param ... Lists of character strings to validate
#'
#' @return TRUE if all inputs are valid, error message otherwise
#'
#' @details
#' Checks that:
#' - At least 2 inputs are provided
#' - All inputs are lists containing only character strings
#'
#' @noRd
validate_text_inputs <- function(...) {
  inputs <- list(...)

  if (length(inputs) < 2) {
    cli::cli_abort("At least two inputs required")
  }

  invalid_inputs <- which(!vapply(inputs, is_char_list, logical(1)))

  if (length(invalid_inputs) > 0) {
    cli::cli_abort(c(
      "All inputs must be lists containing only character strings",
      "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
    ))
  }

  TRUE
}

#' Calculate String Similarity Score
#'
#' @param str1 First string to compare
#' @param str2 Second string to compare
#' @param method Method for comparison: "osa", "lv", "dl", "hamming", "lcs", "qgram",
#'   "cosine", "jaccard", "jw", or "soundex"
#' @param q Size of q-gram for q-gram based methods (default: 1)
#' @param p Winkler scaling factor for "jw" method (default: 0.1)
#' @param bt Booth matching threshold
#' @param weight Vector of weights for operations (deletion, insertion, substitution, transposition)
#'
#' @return Normalized similarity score between 0 and 1
#' @noRd
calculate_text_similarity <- function(str1, str2, method, q = 1, p = NULL, bt = 0,
                                      weight = c(d = 1, i = 1, s = 1, t = 1)) {
  if (is.null(p)) {
    p <- if (method == "jw") 0.1 else 0
  }

  if (method == "jw" && p > 0.25) {
    cli::cli_abort("For Jaro-Winkler (jw) method, p must be <= 0.25")
  }

  str1 <- as.character(str1)
  str2 <- as.character(str2)

  if (str1 == "" && str2 == "") {
    return(1)
  }
  if (str1 == "" || str2 == "") {
    return(0)
  }

  dist <- stringdist::stringdist(
    str1,
    str2,
    method = method,
    q = q,
    p = p,
    bt = bt,
    weight = weight
  )

  similarity <- if (method %in% c("osa", "lv", "dl", "hamming", "lcs")) {
    1 - (dist / max(nchar(str1), nchar(str2)))
  } else if (method == "qgram") {
    1 - (dist / (nchar(str1) + nchar(str2)))
  } else {
    1 - dist
  }

  pmin(1, pmax(0, similarity))
}

#' Calculate Similarity Scores Between Two Lists
#'
#' @param list1 First list of strings to compare
#' @param list2 Second list of strings to compare
#' @param method Method for comparison (e.g., "jw", "lv")
#' @param ... Additional arguments passed to calculate_text_similarity()
#'
#' @return Named numeric vector of similarity scores
#' @noRd
calculate_text_scores <- function(list1, list2, method, ...) {
  if (length(list1) != length(list2)) {
    cli::cli_abort("All lists must have same length")
  }

  scores <- purrr::map2_dbl(
    list1,
    list2,
    function(x, y) calculate_text_similarity(x, y, method = method, ...)
  )

  names(scores) <- unlist(list1)
  scores
}

#' Validate Factor Input Lists
#'
#' @param ... Lists of character or factor values to validate
#' @param levels Character vector of all allowed levels
#'
#' @return TRUE if all inputs are valid, error message otherwise
#' @noRd
validate_factor_inputs <- function(..., levels = NULL) {
  inputs <- list(...)

  if (length(inputs) < 2) {
    cli::cli_abort("At least two inputs required")
  }

  invalid_inputs <- which(!vapply(inputs, function(x) is_valid_list(x, "factor"), logical(1)))

  if (length(invalid_inputs) > 0) {
    cli::cli_abort(c(
      "All inputs must be lists containing only character or factor values",
      "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
    ))
  }

  TRUE
}

#' Calculate Factor Similarity Score
#'
#' @param cat1 First categorical value to compare
#' @param cat2 Second categorical value to compare
#' @param method Method for comparison: "exact", "order"
#' @param levels Character vector of all possible levels
#'
#' @return Normalized similarity score between 0 and 1
#' @noRd
calculate_factor_similarity <- function(cat1, cat2, method, levels) {
  cat1 <- as.character(cat1)
  cat2 <- as.character(cat2)

  if (!is.null(levels)) {
    if (!cat1 %in% levels) cat1 <- NA
    if (!cat2 %in% levels) cat2 <- NA
  }

  if (is.na(cat1) && is.na(cat2)) {
    return(1)
  }
  if (is.na(cat1) || is.na(cat2)) {
    return(0)
  }

  switch(method,
    "exact" = {
      as.numeric(cat1 == cat2)
    },
    "order" = {
      if (cat1 == cat2) {
        return(1)
      }
      if (length(levels) == 1) {
        return(1)
      }
      cat1_idx <- match(cat1, levels)
      cat2_idx <- match(cat2, levels)
      1 - abs(cat1_idx - cat2_idx) / (length(levels) - 1)
    }
  )
}

#' Calculate Similarity Scores Between Two Lists of Categorical Values
#'
#' @param list1 First list of categorical values to compare
#' @param list2 Second list of categorical values to compare
#' @param method Method for comparison
#' @param levels Character vector of all possible levels
#'
#' @return Named numeric vector of similarity scores
#' @noRd
calculate_factor_scores <- function(list1, list2, method, levels) {
  if (length(list1) != length(list2)) {
    cli::cli_abort("All lists must have same length")
  }

  scores <- purrr::map2_dbl(
    list1,
    list2,
    function(x, y) calculate_factor_similarity(x, y, method = method, levels = levels)
  )

  names(scores) <- unlist(list1)
  scores
}

#' Validate Numeric List Inputs
#' @description Validates that all inputs are lists containing only numeric values
#' @param ... Lists to validate
#' @return TRUE if validation passes, otherwise throws an error
#' @keywords internal
validate_number_inputs <- function(...) {
  inputs <- list(...)

  if (length(inputs) < 2) {
    cli::cli_abort("At least two inputs required")
  }

  invalid_inputs <- which(!vapply(inputs, function(x) is_valid_list(x, "number"), logical(1)))

  if (length(invalid_inputs) > 0) {
    cli::cli_abort(c(
      "All inputs must be lists containing only numeric values",
      "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
    ))
  }

  TRUE
}

#' Calculate Numeric Similarity
#' @param num1 First numeric value to compare
#' @param num2 Second numeric value to compare
#' @param method Method to use for similarity calculation. One of: "exact", "raw", "exp", "percent", "normalized", "fuzzy"
#' @param epsilon Threshold for fuzzy matching. Only used when method is "fuzzy"
#' @param max_diff Maximum difference for normalization. Only used when method is "normalized"
#' @param epsilon_pct Relative epsilon percentile (default: 0.02 or 2%). Only used when method is "fuzzy"
#' @return Numeric similarity score between 0 and 1, or raw difference value if method is "raw"
#' @noRd
calculate_number_similarity <- function(num1, num2, method, epsilon = 0.05, max_diff = NULL, epsilon_pct = 0.02) {
  if (is.na(num1) && is.na(num2)) {
    return(1)
  }
  if (is.na(num1) || is.na(num2)) {
    return(0)
  }

  num1 <- as.numeric(num1)
  num2 <- as.numeric(num2)

  switch(method,
    "exact" = {
      as.numeric(num1 == num2)
    },
    "raw" = {
      abs(num1 - num2)
    },
    "exp" = {
      diff <- abs(num1 - num2)
      similarity <- exp(-diff)
      similarity
    },
    "percent" = {
      if (num1 == 0 && num2 == 0) {
        return(1)
      }
      if (num1 == 0 || num2 == 0) {
        return(0)
      }

      diff_pct <- abs(num1 - num2) / max(abs(num1), abs(num2))
      similarity <- 1 - min(diff_pct, 1)
      similarity
    },
    "normalized" = {
      diff <- abs(num1 - num2)

      if (is.null(max_diff)) {
        max_diff <- 1
      }

      similarity <- 1 - min(diff / max_diff, 1)
      similarity
    },
    "fuzzy" = {
      abs_diff <- abs(num1 - num2)

      larger_val <- max(abs(num1), abs(num2))

      if (larger_val == 0) {
        return(as.numeric(abs_diff == 0))
      }

      relative_epsilon <- larger_val * epsilon_pct
      absolute_epsilon <- epsilon

      effective_epsilon <- max(relative_epsilon, absolute_epsilon)

      if (abs_diff <= effective_epsilon) {
        return(1)
      } else {
        scaled_diff <- (abs_diff - effective_epsilon) / larger_val
        return(max(0, 1 - scaled_diff))
      }
    }
  )
}

#' Calculate Similarity Scores Between Two Numeric Lists
#' @param list1 First list of numeric values
#' @param list2 Second list of numeric values
#' @param method Method to use for similarity calculation. One of: "exact", "percent", "normalized", "fuzzy", "exp", "raw"
#' @param epsilon Threshold for fuzzy matching. Only used when method is "fuzzy"
#' @param max_diff Maximum difference for normalization. Only used when method is "normalized"
#' @param epsilon_pct Relative epsilon percentile (default: 0.02 or 2%). Only used when method is "fuzzy"
#' @return Vector of numeric similarity scores between 0 and 1
#' @keywords internal
calculate_number_scores <- function(list1, list2, method, epsilon = NULL, max_diff = NULL, epsilon_pct = NULL) {
  if (length(list1) != length(list2)) {
    cli::cli_abort("All lists must have same length")
  }

  if (method == "normalized" && is.null(max_diff)) {
    all_values <- c(unlist(list1), unlist(list2))
    max_diff <- max(abs(max(all_values) - min(all_values)), 1e-10)
  }

  scores <- purrr::map2_dbl(
    list1,
    list2,
    function(x, y) {
      calculate_number_similarity(
        x, y,
        method = method,
        epsilon = epsilon,
        max_diff = max_diff,
        epsilon_pct = epsilon_pct
      )
    }
  )

  scores
}

#' Calculate Epsilon Value for Fuzzy Matching
#' @param values Numeric vector or list of values
#' @param percentile Percentile used for epsilon calculation (default: 0.1)
#' @return Numeric epsilon value appropriate for the scale of data
#' @keywords internal
auto_epsilon <- function(values, percentile = 0.1) {
  values <- unlist(values)
  values <- values[!is.na(values)]

  if (length(values) < 2 || all(values == values[1])) {
    return(0.05)
  }

  sd_val <- stats::sd(values)
  mean_val <- mean(abs(values))
  range_val <- max(values) - min(values)

  epsilon <- sd_val * percentile
  magnitude_component <- mean_val * 0.005
  range_component <- range_val * 0.01

  epsilon <- (epsilon + magnitude_component + range_component) / 3

  min_epsilon <- mean_val * 0.001
  max_epsilon <- mean_val * 0.1

  min_range_epsilon <- range_val * 0.01
  min_epsilon <- max(min_epsilon, min_range_epsilon)

  epsilon <- max(min_epsilon, min(max_epsilon, epsilon))

  return(epsilon)
}

#' Auto-calculate Maximum Difference for Normalization
#' @param values Numeric vector or list of values
#' @return Numeric value for maximum difference in normalization calculations
#' @keywords internal
auto_max_diff <- function(values) {
  values <- unlist(values)
  values <- values[!is.na(values)]

  if (length(values) < 2) {
    return(1)
  }

  range_val <- max(values) - min(values)
  max_diff <- max(range_val, 1e-10)

  return(max_diff)
}

#' Flatten a nested list
#'
#' @param x The list to flatten
#'
#' @return A flattened version of the input list
#' @noRd
flatten_list <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  unlist(lapply(x, flatten_list))
}

#' Calculate Average Similarity Scores
#'
#' @param x A similarity object created by one of the similarity functions
#' @param ... Additional arguments passed to specific methods
#'
#' @return A named numeric vector of mean similarity scores for each method
#'
#' @description
#' Calculates and returns the average similarity score for each method used in the comparison.
#'
#' @export
average_similarity <- function(x, ...) {
  if (inherits(x, "SimilarityBase")) {
    return(x$calc_average_similarity())
  }
  UseMethod("average_similarity")
}

#' Calculate Average Similarity Scores By Pairs
#'
#' @param x A similarity object created by one of the similarity functions
#' @param method Optional character vector of methods to include. If NULL, uses all methods.
#' @param ... Additional arguments passed to specific methods
#'
#' @return A data frame containing:
#'   \item{method}{The similarity method used}
#'   \item{pair}{The pair of lists compared}
#'   \item{avg_score}{Mean similarity score for the pair}
#'
#' @description
#' Calculates and returns the average similarity scores for each pair of lists
#' compared, broken down by method.
#'
#' @export
pair_averages <- function(x, method = NULL, ...) {
  if (inherits(x, "SimilarityBase")) {
    return(x$calc_pair_averages(method))
  }
  UseMethod("pair_averages")
}
