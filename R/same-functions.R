#' Compare Text Similarity Across Multiple Lists
#'
#' @param ... Lists of character strings to compare
#' @param method Character vector of similarity methods. Choose from: "osa",
#'   "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"
#'   (default: "jw")
#' @param q Size of q-gram for q-gram based methods (default: 1)
#' @param p Winkler scaling factor for "jw" method (default: 0.1)
#' @param bt Booth matching threshold
#' @param weight Vector of weights for operations: deletion (d), insertion (i),
#'   substitution (s), transposition (t)
#'
#' @return A similarity object for text data that works with print(), summary(), 
#'   average_similarity(), pair_averages(), and plot() methods.
#'
#' @examples
#' list1 <- list("hello", "world")
#' list2 <- list("helo", "word")
#' result <- same_text(list1, list2, method = "jw")
#' print(result)
#'
#' # Compare with multiple methods
#' result2 <- same_text(list1, list2, method = c("jw", "lv"))
#'
#' # Compare multiple lists
#' list3 <- list("hllo", "wrld")
#' result3 <- same_text(list1, list2, list3, method = "jw")
#'
#' # Using nested list structure
#' nested1 <- list(list("hello", "beautiful"), list("big", "world"))
#' nested2 <- list(list("helo", "beauty"), list("large", "earth"))
#' nested_result <- same_text(nested1, nested2, method = "jw")
#' @export
same_text <- function(..., method = "jw", q = 1, p = NULL, bt = 0,
                      weight = c(d = 1, i = 1, s = 1, t = 1)) {
  valid_methods <- c(
    "osa", "lv", "dl", "hamming", "lcs", "qgram",
    "cosine", "jaccard", "jw", "soundex"
  )

  inputs <- list(...)

  if (length(inputs) < 2) {
    cli::cli_abort("At least two inputs required")
  }

  is_valid_element <- function(x) {
    if (is.list(x)) {
      return(all(vapply(x, is_valid_element, logical(1))))
    } else {
      return(is.character(x) && length(x) == 1)
    }
  }

  is_valid_list <- function(x) {
    if (!is.list(x)) {
      return(FALSE)
    }
    return(all(vapply(x, is_valid_element, logical(1))))
  }

  invalid_inputs <- which(!vapply(inputs, is_valid_list, logical(1)))

  if (length(invalid_inputs) > 0) {
    cli::cli_abort(c(
      "All inputs must be lists containing only character strings",
      "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
    ))
  }

  method <- unique(if (length(method) == 1) c(method) else method)

  invalid_methods <- method[!method %in% valid_methods]
  if (length(invalid_methods) > 0) {
    cli::cli_abort(c(
      "All methods must be one of: {paste(valid_methods, collapse = ', ')}"
    ))
  }

  dots <- as.list(substitute(list(...)))[-1]
  list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    names(dots)
  } else {
    purrr::map_chr(dots, ~ deparse(.x)[1])
  }

  flatten_list <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    unlist(lapply(x, flatten_list))
  }

  flattened_inputs <- lapply(inputs, flatten_list)

  lengths <- purrr::map_int(flattened_inputs, length)
  if (length(unique(lengths)) > 1) {
    cli::cli_abort("All lists must have same length after flattening")
  }

  pairs <- get_pairwise_combinations(length(flattened_inputs))

  scores <- purrr::map(method, function(m) {
    pair_scores <- purrr::map2(pairs$first, pairs$second, function(idx1, idx2) {
      pair_name <- paste0(list_names[idx1], "_", list_names[idx2])

      pair_result <- calculate_scores(
        flattened_inputs[[idx1]],
        flattened_inputs[[idx2]],
        method = m,
        q = q,
        p = p,
        bt = bt,
        weight = weight
      )

      mean_score <- round(mean(pair_result), 3)
      cli::cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")

      pair_result
    })

    names(pair_scores) <- purrr::map2_chr(
      pairs$first,
      pairs$second,
      ~ paste0(list_names[.x], "_", list_names[.y])
    )

    pair_scores
  })

  names(scores) <- method

  summaries <- purrr::map(method, function(m) {
    purrr::map(scores[[m]], function(pair_scores) {
      list(
        mean = mean(pair_scores),
        median = stats::median(pair_scores),
        sd = stats::sd(pair_scores),
        min = min(pair_scores),
        max = max(pair_scores),
        q1 = stats::quantile(pair_scores, 0.25),
        q3 = stats::quantile(pair_scores, 0.75),
        iqr = stats::IQR(pair_scores)
      )
    })
  })

  names(summaries) <- method

  result <- similar_text(
    scores = scores,
    summary = summaries,
    methods = method,
    list_names = list_names
  )
  
  # Add class to support plot method
  class(result) <- c(class(result), "similar")
  
  return(result)
}

# This function has been moved to same-factor.R

#' Compare Number Similarity Across Multiple Lists
#'
#' @param ... Lists of numeric values to compare
#' @param method Character vector of similarity methods. Choose from:
#'   "exact", "percent_diff", "normalized", "fuzzy", "rank" 
#'   (default: c("percent_diff", "fuzzy"))
#' @param epsilon Threshold for fuzzy matching. NULL for auto-calculation
#' @param max_diff Maximum difference for normalization. NULL for auto-calculation
#'
#' @return A similarity object for numeric data that works with print(), summary(), 
#'   average_similarity(), pair_averages(), and plot() methods.
#'
#' @examples
#' list1 <- list(1, 2, 3, 4, 5)
#' list2 <- list(1.1, 2.2, 3, 4.1, 5)
#' result <- same_number(list1, list2, method = "fuzzy")
#' print(result)
#'
#' # Compare with multiple methods
#' result2 <- same_number(list1, list2, method = c("exact", "fuzzy", "normalized"))
#'
#' # Compare multiple lists
#' list3 <- list(1, 2.1, 3.2, 4, 5.1)
#' result3 <- same_number(list1, list2, list3, method = "normalized")
#'
#' # Using nested list structure
#' nested1 <- list(list(1, 2), list(3, 4))
#' nested2 <- list(list(1.1, 2.2), list(3, 4.1))
#' nested_result <- same_number(nested1, nested2)
#' @export
same_number <- function(..., method = c("percent_diff", "fuzzy"), epsilon = NULL, max_diff = NULL) {
  valid_methods <- c(
    "exact", "percent_diff", "normalized", "fuzzy", "rank"
  )

  inputs <- list(...)
  validate_number_inputs(...)

  method <- unique(if (length(method) == 1) c(method) else method)

  invalid_methods <- method[!method %in% valid_methods]
  if (length(invalid_methods) > 0) {
    cli::cli_abort(c(
      "All methods must be one of: {paste(valid_methods, collapse = ', ')}"
    ))
  }

  dots <- as.list(substitute(list(...)))[-1]
  list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    names(dots)
  } else {
    purrr::map_chr(dots, ~ deparse(.x)[1])
  }

  flatten_list <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    unlist(lapply(x, flatten_list))
  }

  flattened_inputs <- lapply(inputs, flatten_list)

  all_values <- unlist(flattened_inputs)

  if (is.null(epsilon)) {
    epsilon <- auto_epsilon(all_values)
    cli::cli_alert_info("Using auto-calculated epsilon: {.val {round(epsilon, 5)}}")
  }

  if (is.null(max_diff) && "normalized" %in% method) {
    max_diff <- auto_max_diff(all_values)
    cli::cli_alert_info("Using auto-calculated max_diff: {.val {round(max_diff, 5)}}")
  }

  lengths <- purrr::map_int(flattened_inputs, length)
  if (length(unique(lengths)) > 1) {
    cli::cli_abort("All lists must have same length after flattening")
  }

  pairs <- get_pairwise_combinations(length(flattened_inputs))

  scores <- purrr::map(method, function(m) {
    pair_scores <- purrr::map2(pairs$first, pairs$second, function(idx1, idx2) {
      pair_name <- paste0(list_names[idx1], "_", list_names[idx2])

      pair_result <- calculate_number_scores(
        flattened_inputs[[idx1]],
        flattened_inputs[[idx2]],
        method = m,
        epsilon = epsilon,
        max_diff = max_diff
      )

      mean_score <- round(mean(pair_result), 3)
      cli::cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")

      pair_result
    })

    names(pair_scores) <- purrr::map2_chr(
      pairs$first,
      pairs$second,
      ~ paste0(list_names[.x], "_", list_names[.y])
    )

    pair_scores
  })

  names(scores) <- method

  summaries <- purrr::map(method, function(m) {
    purrr::map(scores[[m]], function(pair_scores) {
      list(
        mean = mean(pair_scores),
        median = stats::median(pair_scores),
        sd = stats::sd(pair_scores),
        min = min(pair_scores),
        max = max(pair_scores),
        q1 = stats::quantile(pair_scores, 0.25),
        q3 = stats::quantile(pair_scores, 0.75),
        iqr = stats::IQR(pair_scores)
      )
    })
  })

  names(summaries) <- method

  result <- similar_number(
    scores = scores,
    summary = summaries,
    methods = method,
    list_names = list_names,
    raw_values = flattened_inputs
  )
  
  # Add class to support plot method
  class(result) <- c(class(result), "similar")
  
  return(result)
}