#' Calculate Numeric Similarity
#' @param num1 First numeric value to compare
#' @param num2 Second numeric value to compare
#' @param method Method to use for similarity calculation. One of: "exact", "percent_diff", "normalized", "fuzzy"
#' @param epsilon Threshold for fuzzy matching. Only used when method is "fuzzy"
#' @param max_diff Maximum difference for normalization. Only used when method is "normalized"
#' @return Numeric similarity score between 0 and 1
calculate_number_similarity <- function(num1, num2, method, epsilon = 0.05, max_diff = NULL) {
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
    "percent_diff" = {
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

      relative_epsilon <- larger_val * 0.02
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
#' @param method Method to use for similarity calculation. One of: "exact", "percent_diff", "normalized", "fuzzy"
#' @param epsilon Threshold for fuzzy matching. Only used when method is "fuzzy"
#' @param max_diff Maximum difference for normalization. Only used when method is "normalized"
#' @return Vector of numeric similarity scores between 0 and 1
#' @keywords internal
calculate_number_scores <- function(list1, list2, method, epsilon = 0.05, max_diff = NULL) {
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
        max_diff = max_diff
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

#' Compare Numeric Lists for Similarity
#' @description Computes similarity scores between two or more lists of numeric values using multiple comparison methods.
#'
#' @param ... Two or more lists containing numeric values to compare
#' @param method Character vector specifying similarity methods (default: all)
#' @param epsilon Threshold for fuzzy matching (default: NULL for auto-calculation)
#' @param max_diff Maximum difference for normalization (default: NULL for auto-calculation)
#'
#' @return An S7 object containing:
#'   \itemize{
#'     \item \code{scores}: A list of similarity scores for each method and list pair
#'     \item \code{summary}: A list of statistical summaries for each method and list pair
#'     \item \code{methods}: The similarity methods used
#'     \item \code{list_names}: Names of the input lists
#'     \item \code{raw_values}: The original input lists
#'   }
#'
#' @examples
#' nums1 <- list(1, 2, 3)
#' nums2 <- list(1, 2.1, 3.2)
#' result <- same_number(nums1, nums2)
#' @export
same_number <- function(..., method = c("exact", "percent_diff", "normalized", "fuzzy"),
                        epsilon = NULL, max_diff = NULL) {
  valid_methods <- c(
    "exact", "percent_diff", "normalized", "fuzzy"
  )

  inputs <- list(...)
  validate_number_inputs(...)

  method <- unique(if (length(method) == 1) c(method) else method)

  invalid_methods <- method[!method %in% valid_methods]
  if (length(invalid_methods) > 0) {
    cli_abort(c(
      "All methods must be one of: {paste(valid_methods, collapse = ', ')}"
    ))
  }

  dots <- as.list(substitute(list(...)))[-1]
  list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    names(dots)
  } else {
    map_chr(dots, ~ deparse(.x)[1])
  }

  has_nested <- FALSE
  for (i in seq_along(inputs)) {
    if (length(names(inputs[[i]])) > 0) {
      has_nested <- TRUE
      break
    }
  }

  if (has_nested) {
    all_keys <- unique(unlist(lapply(inputs, names)))
    key_epsilons <- list()
    key_max_diffs <- list()

    if (is.null(epsilon) || (is.null(max_diff) && "normalized" %in% method)) {
      for (key in all_keys) {
        key_values <- unlist(lapply(inputs, function(x) unlist(x[[key]])))

        if (is.null(epsilon) && "fuzzy" %in% method) {
          key_epsilons[[key]] <- auto_epsilon(key_values)
          cli_alert_info("Using auto-calculated epsilon for {.val {key}}: {.val {round(key_epsilons[[key]], 5)}}")
        }

        if (is.null(max_diff) && "normalized" %in% method) {
          key_max_diffs[[key]] <- auto_max_diff(key_values)
          cli_alert_info("Using auto-calculated max_diff for {.val {key}}: {.val {round(key_max_diffs[[key]], 5)}}")
        }
      }
    }

    scores <- list()
    summaries <- list()

    for (m in method) {
      scores[[m]] <- list()
      summaries[[m]] <- list()
    }

    for (key in all_keys) {
      key_lists <- lapply(inputs, function(x) {
        if (!is.null(x[[key]])) x[[key]] else list()
      })

      key_lists <- key_lists[sapply(key_lists, length) > 0]
      if (length(key_lists) < 2) next

      key_epsilon <- if (!is.null(epsilon)) epsilon else key_epsilons[[key]]
      key_max_diff <- if (!is.null(max_diff)) max_diff else key_max_diffs[[key]]

      pairs <- get_pairwise_combinations(length(key_lists))

      for (m in method) {
        for (i in seq_along(pairs$first)) {
          idx1 <- pairs$first[i]
          idx2 <- pairs$second[i]

          pair_name <- paste0(key, "_", list_names[idx1], "_", list_names[idx2])

          pair_result <- calculate_number_scores(
            key_lists[[idx1]],
            key_lists[[idx2]],
            method = m,
            epsilon = key_epsilon,
            max_diff = key_max_diff
          )

          mean_score <- round(mean(pair_result), 3)
          cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")

          scores[[m]][[pair_name]] <- pair_result

          summaries[[m]][[pair_name]] <- list(
            mean = mean(pair_result),
            median = stats::median(pair_result),
            sd = stats::sd(pair_result),
            min = min(pair_result),
            max = max(pair_result),
            q1 = stats::quantile(pair_result, 0.25),
            q3 = stats::quantile(pair_result, 0.75),
            iqr = stats::IQR(pair_result)
          )
        }
      }
    }

    raw_values <- inputs
  } else {
    flattened_inputs <- lapply(inputs, flatten_list)

    all_values <- unlist(flattened_inputs)

    if (is.null(epsilon) && "fuzzy" %in% method) {
      epsilon <- auto_epsilon(all_values)
      cli_alert_info("Using auto-calculated epsilon: {.val {round(epsilon, 5)}}")
    }

    if (is.null(max_diff) && "normalized" %in% method) {
      max_diff <- auto_max_diff(all_values)
      cli_alert_info("Using auto-calculated max_diff: {.val {round(max_diff, 5)}}")
    }

    lengths <- map_int(flattened_inputs, length)
    if (length(unique(lengths)) > 1) {
      cli_abort("All lists must have same length after flattening")
    }

    pairs <- get_pairwise_combinations(length(flattened_inputs))

    scores <- map(method, function(m) {
      pair_scores <- map2(pairs$first, pairs$second, function(idx1, idx2) {
        pair_name <- paste0(list_names[idx1], "_", list_names[idx2])

        pair_result <- calculate_number_scores(
          flattened_inputs[[idx1]],
          flattened_inputs[[idx2]],
          method = m,
          epsilon = epsilon,
          max_diff = max_diff
        )

        mean_score <- round(mean(pair_result), 3)
        cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")

        pair_result
      })

      names(pair_scores) <- map2_chr(
        pairs$first,
        pairs$second,
        ~ paste0(list_names[.x], "_", list_names[.y])
      )

      pair_scores
    })

    names(scores) <- method
    raw_values <- flattened_inputs
  }

  summaries <- map(method, function(m) {
    map(scores[[m]], function(pair_scores) {
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

  similar_number(
    scores = scores,
    summary = summaries,
    methods = method,
    list_names = list_names,
    raw_values = raw_values
  )
}
