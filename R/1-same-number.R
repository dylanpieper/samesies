#' Compare Numerical Similarity Across Lists
#' @description Computes similarity scores between two or more lists of numeric values.
#'   Applies various mathematical comparison methods including exact matching, percentage differences,
#'   normalized differences, fuzzy threshold-based matching, and exponential decay metrics.
#'
#' @param ... Two or more lists containing numeric values to compare.
#' @param method Character vector specifying similarity methods
#'   (default: c("exact", "raw", "exp", "percent", "normalized", "fuzzy")).
#' @param epsilon Threshold for fuzzy matching (default: NULL for auto-calculation).
#' @param epsilon_pct Relative epsilon percentile for "fuzzy" method (default: 0.02).
#' @param max_diff Maximum difference for normalization (default: NULL for auto-calculation).
#' @param digits Number of digits to round results (default: 3).
#'
#' @details
#' The available methods are:
#' \itemize{
#'   \item \code{exact}: Binary similarity (1 if equal, 0 otherwise)
#'   \item \code{percent}: Percentage difference relative to the larger value
#'   \item \code{normalized}: Absolute difference normalized by a maximum difference value
#'   \item \code{fuzzy}: Similarity based on an epsilon threshold
#'   \item \code{exp}: Exponential decay based on absolute difference (e^-diff)
#'   \item \code{raw}: Returns the raw absolute difference (|num1 - num2|) instead of a similarity score
#' }
#'
#' @return An S3 object containing:
#'   \itemize{
#'     \item \code{scores}: A list of similarity scores for each method and list pair
#'     \item \code{summary}: A list of statistical summaries for each method and list pair
#'     \item \code{methods}: The similarity methods used
#'     \item \code{list_names}: Names of the input lists
#'     \item \code{raw_values}: The original input lists
#'   }
#'
#' @examples
#' list1 <- list(1, 2, 3)
#' list2 <- list(1, 2.1, 3.2)
#'
#' # Using unnamed lists
#' result1 <- same_number(list1, list2)
#'
#' # Using named lists for more control
#' result2 <- same_number("n1" = list1, "n2" = list2)
#' @export
same_number <- function(..., method = c("exact", "raw", "exp", "percent", "normalized", "fuzzy"),
                        epsilon = 0.05, epsilon_pct = 0.02, max_diff = NULL, digits = 3) {
  valid_methods <- c(
    "exact", "raw", "exp", "percent", "normalized", "fuzzy"
  )

  inputs <- list(...)
  validate_number_inputs(...)

  method <- unique(if (length(method) == 1) c(method) else method)

  invalid_methods <- method[!method %in% valid_methods]
  if (length(invalid_methods) > 0) {
    cli_abort(sprintf(
      "Invalid methods for numeric similarity: %s. Valid methods are: %s.",
      paste(invalid_methods, collapse = ", "),
      paste(valid_methods, collapse = ", ")
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
            max_diff = key_max_diff,
            epsilon_pct = epsilon_pct
          )

          mean_score <- round(mean(pair_result), digits)
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
          max_diff = max_diff,
          epsilon_pct = epsilon_pct
        )

        mean_score <- round(mean(pair_result), digits)
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
    raw_values = raw_values,
    digits = digits
  )
}
