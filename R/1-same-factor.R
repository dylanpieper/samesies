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

  if (!cat1 %in% levels) cat1 <- NA
  if (!cat2 %in% levels) cat2 <- NA

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

#' Validate Factor Input Lists
#'
#' @param ... Lists of character or factor values to validate
#' @param levels Character vector of all allowed levels
#'
#' @return TRUE if all inputs are valid, error message otherwise
#' @noRd
validate_factor_inputs <- function(..., levels) {
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

  if (is.null(levels) || length(levels) == 0) {
    cli::cli_abort("Must provide non-empty levels parameter")
  }

  TRUE
}

#' Compare Categorical Data Similarity Across Multiple Lists
#'
#' @param ... Lists of categorical values (character or factor) to compare
#' @param method Character vector of similarity methods. Choose from: "exact",
#'   "order" (default: all)
#' @param levels Character vector of all allowed levels for comparison
#' @param ordered Logical. If TRUE, treat levels as ordered (ordinal). If FALSE,
#'   the "order" method is skipped.
#'
#' @return An S7 object of type "similar_factor" containing:
#'   - scores: Numeric similarity scores by method and comparison
#'   - summary: Summary statistics by method and comparison
#'   - methods: Methods used for comparison
#'   - list_names: Names of compared lists
#'   - levels: Levels used for categorical comparison
#'
#' @export
same_factor <- function(..., method = c("exact", "order"), levels, ordered = FALSE) {
  valid_methods <- c("exact", "order")

  inputs <- list(...)
  validate_factor_inputs(..., levels = levels)

  method <- unique(if (length(method) == 1) c(method) else method)
  invalid_methods <- method[!method %in% valid_methods]
  if (length(invalid_methods) > 0) {
    cli::cli_abort(c("All methods must be one of: {paste(valid_methods, collapse = ', ')}"))
  }

  if ("order" %in% method && !ordered) {
    cli::cli_alert_info("Skipping 'order' method because levels are not explicitly ordered. Set ordered = TRUE to compute the order method.")
    method <- method[method != "order"]
  }
  if (length(method) == 0) {
    cli::cli_abort("No valid methods remain after filtering. Please check the methods argument or set ordered = TRUE to include the 'order' method.")
  }

  dots <- as.list(substitute(list(...)))[-1]
  list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    names(dots)
  } else {
    purrr::map_chr(dots, ~ deparse(.x)[1])
  }

  has_nested <- FALSE
  for (i in seq_along(inputs)) {
    if (length(names(inputs[[i]])) > 0) {
      has_nested <- TRUE
      break
    }
  }

  scores <- list()
  summaries <- list()
  for (m in method) {
    scores[[m]] <- list()
    summaries[[m]] <- list()
  }

  if (has_nested) {
    all_keys <- unique(unlist(lapply(inputs, names)))
    for (key in all_keys) {
      key_lists <- lapply(inputs, function(x) {
        if (!is.null(x[[key]])) x[[key]] else list()
      })
      key_lists <- key_lists[sapply(key_lists, length) > 0]
      if (length(key_lists) < 2) next

      pairs <- get_pairwise_combinations(length(key_lists))
      for (m in method) {
        for (i in seq_along(pairs$first)) {
          idx1 <- pairs$first[i]
          idx2 <- pairs$second[i]
          pair_name <- paste0(key, "_", list_names[idx1], "_", list_names[idx2])
          pair_result <- calculate_factor_scores(
            key_lists[[idx1]],
            key_lists[[idx2]],
            method = m,
            levels = levels
          )
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
          mean_score <- round(mean(pair_result), 3)
          cli::cli_alert_success("Computed {.field {m}} scores for {.val {key}} in {.val {list_names[idx1]}}-{.val {list_names[idx2]}} [mean: {.val {mean_score}}]")
        }
      }
    }
  } else {
    flattened_inputs <- lapply(inputs, flatten_list)
    lengths <- purrr::map_int(flattened_inputs, length)
    if (length(unique(lengths)) > 1) {
      cli::cli_abort("All lists must have same length after flattening")
    }
    pairs <- get_pairwise_combinations(length(flattened_inputs))
    for (m in method) {
      for (i in seq_along(pairs$first)) {
        idx1 <- pairs$first[i]
        idx2 <- pairs$second[i]
        pair_name <- paste0(list_names[idx1], "_", list_names[idx2])
        pair_result <- calculate_factor_scores(
          flattened_inputs[[idx1]],
          flattened_inputs[[idx2]],
          method = m,
          levels = levels
        )
        mean_score <- round(mean(pair_result), 3)
        cli::cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")
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

  similar_factor(
    scores = scores,
    summary = summaries,
    methods = method,
    list_names = list_names,
    levels = levels
  )
}
