#' Compare Factor Similarity Across Lists
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
    cli::cli_abort(sprintf(
      "Invalid methods for factor similarity: %s. Valid methods are: %s.",
      paste(invalid_methods, collapse = ", "),
      paste(valid_methods, collapse = ", ")
    ))
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
