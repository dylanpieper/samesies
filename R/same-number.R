#' Create a Similar Number Object for Comparing Numeric Data Similarity
#'
#' @description
#' `similar_number` is an S7 class that inherits from `similar` for comparing numeric data similarity.
#' It extends the parent class with an additional "raw_values" property and number-specific validation.
#'
#' @param scores List of similarity scores for each method and comparison
#' @param summary List of summary statistics for each method and comparison
#' @param methods Character vector of similarity methods used
#' @param list_names Character vector of input list names
#' @param raw_values List of the original numeric values being compared
#'
#' @returns An S7 class object of type "similar_number" containing:
#'   - scores: Numeric similarity scores by method and comparison
#'   - summary: Summary statistics by method and comparison
#'   - methods: Methods used for comparison
#'   - list_names: Names of compared lists
#'   - raw_values: Original numeric values
#'
#' @examples
#' \dontrun{
#' list1 <- list(1, 2, 3)
#' list2 <- list(1.1, 2.1, 3.2)
#' result <- same_number(list1, list2)
#' }
#' @export
similar_number <- S7::new_class("similar_number",
  parent = similar,
  properties = list(
    raw_values = S7::class_list
  ),
  validator = function(self) {
    valid_methods <- c(
      "exact", "percent_diff", "normalized", "fuzzy"
    )

    if (!all(self@methods %in% valid_methods)) {
      return(sprintf(
        "All methods must be one of: %s",
        paste(valid_methods, collapse = ", ")
      ))
    }

    NULL
  }
)

#' @export
summary.same_number <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object$methods,
    list_names = object$list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )

  class(result) <- "summary.same_number"
  return(result)
}

#' Print Method for same_number Objects
#'
#' @param x A same_number object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.same_number <- function(x, ...) {
  cli::cli_h1("same_number: Numeric Data Similarity Analysis")
  cli::cli_text("Methods used: {.val {paste(x$methods, collapse = ', ')}}")
  cli::cli_text("Lists compared: {.val {paste(x$list_names, collapse = ', ')}}")

  overall_avgs <- average_similarity(x)

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
    paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
  }))

  purrr::walk(x$methods, function(method) {
    cli::cli_h2("Method: {.field {method}}")

    purrr::walk(names(x$summary[[method]]), function(pair_name) {
      cli::cli_h3("Comparison: {.val {pair_name}}")

      cli::cli_h3("Summary Statistics")
      summary_stats <- x$summary[[method]][[pair_name]]

      cli::cli_bullets(c(
        "*" = "Mean: {.val {round(summary_stats$mean, 3)}}",
        "*" = "Median: {.val {round(summary_stats$median, 3)}}",
        "*" = "SD: {.val {round(summary_stats$sd, 3)}}",
        "*" = "Range: [{.val {round(summary_stats$min, 3)}} - {.val {round(summary_stats$max, 3)}}]"
      ))
    })
  })

  invisible(x)
}

#' @export
print.summary.same_number <- function(x, ...) {
  cli::cli_h1("Summary: Numeric Data Similarity Analysis")

  cli::cli_h2("Methods Used")
  cli::cli_text("{.val {paste(x$methods, collapse = ', ')}}")

  cli::cli_h2("Lists Compared")
  cli::cli_text("{.val {paste(x$list_names, collapse = ', ')}}")

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(x$overall_averages), function(method) {
    paste0("* ", method, ": {.val ", round(x$overall_averages[method], 3), "}")
  }))

  cli::cli_h2("Pair Averages")
  print(x$pair_averages, row.names = FALSE)

  invisible(x)
}

#' Plot Method for same_number Objects
#'
#' @param x A same_number object
#' @param y Not used (for S3 generic compatibility)
#' @param type Plot type: "combined" (default), "boxplot", "point", "histogram"
#' @param palette Color palette to use for the plot (default: "Set2")
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot2 visualization of similarity scores
#'
#' @export
plot.same_number <- function(x, y, type = "combined", palette = "Set2", ...) {
  # Prepare common data frame for plotting
  plot_data <- purrr::map_df(x$methods, function(method) {
    purrr::map_df(names(x$data[[method]]), function(pair_name) {
      data.frame(
        method = method,
        pair = pair_name,
        score = unlist(x$data[[method]][[pair_name]])
      )
    })
  })

  # Create base plot
  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Method", y = "Similarity Score") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Return the appropriate plot type
  switch(type,
    "combined" = base_plot +
      ggplot2::geom_violin(ggplot2::aes(fill = method), alpha = 0.4) +
      ggplot2::geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    "boxplot" = base_plot +
      ggplot2::geom_boxplot(ggplot2::aes(fill = method)) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    "point" = base_plot +
      ggplot2::geom_jitter(ggplot2::aes(color = pair), width = 0.2, alpha = 0.7) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = 3, color = "black"),
    "violin" = base_plot +
      ggplot2::geom_violin(ggplot2::aes(fill = method)) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    cli::cli_abort("Invalid plot type. Choose 'combined', 'boxplot', 'point', or 'violin'")
  )
}

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
      if (abs(num1 - num2) <= epsilon) {
        return(1)
      } else {
        diff <- abs(num1 - num2) - epsilon
        return(max(0, 1 - diff))
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
#' @return Numeric epsilon value between min_epsilon and max_epsilon
#' @keywords internal
auto_epsilon <- function(values, percentile = 0.1) {
  values <- unlist(values)
  values <- values[!is.na(values)]

  if (length(values) < 2 || all(values == values[1])) {
    return(0.05)
  }

  sd_val <- stats::sd(values)
  mean_val <- mean(abs(values))

  epsilon <- sd_val * percentile

  if (mean_val > 0) {
    min_epsilon <- mean_val * 0.001
    max_epsilon <- mean_val * 0.2
    epsilon <- max(min_epsilon, min(max_epsilon, epsilon))
  }

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

  is_valid <- function(x) {
    if (is.list(x)) {
      return(all(vapply(x, is_valid, logical(1))))
    } else {
      return(is.numeric(x) || is.na(x))
    }
  }

  is_valid_list <- function(x) {
    if (!is.list(x)) {
      return(FALSE)
    }
    return(all(vapply(x, is_valid, logical(1))))
  }

  invalid_inputs <- which(!vapply(inputs, is_valid_list, logical(1)))

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
#' @param method Character vector specifying similarity methods. Default c("percent_diff", "fuzzy")
#' @param epsilon Threshold for fuzzy matching. NULL for auto-calculation
#' @param max_diff Maximum difference for normalization. NULL for auto-calculation
#'
#' @return An S3 object of class "same_number" that contains an S7 'similar_number' object
#' and provides methods for average_similarity(), pair_averages(), plot(), print(), and summary().
#'
#' @examples
#' nums1 <- list(1, 2, 3)
#' nums2 <- list(1, 2.1, 3.2)
#' result <- same_number(nums1, nums2)
#' print(result)
#'
#' @export
same_number <- function(..., method = c("percent_diff", "fuzzy"), epsilon = NULL, max_diff = NULL) {
  valid_methods <- c(
    "exact", "percent_diff", "normalized", "fuzzy"
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

  # Create the S7 object
  s7_obj <- similar_number(
    scores = scores,
    summary = summaries,
    methods = method,
    list_names = list_names,
    raw_values = flattened_inputs
  )

  # Create the S3 object
  result <- list(
    data = scores,
    summary = summaries,
    methods = method,
    list_names = list_names,
    s7_obj = s7_obj
  )

  class(result) <- c("same_number", "same_similarity")
  return(result)
}

#' Print Method for summary.similar_number Objects
#'
#' @param x A summary.similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' Calculate Average Similarity Scores for same_number Objects
#'
#' @param x A same_number object
#' @param ... Additional arguments passed to specific methods
#'
#' @return A named numeric vector of mean similarity scores for each method
#'
#' @description
#' Calculates and returns the average similarity score for each method used in the comparison.
#'
#' @export
average_similarity.same_number <- function(x, ...) {
  mean_scores_by_method(x$data)
}

#' Calculate Average Similarity Scores By Pairs for same_number Objects
#'
#' @param x A same_number object
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
pair_averages.same_number <- function(x, method = NULL, ...) {
  # Get methods to use
  methods_list <- x$methods
  if (!is.null(method)) {
    if (!all(method %in% methods_list)) {
      cli::cli_abort("Specified method(s) not found in the similarity object")
    }
    methods_to_use <- method
  } else {
    methods_to_use <- methods_list
  }

  result <- purrr::map_df(methods_to_use, function(m) {
    method_scores <- x$data[[m]]

    purrr::map_df(names(method_scores), function(pair_name) {
      data.frame(
        method = m,
        pair = pair_name,
        avg_score = mean(unlist(method_scores[[pair_name]]), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
  })

  result <- result[order(result$method, -result$avg_score), ]
  result$avg_score <- round(result$avg_score, 3)

  rownames(result) <- NULL
  return(result)
}

#' @export
print.summary.similar_number <- function(x, ...) {
  cli::cli_h1("Summary: Numeric Data Similarity Analysis")

  cli::cli_h2("Methods Used")
  cli::cli_text("{.val {paste(x$methods, collapse = ', ')}}")

  cli::cli_h2("Lists Compared")
  cli::cli_text("{.val {paste(x$list_names, collapse = ', ')}}")

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(x$overall_averages), function(method) {
    paste0("* ", method, ": {.val ", round(x$overall_averages[method], 3), "}")
  }))

  cli::cli_h2("Pair Averages")
  print(x$pair_averages, row.names = FALSE)

  invisible(x)
}
