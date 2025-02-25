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

#' Calculate Average Similarity Scores
#'
#' @param x A similarity object created by one of the similarity functions (same_text, same_factor, same_number)
#' @param ... Additional arguments passed to specific methods
#'
#' @return A named numeric vector of mean similarity scores for each method
#'
#' @description
#' Calculates and returns the average similarity score for each method used in the comparison.
#'
#' @export
average_similarity <- function(x, ...) {
  if (S7::S7_inherits(x, similar_text)) {
    mean_scores_by_method(x@scores)
  } else if (S7::S7_inherits(x, similar_factor)) {
    mean_scores_by_method(x@scores)
  } else if (S7::S7_inherits(x, similar_number)) {
    mean_scores_by_method(x@scores)
  } else {
    cli::cli_abort("Unsupported object type. Must be one of: similar_text, similar_factor, similar_number")
  }
}

#' Calculate Mean Scores for Each Method
#'
#' @param scores A named list of scores from similarity methods
#'
#' @return A named numeric vector of mean scores rounded to 3 decimal places
#'
#' @noRd
mean_scores_by_method <- function(scores) {
  result <- purrr::map_dbl(names(scores), function(method) {
    method_scores <- scores[[method]]
    all_scores <- unlist(method_scores)
    mean(all_scores, na.rm = TRUE)
  })

  names(result) <- names(scores)
  round(result, 3)
}

#' Calculate Average Similarity Scores By Pairs
#'
#' @param x A similarity object created by one of the similarity functions (same_text, same_factor, same_number)
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
  if (S7::S7_inherits(x, similar_text) ||
    S7::S7_inherits(x, similar_factor) ||
    S7::S7_inherits(x, similar_number)) {
    # Get methods to use
    methods_list <- x@methods
    if (!is.null(method)) {
      if (!all(method %in% methods_list)) {
        cli::cli_abort("Specified method(s) not found in the similarity object")
      }
      methods_to_use <- method
    } else {
      methods_to_use <- methods_list
    }

    result <- purrr::map_df(methods_to_use, function(m) {
      method_scores <- x@scores[[m]]

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
  } else {
    cli::cli_abort("Unsupported object type. Must be one of: similar_text, similar_factor, similar_number")
  }
}

#' Visualize Similarity Scores Across Methods
#'
#' @param x A similarity object created by one of the similarity functions (same_text, same_factor, same_number)
#' @param type Plot type, varies by similarity object but usually includes:
#'   "combined" (default), "boxplot", "point", or a type-specific visualization
#' @param palette Color palette to use for the plot (default: "Set2"). Can be any RColorBrewer palette.
#' @param ... Additional arguments passed to specific methods
#'
#' @return A ggplot2 visualization comparing similarity scores across methods
#'
#' @description
#' Creates visualizations to help interpret and compare the different similarity
#' methods used in the analysis. Plot types vary depending on the specific
#' similarity object type.
#'
#' @export
plot_methods <- function(x, type = "combined", palette = "Set2", ...) {
  if (S7::S7_inherits(x, similar_text) ||
    S7::S7_inherits(x, similar_factor) ||
    S7::S7_inherits(x, similar_number)) {
    plot_data <- purrr::map_df(x@methods, function(method) {
      purrr::map_df(names(x@scores[[method]]), function(pair_name) {
        data.frame(
          method = method,
          pair = pair_name,
          score = unlist(x@scores[[method]][[pair_name]])
        )
      })
    })

    base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Method", y = "Similarity Score") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    if (S7::S7_inherits(x, similar_text)) {
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
    } else if (S7::S7_inherits(x, similar_factor)) {
      switch(type,
        "combined" = base_plot +
          ggplot2::geom_boxplot(ggplot2::aes(fill = method), alpha = 0.7, outlier.shape = NA) +
          ggplot2::geom_jitter(ggplot2::aes(color = pair), width = 0.1, alpha = 0.4) +
          ggplot2::scale_fill_brewer(palette = palette) +
          ggplot2::theme(legend.position = "right"),
        "boxplot" = base_plot +
          ggplot2::geom_boxplot(ggplot2::aes(fill = method)) +
          ggplot2::scale_fill_brewer(palette = palette) +
          ggplot2::theme(legend.position = "none"),
        "point" = base_plot +
          ggplot2::geom_jitter(ggplot2::aes(color = pair), width = 0.2, alpha = 0.7) +
          ggplot2::stat_summary(fun = mean, geom = "point", size = 3, color = "black"),
        "heatmap" = {
          heatmap_data <- plot_data %>%
            dplyr::group_by(method, pair) %>%
            dplyr::summarize(
              exact_match_pct = mean(score == 1),
              avg_score = mean(score)
            ) %>%
            dplyr::ungroup()

          ggplot2::ggplot(heatmap_data, ggplot2::aes(x = method, y = pair, fill = exact_match_pct)) +
            ggplot2::geom_tile(color = "white") +
            ggplot2::scale_fill_gradient(low = "white", high = "steelblue", name = "Exact Match %") +
            ggplot2::geom_text(ggplot2::aes(label = scales::percent(exact_match_pct, accuracy = 1))) +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "Method", y = "Comparison Pair") +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )
        },
        cli::cli_abort("Invalid plot type. Choose 'combined', 'boxplot', 'point', or 'heatmap'")
      )
    } else if (S7::S7_inherits(x, similar_number)) {
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
        "histogram" = {
          ggplot2::ggplot(plot_data, ggplot2::aes(x = score, fill = method)) +
            ggplot2::geom_histogram(position = "dodge", bins = 20, alpha = 0.7) +
            ggplot2::facet_wrap(~method) +
            ggplot2::scale_fill_brewer(palette = palette) +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "Similarity Score", y = "Count") +
            ggplot2::theme(legend.position = "none")
        },
        cli::cli_abort("Invalid plot type. Choose 'combined', 'boxplot', 'point', or 'histogram'")
      )
    }
  } else {
    cli::cli_abort("Unsupported object type. Must be one of: similar_text, similar_factor, similar_number")
  }
}

#' Print Method for similar_text Objects
#'
#' @param x A 'similar_text' object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns x while printing formatted text similarity analysis output
#'
#' @export
print.similar_text <- function(x, ...) {
  cli::cli_h1("Text Similarity Analysis")
  cli::cli_text("Methods used: {.val {paste(x@methods, collapse = ', ')}}")
  cli::cli_text("Lists compared: {.val {paste(x@list_names, collapse = ', ')}}")

  overall_avgs <- average_similarity(x)

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
    paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
  }))

  purrr::walk(x@methods, function(method) {
    cli::cli_h2("Method: {.field {method}}")

    purrr::walk(names(x@summary[[method]]), function(pair_name) {
      cli::cli_h3("Comparison: {.val {pair_name}}")

      cli::cli_h3("Summary Statistics")
      summary_stats <- x@summary[[method]][[pair_name]]

      cli::cli_bullets(c(
        "*" = "Mean: {.val {round(summary_stats$mean, 3)}}",
        "*" = "Median: {.val {round(summary_stats$median, 3)}}",
        "*" = "SD: {.val {round(summary_stats$sd, 3)}}",
        "*" = "IQR: {.val {round(summary_stats$iqr, 3)}}",
        "*" = "Range: [{.val {round(summary_stats$min, 3)}} - {.val {round(summary_stats$max, 3)}}]"
      ))
    })
  })

  invisible(x)
}

#' Print Method for similar_factor Objects
#'
#' @param x A 'similar_factor' object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns x while printing formatted categorical similarity analysis output
#'
#' @export
print.similar_factor <- function(x, ...) {
  cli::cli_h1("Categorical Data Similarity Analysis")
  cli::cli_text("Methods used: {.val {paste(x@methods, collapse = ', ')}}")
  cli::cli_text("Lists compared: {.val {paste(x@list_names, collapse = ', ')}}")
  cli::cli_text("Levels used: {.val {paste(x@levels, collapse = ', ')}}")

  overall_avgs <- average_similarity(x)

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
    paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
  }))

  purrr::walk(x@methods, function(method) {
    cli::cli_h2("Method: {.field {method}}")

    purrr::walk(names(x@summary[[method]]), function(pair_name) {
      cli::cli_h3("Comparison: {.val {pair_name}}")

      cli::cli_h3("Summary Statistics")
      summary_stats <- x@summary[[method]][[pair_name]]

      cli::cli_bullets(c(
        "*" = "Mean: {.val {round(summary_stats$mean, 3)}}",
        "*" = "Median: {.val {round(summary_stats$median, 3)}}",
        "*" = "SD: {.val {round(summary_stats$sd, 3)}}",
        "*" = "Exact Matches: {.val {sum(x@scores[[method]][[pair_name]] == 1)}} of {.val {length(x@scores[[method]][[pair_name]])}}"
      ))
    })
  })

  invisible(x)
}

#' Print Method for similar_number Objects
#'
#' @param x A 'similar_number' object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.similar_number <- function(x, ...) {
  cli::cli_h1("Numeric Data Similarity Analysis")
  cli::cli_text("Methods used: {.val {paste(x@methods, collapse = ', ')}}")
  cli::cli_text("Lists compared: {.val {paste(x@list_names, collapse = ', ')}}")

  overall_avgs <- average_similarity(x)

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
    paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
  }))

  purrr::walk(x@methods, function(method) {
    cli::cli_h2("Method: {.field {method}}")

    purrr::walk(names(x@summary[[method]]), function(pair_name) {
      cli::cli_h3("Comparison: {.val {pair_name}}")

      cli::cli_h3("Summary Statistics")
      summary_stats <- x@summary[[method]][[pair_name]]

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

#' Summary Method for similar_text Objects
#'
#' @param object A 'similar_text' object
#' @param ... Additional arguments (not used)
#'
#' @return A summary object with class "summary.similar_text"
#'
#' @export
summary.similar_text <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object@methods,
    list_names = object@list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )

  class(result) <- "summary.similar_text"
  return(result)
}

#' Summary Method for similar_factor Objects
#'
#' @param object A 'similar_factor' object
#' @param ... Additional arguments (not used)
#'
#' @return A summary object with class "summary.similar_factor"
#'
#' @export
summary.similar_factor <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object@methods,
    list_names = object@list_names,
    levels = object@levels,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )

  class(result) <- "summary.similar_factor"
  return(result)
}

#' Summary Method for similar_number Objects
#'
#' @param object A 'similar_number' object
#' @param ... Additional arguments (not used)
#'
#' @return A summary object with class "summary.similar_number"
#'
#' @export
summary.similar_number <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object@methods,
    list_names = object@list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )

  class(result) <- "summary.similar_number"
  return(result)
}

#' Print Method for summary.similar_text Objects
#'
#' @param x A summary.similar_text object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.summary.similar_text <- function(x, ...) {
  cli::cli_h1("Summary: Text Similarity Analysis")

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

#' Print Method for summary.similar_factor Objects
#'
#' @param x A summary.similar_factor object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.summary.similar_factor <- function(x, ...) {
  cli::cli_h1("Summary: Categorical Data Similarity Analysis")

  cli::cli_h2("Methods Used")
  cli::cli_text("{.val {paste(x$methods, collapse = ', ')}}")

  cli::cli_h2("Lists Compared")
  cli::cli_text("{.val {paste(x$list_names, collapse = ', ')}}")

  cli::cli_h2("Levels")
  cli::cli_text("{.val {paste(x$levels, collapse = ', ')}}")

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(x$overall_averages), function(method) {
    paste0("* ", method, ": {.val ", round(x$overall_averages[method], 3), "}")
  }))

  cli::cli_h2("Pair Averages")
  print(x$pair_averages, row.names = FALSE)

  invisible(x)
}

#' Print Method for summary.similar_number Objects
#'
#' @param x A summary.similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
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
