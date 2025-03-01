#' Calculate average similarity scores
#'
#' @param x A similarity object
#' @param ... Additional arguments (not used)
#'
#' @return A named numeric vector of mean similarity scores for each method
#' @noRd
S7::method(average_similarity, similar) <- function(x, ...) {
  mean_scores_by_method(x@scores, x@digits)
}

#' Calculate average similarity scores by pairs
#'
#' @param x A similarity object
#' @param method Optional character vector of methods to include
#' @param ... Additional arguments (not used)
#'
#' @return A data frame containing pair-wise average scores
#' @noRd
S7::method(pair_averages, similar) <- function(x, method = NULL, ...) {
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
  result$avg_score <- round(result$avg_score, x@digits)

  rownames(result) <- NULL
  return(result)
}

#' Print a similarity object
#'
#' @param x A similarity object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @noRd
S7::method(print, similar) <- function(x, ...) {
  cli::cli_h1("Similarity Analysis")
  cli::cli_text("Methods used: {.val {paste(x@methods, collapse = ', ')}}")
  cli::cli_text("Lists compared: {.val {paste(x@list_names, collapse = ', ')}}")

  overall_avgs <- average_similarity(x)

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
    paste0("* ", method, ": {.val ", round(overall_avgs[method], x@digits), "}")
  }))

  purrr::walk(x@methods, function(method) {
    cli::cli_h2("Method: {.field {method}}")

    purrr::walk(names(x@summary[[method]]), function(pair_name) {
      cli::cli_h3("Comparison: {.val {pair_name}}")

      summary_stats <- x@summary[[method]][[pair_name]]

      cli::cli_bullets(c(
        "*" = "Mean: {.val {round(summary_stats$mean, x@digits)}}",
        "*" = "Median: {.val {round(summary_stats$median, x@digits)}}",
        "*" = "SD: {.val {round(summary_stats$sd, x@digits)}}"
      ))
    })
  })

  invisible(x)
}

#' Summarize a similarity object
#'
#' @param object A similarity object
#' @param ... Additional arguments (not used)
#'
#' @return A summary object
#' @noRd
S7::method(summary, similar) <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object@methods,
    list_names = object@list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )
  
  # Store digits as an attribute to be used by print.summary.similar
  attr(result, "digits") <- object@digits

  class(result) <- "summary.similar"
  return(result)
}

#' Print method for summary.similar objects
#'
#' @param x A summary.similar object
#' @param ... Additional arguments (not used)
#'
#' @export
#' @noRd
print.summary.similar <- function(x, ...) {
  cli::cli_h1("Summary: Similarity Analysis")

  cli::cli_h2("Methods Used")
  cli::cli_text("{.val {paste(x$methods, collapse = ', ')}}")

  cli::cli_h2("Lists Compared")
  cli::cli_text("{.val {paste(x$list_names, collapse = ', ')}}")

  cli::cli_h2("Overall Method Averages")
  # For the summary object, need to get digits from x itself since it's not stored in the summary
  # Use default of 3 if not found as backward compatibility
  digits <- if (!is.null(attr(x, "digits"))) attr(x, "digits") else 3
  cli::cli_bullets(purrr::map_chr(names(x$overall_averages), function(method) {
    paste0("* ", method, ": {.val ", round(x$overall_averages[method], digits), "}")
  }))

  cli::cli_h2("Pair Averages")
  print(x$pair_averages, row.names = FALSE)

  invisible(x)
}
