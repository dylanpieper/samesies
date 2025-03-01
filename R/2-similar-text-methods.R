#' Print method for similar_text objects
#'
#' @param x A similar_text object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @noRd
S7::method(print, similar_text) <- function(x, ...) {
  cli::cli_h1("Text Similarity Analysis")
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
        "*" = "SD: {.val {round(summary_stats$sd, x@digits)}}",
        "*" = "IQR: {.val {round(summary_stats$iqr, x@digits)}}",
        "*" = "Range: [{.val {round(summary_stats$min, x@digits)}} - {.val {round(summary_stats$max, x@digits)}}]"
      ))
    })
  })

  invisible(x)
}

#' Summary method for similar_text objects
#'
#' @param object A similar_text object
#' @param ... Additional arguments (not used)
#'
#' @return A summary.similar_text object
#' @noRd
S7::method(summary, similar_text) <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object@methods,
    list_names = object@list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )
  
  # Store digits as an attribute to be used by print.summary.similar_text
  attr(result, "digits") <- object@digits

  class(result) <- "summary.similar_text"
  return(result)
}

#' Print method for summary.similar_text objects
#'
#' @param x A summary.similar_text object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @noRd
print.summary.similar_text <- function(x, ...) {
  cli::cli_h1("Summary: Text Similarity Analysis")

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
