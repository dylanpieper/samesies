#' Print method for similar_number objects
#'
#' @param x A similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @export
print.similar_number <- function(x, ...) {
  cli::cli_h1("Numeric Similarity Analysis")
  cli::cli_text("Methods used: {.val {paste(x$methods, collapse = ', ')}}")
  cli::cli_text("Lists compared: {.val {paste(x$list_names, collapse = ', ')}}")

  overall_avgs <- average_similarity(x)

  cli::cli_h2("Overall Method Averages")
  cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
    paste0("* ", method, ": {.val ", round(overall_avgs[method], x$digits), "}")
  }))

  purrr::walk(x$methods, function(method) {
    cli::cli_h2("Method: {.field {method}}")

    purrr::walk(names(x$summary[[method]]), function(pair_name) {
      cli::cli_h3("Comparison: {.val {pair_name}}")

      summary_stats <- x$summary[[method]][[pair_name]]

      cli::cli_bullets(c(
        "*" = "Mean: {.val {round(summary_stats$mean, x$digits)}}",
        "*" = "Median: {.val {round(summary_stats$median, x$digits)}}",
        "*" = "SD: {.val {round(summary_stats$sd, x$digits)}}",
        "*" = "Range: [{.val {round(summary_stats$min, x$digits)}} - {.val {round(summary_stats$max, x$digits)}}]",
        "*" = "Exact Matches: {.val {sum(x$scores[[method]][[pair_name]] == 1)}} of {.val {length(x$scores[[method]][[pair_name]])}}"
      ))
    })
  })

  invisible(x)
}

#' Summary method for similar_number objects
#'
#' @param object A similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return A summary.similar_number object
#' @export
summary.similar_number <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object$methods,
    list_names = object$list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )

  attr(result, "digits") <- object$digits

  class(result) <- "summary.similar_number"
  return(result)
}

#' Print method for summary.similar_number objects
#'
#' @param x A summary.similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @export
print.summary.similar_number <- function(x, ...) {
  cli::cli_h1("Summary: Numeric Data Similarity Analysis")

  cli::cli_h2("Methods Used")
  cli::cli_text("{.val {paste(x$methods, collapse = ', ')}}")

  cli::cli_h2("Lists Compared")
  cli::cli_text("{.val {paste(x$list_names, collapse = ', ')}}")

  cli::cli_h2("Overall Method Averages")

  digits <- if (!is.null(attr(x, "digits"))) attr(x, "digits") else 3
  cli::cli_bullets(purrr::map_chr(names(x$overall_averages), function(method) {
    paste0("* ", method, ": {.val ", round(x$overall_averages[method], digits), "}")
  }))

  cli::cli_h2("Pair Averages")
  print(x$pair_averages, row.names = FALSE)

  invisible(x)
}
