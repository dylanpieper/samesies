#' Print method for similar_number objects
#'
#' @param x A similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
S7::method(print, similar_number) <- function(x, ...) {
  cli::cli_h1("Numeric Similarity Analysis")
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

#' Summary method for similar_number objects
#'
#' @param object A similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return A summary.similar_number object
S7::method(summary, similar_number) <- function(object, ...) {
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
  cli::cli_bullets(purrr::map_chr(names(x$overall_averages), function(method) {
    paste0("* ", method, ": {.val ", round(x$overall_averages[method], 3), "}")
  }))

  cli::cli_h2("Pair Averages")
  print(x$pair_averages, row.names = FALSE)

  invisible(x)
}
