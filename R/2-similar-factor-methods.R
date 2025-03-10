#' Print method for similar_factor objects
#'
#' @param x A similar_factor object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @export
print.similar_factor <- function(x, ...) {
  cli::cli_h1("Factor Similarity Analysis")
  cli::cli_text("Methods used: {.val {paste(x$methods, collapse = ', ')}}")
  cli::cli_text("Lists compared: {.val {paste(x$list_names, collapse = ', ')}}")
  cli::cli_text("Levels used: {.val {paste(x$levels, collapse = ', ')}}")

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
        "*" = "Exact Matches: {.val {sum(x$scores[[method]][[pair_name]] == 1)}} of {.val {length(x$scores[[method]][[pair_name]])}}"
      ))
    })
  })

  invisible(x)
}

#' Summary method for similar_factor objects
#'
#' @param object A similar_factor object
#' @param ... Additional arguments (not used)
#'
#' @return A summary.similar_factor object
#' @export
summary.similar_factor <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object$methods,
    list_names = object$list_names,
    levels = object$levels,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )
  
  # Store digits as an attribute to be used by print.summary.similar_factor
  attr(result, "digits") <- object$digits

  class(result) <- "summary.similar_factor"
  return(result)
}

#' Print method for summary.similar_factor objects
#'
#' @param x A summary.similar_factor object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
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
