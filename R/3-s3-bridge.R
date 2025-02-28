#' Print method for similar objects
#'
#' @param x A similar object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @export
print.similar <- function(x, ...) {
  S7::S7_dispatch("print", x, ...)
}

#' Calculate average similarity scores
#'
#' @param x A similarity object
#' @param ... Additional arguments (not used)
#'
#' @return A named numeric vector of mean similarity scores for each method
#' @export
average_similarity.similar <- function(x, ...) {
  S7::S7_dispatch("average_similarity", x, ...)
}

#' Calculate average similarity scores by pairs
#'
#' @param x A similarity object
#' @param method Optional character vector of methods to include
#' @param ... Additional arguments (not used)
#'
#' @return A data frame containing pair-wise average scores
#' @export
pair_averages.similar <- function(x, method = NULL, ...) {
  S7::S7_dispatch("pair_averages", x, method, ...)
}

#' Summarize a similarity object
#'
#' @param object A similarity object
#' @param ... Additional arguments (not used)
#'
#' @return A summary object
#' @export
summary.similar <- function(object, ...) {
  S7::S7_dispatch("summary", object, ...)
}

#' Print method for similar_text objects
#'
#' @param x A similar_text object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @export
print.similar_text <- function(x, ...) {
  S7::S7_dispatch("print", x, ...)
}

#' Print method for similar_factor objects
#'
#' @param x A similar_factor object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @export
print.similar_factor <- function(x, ...) {
  S7::S7_dispatch("print", x, ...)
}

#' Print method for similar_number objects
#'
#' @param x A similar_number object
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#' @export
print.similar_number <- function(x, ...) {
  S7::S7_dispatch("print", x, ...)
}
