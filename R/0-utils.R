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

#' Check if a value is valid
#'
#' @param x The value to check
#' @param type The expected type of the value: "text", "number", or "factor"
#' @param levels Optional character vector of allowed levels (for factor type)
#'
#' @return Logical indicating if the value is valid
#' @noRd
is_valid <- function(x, type = c("text", "number", "factor"), levels = NULL) {
  type <- match.arg(type)

  # Return TRUE directly for lists because we check if elements are valid elsewhere
  if (is.list(x)) {
    return(TRUE)
  }

  switch(type,
    "text" = {
      is.character(x) && length(x) == 1
    },
    "number" = {
      is.numeric(x) || is.na(x)
    },
    "factor" = {
      (is.character(x) || is.factor(x) || is.na(x))
    }
  )
}

#' Check if a list contains only valid values
#'
#' @param x The list to check
#' @param type The expected type of the values: "text", "number", or "factor"
#' @param levels Optional character vector of allowed levels (for factor type)
#'
#' @return Logical indicating if the list contains only valid values
#' @noRd
is_valid_list <- function(x, type = c("text", "number", "factor"), levels = NULL) {
  type <- match.arg(type)

  check_element <- function(y) {
    if (is.list(y)) {
      return(all(vapply(y, check_element, logical(1))))
    }

    switch(type,
      "text" = {
        is.character(y) && length(y) == 1
      },
      "number" = {
        is.numeric(y) || is.na(y)
      },
      "factor" = {
        (is.character(y) || is.factor(y) || is.na(y))
      }
    )
  }

  is.list(x) && all(vapply(x, check_element, logical(1)))
}

#' Flatten a nested list
#'
#' @param x The list to flatten
#'
#' @return A flattened version of the input list
#' @noRd
flatten_list <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  unlist(lapply(x, flatten_list))
}

#' Calculate Average Similarity Scores
#'
#' @param x A similarity object created by one of the similarity functions
#' @param ... Additional arguments passed to specific methods
#'
#' @return A named numeric vector of mean similarity scores for each method
#'
#' @description
#' Calculates and returns the average similarity score for each method used in the comparison.
#'
#' @export
average_similarity <- function(x, ...) {
  if (inherits(x, "SimilarityBase")) {
    return(x$calc_average_similarity())
  }
  UseMethod("average_similarity")
}

#' Calculate Average Similarity Scores By Pairs
#'
#' @param x A similarity object created by one of the similarity functions
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
  if (inherits(x, "SimilarityBase")) {
    return(x$calc_pair_averages(method))
  }
  UseMethod("pair_averages")
}
