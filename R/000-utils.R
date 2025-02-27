# File renamed from utils.R to aaa-utils.R to ensure it's loaded early

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

#' Create a Calculator Factory
#'
#' @description
#' Creates a factory function that returns calculator objects for specific similarity types
#'
#' @return A function that creates calculator objects
#'
#' @noRd
create_calculator_factory <- function() {
  function(type) {
    switch(type,
      "text" = TextSimilarityCalculator$new(),
      "factor" = FactorSimilarityCalculator$new(),
      "number" = NumberSimilarityCalculator$new(),
      cli::cli_abort("Unknown calculator type: {type}")
    )
  }
}

#' Create a Formatter Factory
#'
#' @description
#' Creates a factory function that returns formatter objects for specific similarity types
#'
#' @return A function that creates formatter objects
#'
#' @noRd
create_formatter_factory <- function() {
  function(type) {
    switch(type,
      "text" = TextSimilarityFormatter$new(),
      "factor" = FactorSimilarityFormatter$new(), 
      "number" = NumberSimilarityFormatter$new(),
      cli::cli_abort("Unknown formatter type: {type}")
    )
  }
}

#' Create a Plotter Factory
#' 
#' @description
#' Creates a factory function that returns plotter objects for specific similarity types
#'
#' @return A function that creates plotter objects
#'
#' @noRd
create_plotter_factory <- function() {
  function(type) {
    switch(type,
      "text" = TextSimilarityPlotter$new(),
      "factor" = FactorSimilarityPlotter$new(),
      "number" = NumberSimilarityPlotter$new(),
      cli::cli_abort("Unknown plotter type: {type}")
    )
  }
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

#' @export
average_similarity.default <- function(x, ...) {
  cli::cli_abort("No average_similarity method defined for this class")
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

#' @export
pair_averages.default <- function(x, method = NULL, ...) {
  cli::cli_abort("No pair_averages method defined for this class")
}

#' Visualize Similarity Scores Across Methods
#'
#' @param x A similarity object created by one of the similarity functions
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
plot.similar <- function(x, type = "combined", palette = "Set2", ...) {
  if (inherits(x, "SimilarityBase")) {
    return(x$make_plot(type, palette, ...))
  }
  NextMethod()
}