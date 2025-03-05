#' Abstract parent class for similarity comparison
#'
#' @description
#' `similar` is an S3 class for all similarity comparison objects.
#' This class defines common properties shared among child classes
#' like `similar_text`, `similar_factor`, and `similar_number`.
#'
#' @param scores List of similarity scores per method and comparison
#' @param summary Summary statistics by method and comparison
#' @param methods Character vector of methods used for comparison
#' @param list_names Character vector of names for the compared lists
#' @param digits Number of digits to round results (default: 3)
#'
#' @details
#' This class provides the foundation for all similarity comparison classes.
#' It includes common properties:
#' - scores: List of similarity scores per method and comparison
#' - summary: Summary statistics by method and comparison
#' - methods: Character vector of methods used for comparison
#' - list_names: Character vector of names for the compared lists
#' - digits: Number of digits to round results in output
#'
#' @return An object of class "similar" with the following components:
#' \itemize{
#'   \item scores: List of similarity scores per method and comparison
#'   \item summary: Summary statistics by method and comparison
#'   \item methods: Character vector of methods used for comparison
#'   \item list_names: Character vector of names for the compared lists
#'   \item digits: Number of digits to round results in output
#' }
#' The similarity scores are normalized values between 0 and 1, where 1 indicates perfect similarity
#' and 0 indicates no similarity.
#'
#' @export
similar <- function(scores, summary, methods, list_names, digits = 3) {
  # Validation
  for (method_name in names(scores)) {
    for (list_pair in names(scores[[method_name]])) {
      scores_values <- scores[[method_name]][[list_pair]]

      if (!is.numeric(scores_values)) {
        stop(sprintf(
          "All scores must be numeric. Found non-numeric score(s) in %s for %s.",
          method_name, list_pair
        ))
      }

      if (any(scores_values < 0 | scores_values > 1, na.rm = TRUE)) {
        stop(sprintf(
          "All scores must be between 0 and 1 (inclusive). Found score(s) out of range in %s for %s.",
          method_name, list_pair
        ))
      }
    }
  }

  if (length(list_names) == 0) {
    stop("list_names cannot be empty.")
  }

  structure(
    list(
      scores = scores,
      summary = summary,
      methods = methods,
      list_names = list_names,
      digits = digits
    ),
    class = "similar"
  )
}

#' Text similarity comparison class
#'
#' @description
#' `similar_text` is an S3 class for text similarity comparisons.
#'
#' @param scores List of similarity scores per method and comparison
#' @param summary Summary statistics by method and comparison
#' @param methods Character vector of methods used for comparison
#' @param list_names Character vector of names for the compared lists
#' @param digits Number of digits to round results (default: 3)
#'
#' @details
#' This class extends the `similar` class and implements
#' text-specific similarity comparison methods.
#'
#' @return An object of class "similar_text" (which inherits from "similar") containing:
#' \itemize{
#'   \item scores: List of text similarity scores per method and comparison
#'   \item summary: Summary statistics by method and comparison
#'   \item methods: Character vector of text similarity methods used (osa, lv, dl, etc.)
#'   \item list_names: Character vector of names for the compared text lists
#'   \item digits: Number of digits to round results in output
#' }
#' The text similarity scores are normalized values between 0 and 1, where 1 indicates 
#' identical text and 0 indicates completely different text based on the specific method used.
#'
#' @export
similar_text <- function(scores, summary, methods, list_names, digits = 3) {
  valid_methods <- c(
    "osa", "lv", "dl", "hamming", "lcs", "qgram",
    "cosine", "jaccard", "jw", "soundex"
  )

  invalid_methods <- methods[!methods %in% valid_methods]
  if (length(invalid_methods) > 0) {
    stop(sprintf(
      "Invalid methods for text similarity: %s. Valid methods are: %s.",
      paste(invalid_methods, collapse = ", "),
      paste(valid_methods, collapse = ", ")
    ))
  }

  structure(
    similar(
      scores = scores,
      summary = summary,
      methods = methods,
      list_names = list_names,
      digits = digits
    ),
    class = c("similar_text", "similar")
  )
}

#' Factor similarity comparison class
#'
#' @description
#' `similar_factor` is an S3 class for categorical/factor similarity comparisons.
#'
#' @param scores List of similarity scores per method and comparison
#' @param summary Summary statistics by method and comparison
#' @param methods Character vector of methods used for comparison
#' @param list_names Character vector of names for the compared lists
#' @param digits Number of digits to round results (default: 3)
#' @param levels Character vector of factor levels
#'
#' @details
#' This class extends the `similar` class and implements
#' categorical data-specific similarity comparison methods.
#'
#' @return An object of class "similar_factor" (which inherits from "similar") containing:
#' \itemize{
#'   \item scores: List of factor similarity scores per method and comparison
#'   \item summary: Summary statistics by method and comparison
#'   \item methods: Character vector of factor comparison methods used (exact, order)
#'   \item list_names: Character vector of names for the compared factor lists
#'   \item digits: Number of digits to round results in output
#'   \item levels: Character vector of factor levels used in the comparison
#' }
#' The factor similarity scores are normalized values between 0 and 1, where 1 indicates 
#' identical factors and 0 indicates completely different factors based on the specific method used.
#'
#' @export
similar_factor <- function(scores, summary, methods, list_names, levels, digits = 3) {
  valid_methods <- c("exact", "order")

  invalid_methods <- methods[!methods %in% valid_methods]
  if (length(invalid_methods) > 0) {
    stop(sprintf(
      "Invalid methods for factor similarity: %s. Valid methods are: %s.",
      paste(invalid_methods, collapse = ", "),
      paste(valid_methods, collapse = ", ")
    ))
  }

  similar_obj <- similar(
    scores = scores,
    summary = summary,
    methods = methods,
    list_names = list_names,
    digits = digits
  )
  
  obj <- structure(
    c(similar_obj, list(levels = levels)),
    class = c("similar_factor", "similar")
  )
  
  return(obj)
}

#' Numeric similarity comparison class
#'
#' @description
#' `similar_number` is an S3 class for numeric similarity comparisons.
#'
#' @param scores List of similarity scores per method and comparison
#' @param summary Summary statistics by method and comparison
#' @param methods Character vector of methods used for comparison
#' @param list_names Character vector of names for the compared lists
#' @param digits Number of digits to round results (default: 3)
#' @param raw_values List of raw numeric values being compared
#'
#' @details
#' This class extends the `similar` class and implements
#' numeric data-specific similarity comparison methods.
#'
#' @return An object of class "similar_number" (which inherits from "similar") containing:
#' \itemize{
#'   \item scores: List of numeric similarity scores per method and comparison
#'   \item summary: Summary statistics by method and comparison
#'   \item methods: Character vector of numeric comparison methods used (exact, pct_diff, normalized, fuzzy)
#'   \item list_names: Character vector of names for the compared numeric lists
#'   \item digits: Number of digits to round results in output
#'   \item raw_values: List of raw numeric values that were compared
#' }
#' The numeric similarity scores are normalized values between 0 and 1, where 1 indicates 
#' identical numbers and 0 indicates maximally different numbers based on the specific method used.
#'
#' @export
similar_number <- function(scores, summary, methods, list_names, raw_values, digits = 3) {
  valid_methods <- c("exact", "pct_diff", "normalized", "fuzzy")

  invalid_methods <- methods[!methods %in% valid_methods]
  if (length(invalid_methods) > 0) {
    stop(sprintf(
      "Invalid methods for numeric similarity: %s. Valid methods are: %s.",
      paste(invalid_methods, collapse = ", "),
      paste(valid_methods, collapse = ", ")
    ))
  }

  similar_obj <- similar(
    scores = scores,
    summary = summary,
    methods = methods,
    list_names = list_names,
    digits = digits
  )
  
  obj <- structure(
    c(similar_obj, list(raw_values = raw_values)),
    class = c("similar_number", "similar")
  )
  
  return(obj)
}
