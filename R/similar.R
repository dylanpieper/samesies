#' Abstract parent class for similarity comparison
#'
#' @description
#' `similar` is an S7 abstract parent class for all similarity comparison objects.
#' This class defines common properties and validation logic shared among child classes
#' like `similar_text`, `similar_factor`, and `similar_number`.
#'
#' @details
#' This class provides the foundation for all similarity comparison classes.
#' It includes common properties:
#' - scores: List of similarity scores per method and comparison
#' - summary: Summary statistics by method and comparison
#' - methods: Character vector of methods used for comparison
#' - list_names: Character vector of names for the compared lists
#'
#' It also implements common validation logic for ensuring scores are numeric
#' and fall within the valid range of 0 to 1.
#'
#' Child classes should inherit from this class and implement their own
#' specific validation logic for allowed methods and any additional properties.
#'
#' @export
similar <- S7::new_class("similar",
  properties = list(
    # Nested list of scores by method and comparison pair
    scores = S7::class_list,
    # Summary statistics by method and comparison pair
    summary = S7::class_list,
    # Methods used for comparison
    methods = S7::class_character,
    # Names of compared lists
    list_names = S7::class_character
  ),
  validator = function(self) {
    # Validate that all scores are numeric and between 0-1
    for (method_name in names(self@scores)) {
      for (list_pair in names(self@scores[[method_name]])) {
        scores <- self@scores[[method_name]][[list_pair]]
        
        if (!is.numeric(scores)) {
          return(sprintf(
            "All scores must be numeric. Found non-numeric score(s) in %s for %s.",
            method_name, list_pair
          ))
        }
        
        if (any(scores < 0 | scores > 1, na.rm = TRUE)) {
          return(sprintf(
            "All scores must be between 0 and 1 (inclusive). Found score(s) out of range in %s for %s.",
            method_name, list_pair
          ))
        }
      }
    }
    
    # Validate list_names are character and non-empty
    if (length(self@list_names) == 0) {
      return("list_names cannot be empty.")
    }
    
    # NOTE: Method validation is handled by child classes
    
    NULL
  },
  abstract = TRUE  # Mark as abstract to prevent direct instantiation
)

#' Calculate average similarity across all methods
#'
#' @param x A similar object
#' @param ... Additional arguments (not used)
#'
#' @return Named numeric vector of means by method
#' @exportS3Method
average_similarity.similar <- function(x, ...) {
  mean_scores_by_method(x@scores)
}

#' Calculate average similarity by pair
#'
#' @param x A similar object
#' @param method Optional character vector of methods to include
#'
#' @return Data frame of pair-wise averages
#' @exportS3Method
pair_averages.similar <- function(x, method = NULL, ...) {
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
}

#' Visualize similarity scores
#'
#' @param x A similar object
#' @param y Not used (for S3 generic compatibility)
#' @param type Plot type (varies by object type)
#' @param palette Color palette to use
#' @param ... Additional arguments passed to methods
#'
#' @return A ggplot2 visualization
#' @exportS3Method
plot.similar <- function(x, y, ...) {
  # Generic method to be implemented by child classes
  cli::cli_abort("No plot method implemented for this similarity class")
}