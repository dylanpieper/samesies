#' Base Similarity Calculator
#'
#' @description
#' `SimilarityCalculatorBase` is an R6 class that defines the interface for
#' similarity calculators.
#'
#' @importFrom R6 R6Class
#' @export
SimilarityCalculatorBase <- R6::R6Class("SimilarityCalculatorBase",
  public = list(
    #' @description
    #' Get the type of calculator
    #'
    #' @return A string identifying the calculator type
    get_type = function() {
      cli::cli_abort("get_type() must be implemented by derived classes")
    },
    
    #' @description
    #' Get the valid methods for this calculator
    #'
    #' @return A character vector of valid methods
    get_valid_methods = function() {
      cli::cli_abort("get_valid_methods() must be implemented by derived classes")
    },
    
    #' @description
    #' Validate that an object has valid methods for this calculator
    #'
    #' @param obj The similarity object to validate
    #'
    #' @return NULL if valid, error message otherwise
    validate = function(obj) {
      valid_methods <- self$get_valid_methods()
      
      if (!all(obj$methods %in% valid_methods)) {
        cli::cli_abort(sprintf(
          "All methods must be one of: %s",
          paste(valid_methods, collapse = ", ")
        ))
      }
      
      NULL
    },
    
    #' @description
    #' Calculate similarity between two elements
    #'
    #' @param elem1 First element
    #' @param elem2 Second element
    #' @param method The method to use
    #' @param ... Additional arguments passed to method
    #'
    #' @return A similarity score between 0 and 1
    calculate_similarity = function(elem1, elem2, method, ...) {
      cli::cli_abort("calculate_similarity() must be implemented by derived classes")
    },
    
    #' @description
    #' Calculate similarity scores between two lists
    #'
    #' @param list1 First list
    #' @param list2 Second list
    #' @param method The method to use
    #' @param ... Additional arguments passed to method
    #'
    #' @return A named vector of similarity scores
    calculate_scores = function(list1, list2, method, ...) {
      cli::cli_abort("calculate_scores() must be implemented by derived classes")
    },
    
    #' @description
    #' Validate inputs before calculation
    #'
    #' @param ... Lists to validate
    #'
    #' @return TRUE if valid, error otherwise
    validate_inputs = function(...) {
      cli::cli_abort("validate_inputs() must be implemented by derived classes")
    },
    
    #' @description
    #' Compare multiple lists and return a similarity object
    #'
    #' @param ... Lists to compare
    #' @param method Methods to use for comparison
    #' @param ... Additional arguments passed to method
    #'
    #' @return A similarity object
    compare = function(...) {
      cli::cli_abort("compare() must be implemented by derived classes")
    }
  )
)

#' Text Similarity Calculator
#'
#' @description
#' Calculator for text similarity metrics
#'
#' @importFrom R6 R6Class
#' @export
TextSimilarityCalculator <- R6::R6Class("TextSimilarityCalculator",
  inherit = SimilarityCalculatorBase,
  public = list(
    #' @description
    #' Get the type of calculator
    #'
    #' @return "similarity_text"
    get_type = function() {
      "similarity_text"
    },
    
    #' @description
    #' Get the valid methods for text similarity
    #'
    #' @return A character vector of valid methods
    get_valid_methods = function() {
      c("osa", "lv", "dl", "hamming", "lcs", "qgram",
        "cosine", "jaccard", "jw", "soundex")
    },
    
    #' @description
    #' Calculate similarity between two text strings
    #'
    #' @param str1 First string
    #' @param str2 Second string
    #' @param method Method for comparison
    #' @param q Size of q-gram (default: 1)
    #' @param p Winkler scaling factor (default varies by method)
    #' @param bt Booth matching threshold
    #' @param weight Vector of weights for operations
    #'
    #' @return Normalized similarity score between 0 and 1
    calculate_similarity = function(str1, str2, method, q = 1, p = NULL, bt = 0,
                                  weight = c(d = 1, i = 1, s = 1, t = 1)) {
      if (is.null(p)) {
        p <- if (method == "jw") 0.1 else 0
      }
      
      if (method == "jw" && p > 0.25) {
        cli::cli_abort("For Jaro-Winkler (jw) method, p must be <= 0.25")
      }
      
      str1 <- as.character(str1)
      str2 <- as.character(str2)
      
      if (str1 == "" && str2 == "") {
        return(1)
      }
      if (str1 == "" || str2 == "") {
        return(0)
      }
      
      dist <- stringdist::stringdist(
        str1,
        str2,
        method = method,
        q = q,
        p = p,
        bt = bt,
        weight = weight
      )
      
      similarity <- if (method %in% c("osa", "lv", "dl", "hamming", "lcs")) {
        1 - (dist / max(nchar(str1), nchar(str2)))
      } else if (method == "qgram") {
        1 - (dist / (nchar(str1) + nchar(str2)))
      } else {
        1 - dist
      }
      
      pmin(1, pmax(0, similarity))
    },
    
    #' @description
    #' Calculate similarity scores between two lists of strings
    #'
    #' @param list1 First list of strings
    #' @param list2 Second list of strings
    #' @param method Method for comparison
    #' @param ... Additional arguments passed to calculate_similarity()
    #'
    #' @return A named vector of similarity scores
    calculate_scores = function(list1, list2, method, ...) {
      if (length(list1) != length(list2)) {
        cli::cli_abort("All lists must have same length")
      }
      
      scores <- purrr::map2_dbl(
        list1,
        list2,
        function(x, y) self$calculate_similarity(x, y, method = method, ...)
      )
      
      names(scores) <- unlist(list1)
      scores
    },
    
    #' @description
    #' Validate text input lists
    #'
    #' @param ... Lists of character strings to validate
    #'
    #' @return TRUE if valid, error otherwise
    validate_inputs = function(...) {
      inputs <- list(...)
      
      if (length(inputs) < 2) {
        cli::cli_abort("At least two inputs required")
      }
      
      is_valid <- function(x) {
        if (is.list(x)) {
          return(all(vapply(x, is_valid, logical(1))))
        } else {
          return(is.character(x) && length(x) == 1)
        }
      }
      
      is_char_list <- function(x) {
        if (!is.list(x)) {
          return(FALSE)
        }
        return(all(vapply(x, is_valid, logical(1))))
      }
      
      invalid_inputs <- which(!vapply(inputs, is_char_list, logical(1)))
      
      if (length(invalid_inputs) > 0) {
        cli::cli_abort(c(
          "All inputs must be lists containing only character strings",
          "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
        ))
      }
      
      TRUE
    },
    
    #' @description
    #' Compare text similarity across multiple lists
    #'
    #' @param ... Lists of character strings to compare
    #' @param method Character vector of similarity methods
    #' @param q Size of q-gram (default: 1)
    #' @param p Winkler scaling factor (default varies by method)
    #' @param bt Booth matching threshold
    #' @param weight Vector of weights for operations
    #'
    #' @return A SimilarityBase object with text components
    compare = function(..., method = "jw", q = 1, p = NULL, bt = 0,
                      weight = c(d = 1, i = 1, s = 1, t = 1)) {
      inputs <- list(...)
      
      # Validate inputs
      self$validate_inputs(...)
      
      method <- unique(if (length(method) == 1) c(method) else method)
      
      # Validate methods
      valid_methods <- self$get_valid_methods()
      invalid_methods <- method[!method %in% valid_methods]
      if (length(invalid_methods) > 0) {
        cli::cli_abort(c(
          "All methods must be one of: {paste(valid_methods, collapse = ', ')}"
        ))
      }
      
      # Get list names
      dots <- as.list(substitute(list(...)))[-1]
      list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
        names(dots)
      } else {
        purrr::map_chr(dots, ~ deparse(.x)[1])
      }
      
      # Flatten nested lists
      flatten_list <- function(x) {
        if (!is.list(x)) {
          return(x)
        }
        unlist(lapply(x, flatten_list))
      }
      
      flattened_inputs <- lapply(inputs, flatten_list)
      
      # Check all lists have same length after flattening
      lengths <- purrr::map_int(flattened_inputs, length)
      if (length(unique(lengths)) > 1) {
        cli::cli_abort("All lists must have same length after flattening")
      }
      
      # Get pairwise combinations
      pairs <- get_pairwise_combinations(length(flattened_inputs))
      
      # Calculate scores for each method and pair
      scores <- purrr::map(method, function(m) {
        pair_scores <- purrr::map2(pairs$first, pairs$second, function(idx1, idx2) {
          pair_name <- paste0(list_names[idx1], "_", list_names[idx2])
          
          pair_result <- self$calculate_scores(
            flattened_inputs[[idx1]],
            flattened_inputs[[idx2]],
            method = m,
            q = q,
            p = p,
            bt = bt,
            weight = weight
          )
          
          mean_score <- round(mean(pair_result), 3)
          cli::cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")
          
          pair_result
        })
        
        names(pair_scores) <- purrr::map2_chr(
          pairs$first,
          pairs$second,
          ~ paste0(list_names[.x], "_", list_names[.y])
        )
        
        pair_scores
      })
      
      names(scores) <- method
      
      # Calculate summary statistics
      summaries <- purrr::map(method, function(m) {
        purrr::map(scores[[m]], function(pair_scores) {
          list(
            mean = mean(pair_scores),
            median = stats::median(pair_scores),
            sd = stats::sd(pair_scores),
            min = min(pair_scores),
            max = max(pair_scores),
            q1 = stats::quantile(pair_scores, 0.25),
            q3 = stats::quantile(pair_scores, 0.75),
            iqr = stats::IQR(pair_scores)
          )
        })
      })
      
      names(summaries) <- method
      
      # Create and return similarity object
      SimilarityBase$new(
        scores = scores,
        summary = summaries,
        methods = method,
        list_names = list_names,
        calculator_type = "text"
      )
    }
  )
)

#' Factor Similarity Calculator
#'
#' @description
#' Calculator for factor/categorical similarity metrics
#'
#' @importFrom R6 R6Class
#' @export
FactorSimilarityCalculator <- R6::R6Class("FactorSimilarityCalculator",
  inherit = SimilarityCalculatorBase,
  public = list(
    #' @field levels Combined levels from all factor inputs
    levels = NULL,
    
    #' @description
    #' Get the type of calculator
    #'
    #' @return "similarity_factor"
    get_type = function() {
      "similarity_factor"
    },
    
    #' @description
    #' Get the valid methods for factor similarity
    #'
    #' @return A character vector of valid methods
    get_valid_methods = function() {
      c("exact", "jaccard", "overlap", "matching")
    },
    
    #' @description
    #' Calculate similarity between two factor values
    #'
    #' @param f1 First factor value
    #' @param f2 Second factor value
    #' @param method Method for comparison
    #' @param ... Additional arguments (not used)
    #'
    #' @return Normalized similarity score between 0 and 1
    calculate_similarity = function(f1, f2, method, ...) {
      f1 <- as.character(f1)
      f2 <- as.character(f2)
      
      switch(method,
        "exact" = {
          if (is.na(f1) && is.na(f2)) return(1)
          if (is.na(f1) || is.na(f2)) return(0)
          as.numeric(f1 == f2)
        },
        "jaccard" = {
          if (is.na(f1) && is.na(f2)) return(1)
          if (is.na(f1) || is.na(f2)) return(0)
          
          # For single values, Jaccard is same as exact match
          as.numeric(f1 == f2)
        },
        "overlap" = {
          if (is.na(f1) && is.na(f2)) return(1)
          if (is.na(f1) || is.na(f2)) return(0)
          
          # For single values, overlap is same as exact match
          as.numeric(f1 == f2)
        },
        "matching" = {
          if (is.na(f1) && is.na(f2)) return(1)
          if (is.na(f1) || is.na(f2)) return(0)
          
          # For single values, matching coefficient is same as exact match
          as.numeric(f1 == f2)
        },
        cli::cli_abort("Unknown factor similarity method: {method}")
      )
    },
    
    #' @description
    #' Calculate similarity scores between two lists of factors
    #'
    #' @param list1 First list of factors
    #' @param list2 Second list of factors
    #' @param method Method for comparison
    #' @param ... Additional arguments passed to calculate_similarity()
    #'
    #' @return A named vector of similarity scores
    calculate_scores = function(list1, list2, method, ...) {
      if (length(list1) != length(list2)) {
        cli::cli_abort("All lists must have same length")
      }
      
      scores <- purrr::map2_dbl(
        list1,
        list2,
        function(x, y) self$calculate_similarity(x, y, method = method, ...)
      )
      
      names(scores) <- as.character(unlist(list1))
      scores
    },
    
    #' @description
    #' Validate factor input lists
    #'
    #' @param ... Lists of factors to validate
    #'
    #' @return TRUE if valid, error otherwise
    validate_inputs = function(...) {
      inputs <- list(...)
      
      if (length(inputs) < 2) {
        cli::cli_abort("At least two inputs required")
      }
      
      # Helper function to check if an element is a valid factor-like input
      is_valid <- function(x) {
        if (is.list(x)) {
          return(all(vapply(x, is_valid, logical(1))))
        } else {
          return(is.factor(x) || is.character(x) || is.logical(x) || is.numeric(x))
        }
      }
      
      # Helper function to check if a list contains only valid factor-like inputs
      is_valid_list <- function(x) {
        if (!is.list(x)) {
          return(FALSE)
        }
        return(all(vapply(x, is_valid, logical(1))))
      }
      
      invalid_inputs <- which(!vapply(inputs, is_valid_list, logical(1)))
      
      if (length(invalid_inputs) > 0) {
        cli::cli_abort(c(
          "All inputs must be lists containing only factor-like values",
          "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
        ))
      }
      
      # Collect all unique levels
      all_values <- unlist(lapply(inputs, function(lst) {
        unlist(lapply(lst, as.character))
      }))
      
      self$levels <- sort(unique(all_values))
      
      TRUE
    },
    
    #' @description
    #' Compare factor similarity across multiple lists
    #'
    #' @param ... Lists of factors to compare
    #' @param method Character vector of similarity methods
    #' @param ... Additional arguments (not used)
    #'
    #' @return A SimilarityBase object with factor components
    compare = function(..., method = "exact") {
      inputs <- list(...)
      
      # Validate inputs
      self$validate_inputs(...)
      
      method <- unique(if (length(method) == 1) c(method) else method)
      
      # Validate methods
      valid_methods <- self$get_valid_methods()
      invalid_methods <- method[!method %in% valid_methods]
      if (length(invalid_methods) > 0) {
        cli::cli_abort(c(
          "All methods must be one of: {paste(valid_methods, collapse = ', ')}"
        ))
      }
      
      # Get list names
      dots <- as.list(substitute(list(...)))[-1]
      list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
        names(dots)
      } else {
        purrr::map_chr(dots, ~ deparse(.x)[1])
      }
      
      # Flatten nested lists
      flatten_list <- function(x) {
        if (!is.list(x)) {
          return(x)
        }
        unlist(lapply(x, flatten_list))
      }
      
      flattened_inputs <- lapply(inputs, flatten_list)
      
      # Check all lists have same length after flattening
      lengths <- purrr::map_int(flattened_inputs, length)
      if (length(unique(lengths)) > 1) {
        cli::cli_abort("All lists must have same length after flattening")
      }
      
      # Get pairwise combinations
      pairs <- get_pairwise_combinations(length(flattened_inputs))
      
      # Calculate scores for each method and pair
      scores <- purrr::map(method, function(m) {
        pair_scores <- purrr::map2(pairs$first, pairs$second, function(idx1, idx2) {
          pair_name <- paste0(list_names[idx1], "_", list_names[idx2])
          
          pair_result <- self$calculate_scores(
            flattened_inputs[[idx1]],
            flattened_inputs[[idx2]],
            method = m
          )
          
          mean_score <- round(mean(pair_result), 3)
          cli::cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")
          
          pair_result
        })
        
        names(pair_scores) <- purrr::map2_chr(
          pairs$first,
          pairs$second,
          ~ paste0(list_names[.x], "_", list_names[.y])
        )
        
        pair_scores
      })
      
      names(scores) <- method
      
      # Calculate summary statistics
      summaries <- purrr::map(method, function(m) {
        purrr::map(scores[[m]], function(pair_scores) {
          list(
            mean = mean(pair_scores),
            median = stats::median(pair_scores),
            sd = stats::sd(pair_scores),
            min = min(pair_scores),
            max = max(pair_scores),
            q1 = stats::quantile(pair_scores, 0.25),
            q3 = stats::quantile(pair_scores, 0.75),
            iqr = stats::IQR(pair_scores)
          )
        })
      })
      
      names(summaries) <- method
      
      # Create and return similarity object with levels
      similarity_obj <- SimilarityBase$new(
        scores = scores,
        summary = summaries,
        methods = method,
        list_names = list_names,
        calculator_type = "factor"
      )
      
      # Add levels to the calculator component
      similarity_obj$calculator$levels <- self$levels
      
      similarity_obj
    }
  )
)

#' Number Similarity Calculator
#'
#' @description
#' Calculator for numeric similarity metrics
#'
#' @importFrom R6 R6Class
#' @export
NumberSimilarityCalculator <- R6::R6Class("NumberSimilarityCalculator",
  inherit = SimilarityCalculatorBase,
  public = list(
    #' @field raw_values Original numeric values from all inputs
    raw_values = NULL,
    
    #' @description
    #' Get the type of calculator
    #'
    #' @return "similarity_number"
    get_type = function() {
      "similarity_number"
    },
    
    #' @description
    #' Get the valid methods for number similarity
    #'
    #' @return A character vector of valid methods
    get_valid_methods = function() {
      c("exact", "percent_diff", "normalized", "fuzzy", "rank")
    },
    
    #' @description
    #' Calculate similarity between two numeric values
    #'
    #' @param n1 First number
    #' @param n2 Second number
    #' @param method Method for comparison
    #' @param tolerance Tolerance for fuzzy matching (default: 0.01)
    #' @param ... Additional arguments (not used)
    #'
    #' @return Normalized similarity score between 0 and 1
    calculate_similarity = function(n1, n2, method, tolerance = 0.01, ...) {
      # Handle NA values
      if (is.na(n1) && is.na(n2)) return(1)
      if (is.na(n1) || is.na(n2)) return(0)
      
      # Convert to numeric if not already
      n1 <- as.numeric(n1)
      n2 <- as.numeric(n2)
      
      switch(method,
        "exact" = {
          # Exact match (1 if equal, 0 otherwise)
          as.numeric(n1 == n2)
        },
        "percent_diff" = {
          # Percent difference based similarity
          if (n1 == 0 && n2 == 0) return(1)
          if (n1 == 0 || n2 == 0) return(0)
          
          percent_diff <- abs((n1 - n2) / ((n1 + n2) / 2))
          similarity <- 1 - min(1, percent_diff)
          max(0, similarity)
        },
        "normalized" = {
          # Normalize by the maximum value
          max_val <- max(abs(n1), abs(n2))
          if (max_val == 0) return(1)  # Both are zero
          
          diff <- abs(n1 - n2)
          similarity <- 1 - (diff / max_val)
          max(0, similarity)
        },
        "fuzzy" = {
          # Fuzzy equality with tolerance
          diff <- abs(n1 - n2)
          max_val <- max(abs(n1), abs(n2))
          
          if (max_val == 0) return(1)  # Both are zero
          relative_diff <- diff / max_val
          
          if (relative_diff <= tolerance) {
            # Scale similarity from 1.0 (exact match) to 0.9 (at tolerance)
            similarity <- 1 - (relative_diff / tolerance) * 0.1
          } else {
            # Scale similarity from 0.9 to 0 beyond tolerance
            similarity <- 0.9 * (1 - min(1, (relative_diff - tolerance) / (1 - tolerance)))
          }
          max(0, similarity)
        },
        "rank" = {
          # Rank-based similarity (requires raw_values)
          if (is.null(self$raw_values)) {
            cli::cli_abort("Raw values must be set to use rank method")
          }
          
          # Get ranks
          rank_n1 <- match(n1, sort(unique(self$raw_values)))
          rank_n2 <- match(n2, sort(unique(self$raw_values)))
          
          # Calculate similarity based on normalized rank difference
          max_rank <- length(unique(self$raw_values))
          if (max_rank <= 1) return(1)  # Only one unique value
          
          rank_diff <- abs(rank_n1 - rank_n2)
          similarity <- 1 - (rank_diff / (max_rank - 1))
          max(0, similarity)
        },
        cli::cli_abort("Unknown number similarity method: {method}")
      )
    },
    
    #' @description
    #' Calculate similarity scores between two lists of numbers
    #'
    #' @param list1 First list of numbers
    #' @param list2 Second list of numbers
    #' @param method Method for comparison
    #' @param tolerance Tolerance for fuzzy matching
    #' @param ... Additional arguments (not used)
    #'
    #' @return A named vector of similarity scores
    calculate_scores = function(list1, list2, method, tolerance = 0.01, ...) {
      if (length(list1) != length(list2)) {
        cli::cli_abort("All lists must have same length")
      }
      
      scores <- purrr::map2_dbl(
        list1,
        list2,
        function(x, y) self$calculate_similarity(x, y, method = method, tolerance = tolerance, ...)
      )
      
      # Attempt to create names, but don't fail if they can't be converted to strings
      tryCatch({
        names(scores) <- as.character(unlist(list1))
      }, error = function(e) {
        # If naming fails, use index-based names
        names(scores) <- paste0("item_", seq_along(scores))
      })
      
      scores
    },
    
    #' @description
    #' Validate numeric input lists
    #'
    #' @param ... Lists of numbers to validate
    #'
    #' @return TRUE if valid, error otherwise
    validate_inputs = function(...) {
      inputs <- list(...)
      
      if (length(inputs) < 2) {
        cli::cli_abort("At least two inputs required")
      }
      
      # Helper function to check if an element is numeric or can be converted to numeric
      is_valid <- function(x) {
        if (is.list(x)) {
          return(all(vapply(x, is_valid, logical(1))))
        } else {
          return(is.numeric(x) || !is.na(suppressWarnings(as.numeric(x))))
        }
      }
      
      # Helper function to check if a list contains only numeric-like inputs
      is_valid_list <- function(x) {
        if (!is.list(x)) {
          return(FALSE)
        }
        return(all(vapply(x, is_valid, logical(1))))
      }
      
      invalid_inputs <- which(!vapply(inputs, is_valid_list, logical(1)))
      
      if (length(invalid_inputs) > 0) {
        cli::cli_abort(c(
          "All inputs must be lists containing only numeric values",
          "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
        ))
      }
      
      # Collect all numeric values
      all_values <- unlist(lapply(inputs, function(lst) {
        unlist(lapply(lst, as.numeric))
      }))
      
      self$raw_values <- all_values
      
      TRUE
    },
    
    #' @description
    #' Compare number similarity across multiple lists
    #'
    #' @param ... Lists of numbers to compare
    #' @param method Character vector of similarity methods
    #' @param tolerance Tolerance for fuzzy matching (default: 0.01)
    #' @param ... Additional arguments (not used)
    #'
    #' @return A SimilarityBase object with number components
    compare = function(..., method = "exact", tolerance = 0.01) {
      inputs <- list(...)
      
      # Validate inputs
      self$validate_inputs(...)
      
      method <- unique(if (length(method) == 1) c(method) else method)
      
      # Validate methods
      valid_methods <- self$get_valid_methods()
      invalid_methods <- method[!method %in% valid_methods]
      if (length(invalid_methods) > 0) {
        cli::cli_abort(c(
          "All methods must be one of: {paste(valid_methods, collapse = ', ')}"
        ))
      }
      
      # Get list names
      dots <- as.list(substitute(list(...)))[-1]
      list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
        names(dots)
      } else {
        purrr::map_chr(dots, ~ deparse(.x)[1])
      }
      
      # Flatten nested lists and convert to numeric
      flatten_list <- function(x) {
        if (!is.list(x)) {
          return(as.numeric(x))
        }
        as.numeric(unlist(lapply(x, flatten_list)))
      }
      
      flattened_inputs <- lapply(inputs, flatten_list)
      
      # Check all lists have same length after flattening
      lengths <- purrr::map_int(flattened_inputs, length)
      if (length(unique(lengths)) > 1) {
        cli::cli_abort("All lists must have same length after flattening")
      }
      
      # Get pairwise combinations
      pairs <- get_pairwise_combinations(length(flattened_inputs))
      
      # Calculate scores for each method and pair
      scores <- purrr::map(method, function(m) {
        pair_scores <- purrr::map2(pairs$first, pairs$second, function(idx1, idx2) {
          pair_name <- paste0(list_names[idx1], "_", list_names[idx2])
          
          pair_result <- self$calculate_scores(
            flattened_inputs[[idx1]],
            flattened_inputs[[idx2]],
            method = m,
            tolerance = tolerance
          )
          
          mean_score <- round(mean(pair_result), 3)
          cli::cli_alert_success("Computed {.field {m}} scores for {.val {pair_name}} [mean: {.val {mean_score}}]")
          
          pair_result
        })
        
        names(pair_scores) <- purrr::map2_chr(
          pairs$first,
          pairs$second,
          ~ paste0(list_names[.x], "_", list_names[.y])
        )
        
        pair_scores
      })
      
      names(scores) <- method
      
      # Calculate summary statistics
      summaries <- purrr::map(method, function(m) {
        purrr::map(scores[[m]], function(pair_scores) {
          list(
            mean = mean(pair_scores),
            median = stats::median(pair_scores),
            sd = stats::sd(pair_scores),
            min = min(pair_scores),
            max = max(pair_scores),
            q1 = stats::quantile(pair_scores, 0.25),
            q3 = stats::quantile(pair_scores, 0.75),
            iqr = stats::IQR(pair_scores)
          )
        })
      })
      
      names(summaries) <- method
      
      # Create and return similarity object with raw_values
      similarity_obj <- SimilarityBase$new(
        scores = scores,
        summary = summaries,
        methods = method,
        list_names = list_names,
        calculator_type = "number"
      )
      
      # Add raw_values to the calculator component
      similarity_obj$calculator$raw_values <- self$raw_values
      
      similarity_obj
    }
  )
)