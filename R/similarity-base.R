#' Base Similarity Class
#'
#' @description
#' `SimilarityBase` is an R6 class that serves as the foundation for all similarity
#' comparison objects. It uses a composition-based approach with specialized 
#' calculator, formatter, and plotter components.
#'
#' @importFrom R6 R6Class
#' @export
SimilarityBase <- R6::R6Class("SimilarityBase",
  public = list(
    #' @field scores List of similarity scores per method and comparison
    scores = NULL,
    
    #' @field summary List of summary statistics per method and comparison
    summary = NULL,
    
    #' @field methods Character vector of methods used
    methods = NULL,
    
    #' @field list_names Character vector of names for compared lists
    list_names = NULL,
    
    #' @field calculator Component to calculate similarity scores
    calculator = NULL,
    
    #' @field formatter Component to format output
    formatter = NULL,
    
    #' @field plotter Component to create visualizations
    plotter = NULL,
    
    #' @description
    #' Create a new SimilarityBase object
    #'
    #' @param scores List of similarity scores
    #' @param summary List of summary statistics
    #' @param methods Character vector of methods used
    #' @param list_names Character vector of list names
    #' @param calculator_type The type of calculator to use
    #' @param ... Additional arguments passed to initialize
    #'
    #' @return A new SimilarityBase object
    initialize = function(scores = NULL, summary = NULL, methods = NULL, 
                           list_names = NULL, calculator_type = NULL, ...) {
      self$scores <- scores
      self$summary <- summary
      self$methods <- methods
      self$list_names <- list_names
      
      # Use component factories defined in 000-utils.R
      calc_factory <- create_calculator_factory()
      format_factory <- create_formatter_factory()
      plot_factory <- create_plotter_factory()
      
      # Inject components if calculator_type is provided
      if (!is.null(calculator_type)) {
        self$calculator <- calc_factory(calculator_type)
        self$formatter <- format_factory(calculator_type)
        self$plotter <- plot_factory(calculator_type)
      }
      
      # Validate the object
      private$validate()
      
      invisible(self)
    },
    
    #' @description
    #' Calculate average similarity across all methods
    #'
    #' @return Named numeric vector of means by method
    calc_average_similarity = function() {
      mean_scores_by_method(self$scores)
    },
    
    #' @description
    #' Calculate average similarity by pair
    #'
    #' @param method Optional character vector of methods to include
    #'
    #' @return Data frame of pair-wise averages
    calc_pair_averages = function(method = NULL) {
      # Get methods to use
      methods_list <- self$methods
      if (!is.null(method)) {
        if (!all(method %in% methods_list)) {
          cli::cli_abort("Specified method(s) not found in the similarity object")
        }
        methods_to_use <- method
      } else {
        methods_to_use <- methods_list
      }
      
      result <- purrr::map_df(methods_to_use, function(m) {
        method_scores <- self$scores[[m]]
        
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
    },
    
    #' @description
    #' Plot similarity scores by method
    #'
    #' @param type Plot type, one of "combined", "boxplot", "point", "violin"
    #' @param palette Color palette to use
    #' @param ... Additional arguments passed to the plotter
    #'
    #' @return A ggplot2 object
    make_plot = function(type = "combined", palette = "Set2", ...) {
      if (is.null(self$plotter)) {
        cli::cli_abort("No plotter component available")
      }
      
      self$plotter$plot(self, type = type, palette = palette, ...)
    },
    
    #' @description
    #' Print a summary of the similarity analysis
    #'
    #' @param ... Additional arguments (not used)
    #'
    #' @return Invisibly returns the object
    print = function(...) {
      if (is.null(self$formatter)) {
        cli::cli_abort("No formatter component available")
      }
      
      self$formatter$print(self)
      invisible(self)
    },
    
    #' @description
    #' Create a summary of the similarity analysis
    #'
    #' @param ... Additional arguments (not used)
    #'
    #' @return A summary object
    make_summary = function(...) {
      overall_avgs <- self$calc_average_similarity()
      pair_avgs <- self$calc_pair_averages()
      
      result <- list(
        methods = self$methods,
        list_names = self$list_names,
        overall_averages = overall_avgs,
        pair_averages = pair_avgs
      )
      
      if (is.null(self$formatter)) {
        class(result) <- "summary.similarity"
      } else {
        class(result) <- paste0("summary.", self$formatter$get_type())
      }
      
      return(result)
    }
  ),
  
  private = list(
    #' @description
    #' Validate the object
    #'
    #' @return NULL if valid, error message otherwise
    validate = function() {
      # Validate that all scores are numeric and between 0-1
      for (method_name in names(self$scores)) {
        for (list_pair in names(self$scores[[method_name]])) {
          scores <- self$scores[[method_name]][[list_pair]]
          
          if (!is.numeric(scores)) {
            cli::cli_abort(sprintf(
              "All scores must be numeric. Found non-numeric score(s) in %s for %s.",
              method_name, list_pair
            ))
          }
          
          if (any(scores < 0 | scores > 1, na.rm = TRUE)) {
            cli::cli_abort(sprintf(
              "All scores must be between 0 and 1 (inclusive). Found score(s) out of range in %s for %s.",
              method_name, list_pair
            ))
          }
        }
      }
      
      # Validate list_names are character and non-empty
      if (length(self$list_names) == 0) {
        cli::cli_abort("list_names cannot be empty.")
      }
      
      # Additional validation can be done by components
      if (!is.null(self$calculator)) {
        self$calculator$validate(self)
      }
      
      NULL
    }
  )
)

#' Method implementation for average_similarity to work with SimilarityBase
#' @export
average_similarity.SimilarityBase <- function(x, ...) {
  x$calc_average_similarity()
}

#' Method implementation for pair_averages to work with SimilarityBase
#' @export
pair_averages.SimilarityBase <- function(x, method = NULL, ...) {
  x$calc_pair_averages(method)
}

#' Method implementation for plot to work with SimilarityBase
#' @export
plot.SimilarityBase <- function(x, type = "combined", palette = "Set2", ...) {
  x$make_plot(type, palette, ...)
}

#' Method implementation for summary to work with SimilarityBase
#' @export
summary.SimilarityBase <- function(object, ...) {
  object$make_summary(...)
}

#' Print Method for summary.similarity Objects
#'
#' @param x A summary.similarity object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.summary.similarity <- function(x, ...) {
  cli::cli_h1("Summary: Similarity Analysis")
  
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