#' Base Similarity Formatter
#'
#' @description
#' `SimilarityFormatterBase` is an R6 class that defines the interface for
#' similarity formatters that handle output display.
#'
#' @importFrom R6 R6Class
#' @export
SimilarityFormatterBase <- R6::R6Class("SimilarityFormatterBase",
  public = list(
    #' @description
    #' Get the type of formatter
    #'
    #' @return A string identifying the formatter type
    get_type = function() {
      cli::cli_abort("get_type() must be implemented by derived classes")
    },
    
    #' @description
    #' Print a similarity object
    #'
    #' @param obj The similarity object to print
    #' @param ... Additional arguments (not used)
    #'
    #' @return Invisibly returns the input object
    print = function(obj, ...) {
      cli::cli_h1("Similarity Analysis")
      cli::cli_text("Methods used: {.val {paste(obj$methods, collapse = ', ')}}")
      cli::cli_text("Lists compared: {.val {paste(obj$list_names, collapse = ', ')}}")
      
      overall_avgs <- obj$average_similarity()
      
      cli::cli_h2("Overall Method Averages")
      cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
        paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
      }))
      
      purrr::walk(obj$methods, function(method) {
        cli::cli_h2("Method: {.field {method}}")
        
        purrr::walk(names(obj$summary[[method]]), function(pair_name) {
          cli::cli_h3("Comparison: {.val {pair_name}}")
          
          cli::cli_h3("Summary Statistics")
          summary_stats <- obj$summary[[method]][[pair_name]]
          
          cli::cli_bullets(c(
            "*" = "Mean: {.val {round(summary_stats$mean, 3)}}",
            "*" = "Median: {.val {round(summary_stats$median, 3)}}",
            "*" = "SD: {.val {round(summary_stats$sd, 3)}}"
          ))
        })
      })
      
      invisible(obj)
    }
  )
)

#' Text Similarity Formatter
#'
#' @description
#' Formatter for text similarity output
#'
#' @importFrom R6 R6Class
#' @export
TextSimilarityFormatter <- R6::R6Class("TextSimilarityFormatter",
  inherit = SimilarityFormatterBase,
  public = list(
    #' @description
    #' Get the type of formatter
    #'
    #' @return "similarity_text"
    get_type = function() {
      "similarity_text"
    },
    
    #' @description
    #' Print a text similarity object
    #'
    #' @param obj The similarity object to print
    #' @param ... Additional arguments (not used)
    #'
    #' @return Invisibly returns the input object
    print = function(obj, ...) {
      cli::cli_h1("Text Similarity Analysis")
      cli::cli_text("Methods used: {.val {paste(obj$methods, collapse = ', ')}}")
      cli::cli_text("Lists compared: {.val {paste(obj$list_names, collapse = ', ')}}")
      
      overall_avgs <- obj$average_similarity()
      
      cli::cli_h2("Overall Method Averages")
      cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
        paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
      }))
      
      purrr::walk(obj$methods, function(method) {
        cli::cli_h2("Method: {.field {method}}")
        
        purrr::walk(names(obj$summary[[method]]), function(pair_name) {
          cli::cli_h3("Comparison: {.val {pair_name}}")
          
          cli::cli_h3("Summary Statistics")
          summary_stats <- obj$summary[[method]][[pair_name]]
          
          cli::cli_bullets(c(
            "*" = "Mean: {.val {round(summary_stats$mean, 3)}}",
            "*" = "Median: {.val {round(summary_stats$median, 3)}}",
            "*" = "SD: {.val {round(summary_stats$sd, 3)}}",
            "*" = "IQR: {.val {round(summary_stats$iqr, 3)}}",
            "*" = "Range: [{.val {round(summary_stats$min, 3)}} - {.val {round(summary_stats$max, 3)}}]"
          ))
        })
      })
      
      invisible(obj)
    }
  )
)

#' Factor Similarity Formatter
#'
#' @description
#' Formatter for factor similarity output
#'
#' @importFrom R6 R6Class
#' @export
FactorSimilarityFormatter <- R6::R6Class("FactorSimilarityFormatter",
  inherit = SimilarityFormatterBase,
  public = list(
    #' @description
    #' Get the type of formatter
    #'
    #' @return "similarity_factor"
    get_type = function() {
      "similarity_factor" 
    },
    
    #' @description
    #' Print a factor similarity object
    #'
    #' @param obj The similarity object to print
    #' @param ... Additional arguments (not used)
    #'
    #' @return Invisibly returns the input object
    print = function(obj, ...) {
      cli::cli_h1("Factor Similarity Analysis")
      cli::cli_text("Methods used: {.val {paste(obj$methods, collapse = ', ')}}")
      cli::cli_text("Lists compared: {.val {paste(obj$list_names, collapse = ', ')}}")
      
      # Show levels if available
      if (!is.null(obj$calculator) && !is.null(obj$calculator$levels)) {
        cli::cli_text("Levels: {.val {paste(obj$calculator$levels, collapse = ', ')}}")
      }
      
      overall_avgs <- obj$average_similarity()
      
      cli::cli_h2("Overall Method Averages")
      cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
        paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
      }))
      
      purrr::walk(obj$methods, function(method) {
        cli::cli_h2("Method: {.field {method}}")
        
        purrr::walk(names(obj$summary[[method]]), function(pair_name) {
          cli::cli_h3("Comparison: {.val {pair_name}}")
          
          cli::cli_h3("Summary Statistics")
          summary_stats <- obj$summary[[method]][[pair_name]]
          
          cli::cli_bullets(c(
            "*" = "Mean: {.val {round(summary_stats$mean, 3)}}",
            "*" = "Median: {.val {round(summary_stats$median, 3)}}",
            "*" = "SD: {.val {round(summary_stats$sd, 3)}}",
            "*" = "IQR: {.val {round(summary_stats$iqr, 3)}}",
            "*" = "Range: [{.val {round(summary_stats$min, 3)}} - {.val {round(summary_stats$max, 3)}}]"
          ))
        })
      })
      
      invisible(obj)
    }
  )
)

#' Number Similarity Formatter
#'
#' @description
#' Formatter for number similarity output
#'
#' @importFrom R6 R6Class
#' @export
NumberSimilarityFormatter <- R6::R6Class("NumberSimilarityFormatter", 
  inherit = SimilarityFormatterBase,
  public = list(
    #' @description
    #' Get the type of formatter
    #'
    #' @return "similarity_number"
    get_type = function() {
      "similarity_number"
    },
    
    #' @description
    #' Print a number similarity object
    #'
    #' @param obj The similarity object to print
    #' @param ... Additional arguments (not used)
    #'
    #' @return Invisibly returns the input object
    print = function(obj, ...) {
      cli::cli_h1("Number Similarity Analysis")
      cli::cli_text("Methods used: {.val {paste(obj$methods, collapse = ', ')}}")
      cli::cli_text("Lists compared: {.val {paste(obj$list_names, collapse = ', ')}}")
      
      # Show raw value stats if available
      if (!is.null(obj$calculator) && !is.null(obj$calculator$raw_values)) {
        raw_stats <- list(
          min = min(obj$calculator$raw_values, na.rm = TRUE),
          max = max(obj$calculator$raw_values, na.rm = TRUE),
          mean = mean(obj$calculator$raw_values, na.rm = TRUE),
          median = stats::median(obj$calculator$raw_values, na.rm = TRUE)
        )
        
        cli::cli_h2("Raw Value Statistics")
        cli::cli_bullets(c(
          "*" = "Min: {.val {round(raw_stats$min, 3)}}",
          "*" = "Max: {.val {round(raw_stats$max, 3)}}",
          "*" = "Mean: {.val {round(raw_stats$mean, 3)}}",
          "*" = "Median: {.val {round(raw_stats$median, 3)}}"
        ))
      }
      
      overall_avgs <- obj$average_similarity()
      
      cli::cli_h2("Overall Method Averages")
      cli::cli_bullets(purrr::map_chr(names(overall_avgs), function(method) {
        paste0("* ", method, ": {.val ", round(overall_avgs[method], 3), "}")
      }))
      
      purrr::walk(obj$methods, function(method) {
        cli::cli_h2("Method: {.field {method}}")
        
        purrr::walk(names(obj$summary[[method]]), function(pair_name) {
          cli::cli_h3("Comparison: {.val {pair_name}}")
          
          cli::cli_h3("Summary Statistics")
          summary_stats <- obj$summary[[method]][[pair_name]]
          
          cli::cli_bullets(c(
            "*" = "Mean: {.val {round(summary_stats$mean, 3)}}",
            "*" = "Median: {.val {round(summary_stats$median, 3)}}",
            "*" = "SD: {.val {round(summary_stats$sd, 3)}}",
            "*" = "IQR: {.val {round(summary_stats$iqr, 3)}}",
            "*" = "Range: [{.val {round(summary_stats$min, 3)}} - {.val {round(summary_stats$max, 3)}}]"
          ))
        })
      })
      
      invisible(obj)
    }
  )
)

#' Print Method for summary.similarity_text Objects
#'
#' @param x A summary.similarity_text object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.summary.similarity_text <- function(x, ...) {
  cli::cli_h1("Summary: Text Similarity Analysis")
  
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

#' Print Method for summary.similarity_factor Objects
#'
#' @param x A summary.similarity_factor object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.summary.similarity_factor <- function(x, ...) {
  cli::cli_h1("Summary: Factor Similarity Analysis")
  
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

#' Print Method for summary.similarity_number Objects
#'
#' @param x A summary.similarity_number object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.summary.similarity_number <- function(x, ...) {
  cli::cli_h1("Summary: Number Similarity Analysis")
  
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