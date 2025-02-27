#' Generic method for plot methods
#' 
#' @param x A similarity object
#' @param type The type of plot to create
#' @param ... Additional arguments passed to specific methods
#' 
#' @return A ggplot2 object
#' 
#' @export
plot_methods <- function(x, type = "default", ...) {
  UseMethod("plot_methods")
}

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

#' @rdname average_similarity
#' @export
average_similarity.similar <- function(x, ...) {
  mean_scores_by_method(x@scores)
}

#' @rdname pair_averages
#' @export
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

#' @rdname plot_methods
#' @export
plot_methods.similar <- function(x, type = "combined", palette = "Set2", ...) {
  plot_data <- purrr::map_df(x@methods, function(method) {
    purrr::map_df(names(x@scores[[method]]), function(pair_name) {
      scores <- x@scores[[method]][[pair_name]]
      # Ensure we're handling both list and vector inputs correctly
      if (is.list(scores) && !is.null(scores)) {
        scores <- unlist(scores)
      }
      
      # Convert to numeric with error checking
      numeric_scores <- suppressWarnings(as.numeric(scores))
      if (any(is.na(numeric_scores)) && !all(is.na(scores))) {
        # If conversion created NAs where there weren't any before
        # Use a safer approach by extracting numeric values directly
        numeric_scores <- vapply(scores, function(s) {
          if (is.numeric(s)) return(s)
          if (is.character(s)) {
            parsed <- suppressWarnings(as.numeric(s))
            if (!is.na(parsed)) return(parsed)
          }
          return(NA_real_)
        }, numeric(1))
      }
      
      data.frame(
        method = method,
        pair = pair_name,
        score = numeric_scores
      )
    })
  })
  
  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Method", y = "Similarity Score") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Return base plot that child classes can customize
  base_plot
}

#' Generic plot method for similarity objects
#'
#' @param x A similarity object
#' @param type The type of plot to create
#' @param ... Additional arguments passed to specific methods
#'
#' @return A ggplot2 object
#'
#' @export
plot.similar <- function(x, type = "default", ...) {
  # Dispatch to the appropriate plot_methods function
  plot_methods(x, type = type, ...)
}

#' Plot method for similar_text objects
#'
#' @param x A similar_text object
#' @param type Plot type to create: "default"/"combined", "boxplot", "violin", or "point"
#' @param palette Color palette to use for plot elements
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object visualizing text similarity scores
#'
#' @export
plot_methods.similar_text <- function(x, type = "default", palette = "Set2", ...) {
  if (type == "default") type <- "combined"
  
  # Prepare plot data
  plot_data <- purrr::map_df(x@methods, function(method) {
    purrr::map_df(names(x@scores[[method]]), function(pair_name) {
      scores <- x@scores[[method]][[pair_name]]
      # Ensure we're handling both list and vector inputs correctly
      if (is.list(scores) && !is.null(scores)) {
        scores <- unlist(scores)
      }
      
      # Convert to numeric with error checking
      numeric_scores <- suppressWarnings(as.numeric(scores))
      if (any(is.na(numeric_scores)) && !all(is.na(scores))) {
        # If conversion created NAs where there weren't any before
        # Use a safer approach by extracting numeric values directly
        numeric_scores <- vapply(scores, function(s) {
          if (is.numeric(s)) return(s)
          if (is.character(s)) {
            parsed <- suppressWarnings(as.numeric(s))
            if (!is.na(parsed)) return(parsed)
          }
          return(NA_real_)
        }, numeric(1))
      }
      
      data.frame(
        method = method,
        pair = pair_name,
        score = numeric_scores
      )
    })
  })
  
  # Base plot
  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Method", y = "Similarity Score") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  # Create specialized text visualization: layered beeswarm/violin/boxplot
  switch(type,
    "combined" = base_plot +
      ggplot2::geom_violin(ggplot2::aes(fill = method), alpha = 0.4) +
      ggplot2::geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
      ggbeeswarm::geom_beeswarm(alpha = 0.4, size = 1) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    "boxplot" = base_plot +
      ggplot2::geom_boxplot(ggplot2::aes(fill = method)) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    "point" = base_plot +
      ggplot2::geom_jitter(ggplot2::aes(color = pair), width = 0.2, alpha = 0.7) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = 3, color = "black"),
    "violin" = base_plot +
      ggplot2::geom_violin(ggplot2::aes(fill = method)) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    cli::cli_abort("Invalid plot type. Choose 'combined', 'boxplot', 'point', or 'violin'")
  )
}

#' Plot method for similar_factor objects
#'
#' @param x A similar_factor object
#' @param type Plot type to create: "default", "heatmap", "boxplot", or "point"
#' @param palette Color palette to use for plot elements
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object visualizing factor similarity scores
#'
#' @export
plot_methods.similar_factor <- function(x, type = "default", palette = "Set2", ...) {
  # Prepare plot data
  plot_data <- purrr::map_df(x@methods, function(method) {
    purrr::map_df(names(x@scores[[method]]), function(pair_name) {
      scores <- x@scores[[method]][[pair_name]]
      # Ensure we're handling both list and vector inputs correctly
      if (is.list(scores) && !is.null(scores)) {
        scores <- unlist(scores)
      }
      
      # Convert to numeric with error checking
      numeric_scores <- suppressWarnings(as.numeric(scores))
      if (any(is.na(numeric_scores)) && !all(is.na(scores))) {
        # If conversion created NAs where there weren't any before
        # Use a safer approach by extracting numeric values directly
        numeric_scores <- vapply(scores, function(s) {
          if (is.numeric(s)) return(s)
          if (is.character(s)) {
            parsed <- suppressWarnings(as.numeric(s))
            if (!is.na(parsed)) return(parsed)
          }
          return(NA_real_)
        }, numeric(1))
      }
      
      data.frame(
        method = method,
        pair = pair_name,
        score = numeric_scores
      )
    })
  })
  
  # Default is standard boxplot for factors
  if (type == "default") {
    base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
      ggplot2::theme_minimal() +
      ggplot2::geom_boxplot(ggplot2::aes(fill = method)) +
      ggplot2::labs(x = "Method", y = "Similarity Score") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank()
      ) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none")
    
    return(base_plot)
  }
  
  # For heatmap visualization (specialized factor visualization)
  if (type == "heatmap") {
    # Create data for heatmap
    heatmap_data <- plot_data %>%
      dplyr::group_by(method, pair) %>%
      dplyr::summarize(
        exact_match_pct = mean(score == 1),
        avg_score = mean(score)
      ) %>%
      dplyr::ungroup()
    
    # Split the pair names into separate components
    pairs_split <- strsplit(heatmap_data$pair, "_")
    heatmap_data$list1 <- sapply(pairs_split, `[`, 1)
    heatmap_data$list2 <- sapply(pairs_split, `[`, 2)
    
    # Create the heatmap
    return(
      ggplot2::ggplot(heatmap_data, ggplot2::aes(x = list1, y = list2, fill = avg_score)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::facet_wrap(~method) +
        ggplot2::scale_fill_gradient2(
          low = "red", mid = "yellow", high = "green",
          midpoint = 0.5, limits = c(0, 1),
          name = "Avg. Score"
        ) +
        ggplot2::geom_text(ggplot2::aes(
          label = scales::percent(exact_match_pct, accuracy = 1)
        ), color = "black", size = 3) +
        ggplot2::labs(
          title = "Factor Similarity Heatmap",
          x = "List 1",
          y = "List 2"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank()
        )
    )
  }
  
  # Base plot for other types
  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Method", y = "Similarity Score") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  # Other standard plot types
  switch(type,
    "boxplot" = base_plot +
      ggplot2::geom_boxplot(ggplot2::aes(fill = method)) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    "point" = base_plot +
      ggplot2::geom_jitter(ggplot2::aes(color = pair), width = 0.2, alpha = 0.7) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = 3, color = "black"),
    cli::cli_abort("Invalid plot type. Choose 'default', 'heatmap', 'boxplot', or 'point'")
  )
}

#' Plot method for similar_number objects
#'
#' @param x A similar_number object
#' @param type Plot type to create: "default", "histogram", "boxplot", or "point"
#' @param palette Color palette to use for plot elements
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object visualizing numeric similarity scores
#'
#' @export
plot_methods.similar_number <- function(x, type = "default", palette = "Set2", ...) {
  # Prepare plot data
  plot_data <- purrr::map_df(x@methods, function(method) {
    purrr::map_df(names(x@scores[[method]]), function(pair_name) {
      scores <- x@scores[[method]][[pair_name]]
      # Ensure we're handling both list and vector inputs correctly
      if (is.list(scores) && !is.null(scores)) {
        scores <- unlist(scores)
      }
      
      # Convert to numeric with error checking
      numeric_scores <- suppressWarnings(as.numeric(scores))
      if (any(is.na(numeric_scores)) && !all(is.na(scores))) {
        # If conversion created NAs where there weren't any before
        # Use a safer approach by extracting numeric values directly
        numeric_scores <- vapply(scores, function(s) {
          if (is.numeric(s)) return(s)
          if (is.character(s)) {
            parsed <- suppressWarnings(as.numeric(s))
            if (!is.na(parsed)) return(parsed)
          }
          return(NA_real_)
        }, numeric(1))
      }
      
      data.frame(
        method = method,
        pair = pair_name,
        score = numeric_scores
      )
    })
  })
  
  # Default is standard boxplot for numbers
  if (type == "default") {
    base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
      ggplot2::theme_minimal() +
      ggplot2::geom_boxplot(ggplot2::aes(fill = method)) +
      ggplot2::labs(x = "Method", y = "Similarity Score") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank()
      ) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none")
    
    return(base_plot)
  }
  
  # Specialized histogram visualization for numbers
  if (type == "histogram") {
    return(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = score, fill = method)) +
        ggplot2::geom_histogram(position = "dodge", bins = 20, alpha = 0.7) +
        ggplot2::facet_wrap(~method) +
        ggplot2::scale_fill_brewer(palette = palette) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Distribution of Similarity Scores by Method",
          x = "Similarity Score", 
          y = "Count"
        ) +
        ggplot2::theme(legend.position = "none")
    )
  }
  
  # Base plot for other types
  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = method, y = score)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Method", y = "Similarity Score") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  # Other standard plot types
  switch(type,
    "boxplot" = base_plot +
      ggplot2::geom_boxplot(ggplot2::aes(fill = method)) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme(legend.position = "none"),
    "point" = base_plot +
      ggplot2::geom_jitter(ggplot2::aes(color = pair), width = 0.2, alpha = 0.7) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = 3, color = "black"),
    cli::cli_abort("Invalid plot type. Choose 'default', 'histogram', 'boxplot', or 'point'")
  )
}

#' @export
print.similar <- function(x, ...) {
  cli::cli_h1("Similarity Analysis")
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
        "*" = "SD: {.val {round(summary_stats$sd, 3)}}"
      ))
    })
  })
  
  invisible(x)
}

#' @export
summary.similar <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)
  
  result <- list(
    methods = object@methods,
    list_names = object@list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )
  
  class(result) <- "summary.similar"
  return(result)
}

#' @export
print.summary.similar <- function(x, ...) {
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