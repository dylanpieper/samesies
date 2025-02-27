#' Base Similarity Plotter
#'
#' @description
#' `SimilarityPlotterBase` is an R6 class that defines the interface for
#' similarity plotters that create visualizations.
#'
#' @importFrom R6 R6Class
#' @export
SimilarityPlotterBase <- R6::R6Class("SimilarityPlotterBase",
  public = list(
    #' @description
    #' Get the type of plotter
    #'
    #' @return A string identifying the plotter type
    get_type = function() {
      cli::cli_abort("get_type() must be implemented by derived classes")
    },
    
    #' @description
    #' Create a plot of similarity scores
    #'
    #' @param obj The similarity object to plot
    #' @param type Plot type, varies by plotter
    #' @param palette Color palette to use
    #' @param ... Additional arguments passed to specific plotters
    #'
    #' @return A ggplot2 object
    plot = function(obj, type = "combined", palette = "Set2", ...) {
      # Prepare common data frame for plotting
      plot_data <- purrr::map_df(obj$methods, function(method) {
        purrr::map_df(names(obj$scores[[method]]), function(pair_name) {
          data.frame(
            method = method,
            pair = pair_name,
            score = unlist(obj$scores[[method]][[pair_name]])
          )
        })
      })
      
      # Create base plot
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
  )
)

#' Text Similarity Plotter
#'
#' @description
#' Plotter for text similarity visualizations
#'
#' @importFrom R6 R6Class
#' @export
TextSimilarityPlotter <- R6::R6Class("TextSimilarityPlotter",
  inherit = SimilarityPlotterBase,
  public = list(
    #' @description
    #' Get the type of plotter
    #'
    #' @return "similarity_text"
    get_type = function() {
      "similarity_text"
    },
    
    #' @description
    #' Create a plot of text similarity scores
    #'
    #' @param obj The similarity object to plot
    #' @param type Plot type: "combined", "boxplot", "point", or "violin"
    #' @param palette Color palette to use
    #' @param ... Additional arguments (not used)
    #'
    #' @return A ggplot2 object
    plot = function(obj, type = "combined", palette = "Set2", ...) {
      base_plot <- super$plot(obj, type, palette, ...)
      
      switch(type,
        "combined" = base_plot +
          ggplot2::geom_violin(ggplot2::aes(fill = method), alpha = 0.4) +
          ggplot2::geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
          ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
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
  )
)

#' Factor Similarity Plotter
#'
#' @description
#' Plotter for factor similarity visualizations
#'
#' @importFrom R6 R6Class
#' @export
FactorSimilarityPlotter <- R6::R6Class("FactorSimilarityPlotter",
  inherit = SimilarityPlotterBase,
  public = list(
    #' @description
    #' Get the type of plotter
    #'
    #' @return "similarity_factor"
    get_type = function() {
      "similarity_factor"
    },
    
    #' @description
    #' Create a plot of factor similarity scores
    #'
    #' @param obj The similarity object to plot
    #' @param type Plot type: "combined", "boxplot", "point", "violin", or "heatmap"
    #' @param palette Color palette to use
    #' @param ... Additional arguments (not used)
    #'
    #' @return A ggplot2 object
    plot = function(obj, type = "combined", palette = "Set2", ...) {
      # For most types, use the base implementation
      if (type != "heatmap") {
        base_plot <- super$plot(obj, type, palette, ...)
        
        return(switch(type,
          "combined" = base_plot +
            ggplot2::geom_violin(ggplot2::aes(fill = method), alpha = 0.4) +
            ggplot2::geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
            ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
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
          cli::cli_abort("Invalid plot type. Choose 'combined', 'boxplot', 'point', 'violin', or 'heatmap'")
        ))
      }
      
      # For heatmap, create a special plot (if levels are available)
      if (is.null(obj$calculator) || is.null(obj$calculator$levels)) {
        cli::cli_abort("Cannot create heatmap without factor levels")
      }
      
      # Get the average scores by method and pair
      pair_avgs <- obj$pair_averages()
      
      # Prepare data for heatmap
      plot_data <- purrr::map_df(unique(pair_avgs$method), function(m) {
        method_data <- pair_avgs[pair_avgs$method == m, ]
        # Split pair names into two components
        pairs_split <- strsplit(method_data$pair, "_")
        
        data.frame(
          method = m,
          list1 = sapply(pairs_split, `[`, 1),
          list2 = sapply(pairs_split, `[`, 2),
          score = method_data$avg_score,
          stringsAsFactors = FALSE
        )
      })
      
      # Create heatmap for each method
      ggplot2::ggplot(plot_data, ggplot2::aes(x = list1, y = list2, fill = score)) +
        ggplot2::geom_tile() +
        ggplot2::facet_wrap(~method) +
        ggplot2::scale_fill_gradient2(
          low = "red", mid = "yellow", high = "green",
          midpoint = 0.5, limits = c(0, 1)
        ) +
        ggplot2::labs(
          title = "Factor Similarity Heatmap",
          x = "List 1",
          y = "List 2",
          fill = "Score"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank()
        )
    }
  )
)

#' Number Similarity Plotter
#'
#' @description
#' Plotter for number similarity visualizations
#'
#' @importFrom R6 R6Class
#' @export
NumberSimilarityPlotter <- R6::R6Class("NumberSimilarityPlotter",
  inherit = SimilarityPlotterBase,
  public = list(
    #' @description
    #' Get the type of plotter
    #'
    #' @return "similarity_number"
    get_type = function() {
      "similarity_number"
    },
    
    #' @description
    #' Create a plot of number similarity scores
    #'
    #' @param obj The similarity object to plot
    #' @param type Plot type: "combined", "boxplot", "point", "violin", or "scatter"
    #' @param palette Color palette to use
    #' @param ... Additional arguments (not used)
    #'
    #' @return A ggplot2 object
    plot = function(obj, type = "combined", palette = "Set2", ...) {
      # For most types, use the base implementation
      if (type != "scatter") {
        base_plot <- super$plot(obj, type, palette, ...)
        
        return(switch(type,
          "combined" = base_plot +
            ggplot2::geom_violin(ggplot2::aes(fill = method), alpha = 0.4) +
            ggplot2::geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
            ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
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
          cli::cli_abort("Invalid plot type. Choose 'combined', 'boxplot', 'point', 'violin', or 'scatter'")
        ))
      }
      
      # For scatter plot, create a special plot (if raw_values are available)
      if (is.null(obj$calculator) || is.null(obj$calculator$raw_values)) {
        cli::cli_abort("Cannot create scatter plot without raw values")
      }
      
      # Create scatter plot to show original data distribution
      raw_data <- data.frame(value = obj$calculator$raw_values)
      
      ggplot2::ggplot(raw_data, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(binwidth = function(x) diff(range(x)) / 30, 
                               fill = "skyblue", color = "darkblue", alpha = 0.7) +
        ggplot2::labs(
          title = "Distribution of Numeric Values",
          x = "Value",
          y = "Count"
        ) +
        ggplot2::theme_minimal()
    }
  )
)