#' Calculate Factor Similarity Score
#'
#' @param cat1 First categorical value to compare
#' @param cat2 Second categorical value to compare
#' @param method Method for comparison: "exact", "jaccard", "overlap", "matching"
#' @param levels Character vector of all possible levels
#'
#' @return Normalized similarity score between 0 and 1
#' @noRd
calculate_factor_similarity <- function(cat1, cat2, method, levels) {
  cat1 <- as.character(cat1)
  cat2 <- as.character(cat2)

  if (!cat1 %in% levels) cat1 <- NA
  if (!cat2 %in% levels) cat2 <- NA

  if (is.na(cat1) && is.na(cat2)) {
    return(1)
  }
  if (is.na(cat1) || is.na(cat2)) {
    return(0)
  }

  switch(method,
    "exact" = {
      as.numeric(cat1 == cat2)
    },
    "jaccard" = {
      cat1_idx <- match(cat1, levels)
      cat2_idx <- match(cat2, levels)

      cat1_vec <- numeric(length(levels))
      cat2_vec <- numeric(length(levels))

      cat1_vec[cat1_idx] <- 1
      cat2_vec[cat2_idx] <- 1

      sum(cat1_vec & cat2_vec) / sum(cat1_vec | cat2_vec)
    },
    "overlap" = {
      if (cat1 == cat2) {
        return(1)
      }

      cat1_idx <- match(cat1, levels)
      cat2_idx <- match(cat2, levels)

      level_distance <- abs(cat1_idx - cat2_idx) / (length(levels) - 1)
      1 - level_distance
    },
    "matching" = {
      as.numeric(cat1 == cat2)
    }
  )
}

#' Calculate Similarity Scores Between Two Lists of Categorical Values
#'
#' @param list1 First list of categorical values to compare
#' @param list2 Second list of categorical values to compare
#' @param method Method for comparison
#' @param levels Character vector of all possible levels
#'
#' @return Named numeric vector of similarity scores
#' @noRd
calculate_factor_scores <- function(list1, list2, method, levels) {
  if (length(list1) != length(list2)) {
    cli::cli_abort("All lists must have same length")
  }

  scores <- purrr::map2_dbl(
    list1,
    list2,
    function(x, y) calculate_factor_similarity(x, y, method = method, levels = levels)
  )

  names(scores) <- unlist(list1)
  scores
}

#' Validate Factor Input Lists
#'
#' @param ... Lists of character or factor values to validate
#' @param levels Character vector of all allowed levels
#'
#' @return TRUE if all inputs are valid, error message otherwise
#' @noRd
validate_factor_inputs <- function(..., levels) {
  inputs <- list(...)

  if (length(inputs) < 2) {
    cli::cli_abort("At least two inputs required")
  }

  is_valid <- function(x) {
    if (is.list(x)) {
      return(all(vapply(x, is_valid, logical(1))))
    } else {
      return(is.character(x) || is.factor(x) || is.na(x))
    }
  }

  is_valid_list <- function(x) {
    if (!is.list(x)) {
      return(FALSE)
    }
    return(all(vapply(x, is_valid, logical(1))))
  }

  invalid_inputs <- which(!vapply(inputs, is_valid_list, logical(1)))

  if (length(invalid_inputs) > 0) {
    cli::cli_abort(c(
      "All inputs must be lists containing only character or factor values",
      "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
    ))
  }

  if (is.null(levels) || length(levels) == 0) {
    cli::cli_abort("Must provide non-empty levels parameter")
  }

  TRUE
}

#' Compare Categorical Data Similarity Across Multiple Lists
#'
#' @param ... Lists of categorical values (character or factor) to compare
#' @param method Character vector of similarity methods. Choose from: "exact",
#'   "jaccard", "overlap", "matching" (default: "exact")
#' @param levels Character vector of all allowed levels for comparison
#'
#' @return An S7 object of type "similar_factor" containing:
#'   - scores: Numeric similarity scores by method and comparison
#'   - summary: Summary statistics by method and comparison
#'   - methods: Methods used for comparison
#'   - list_names: Names of compared lists
#'   - levels: Levels used for categorical comparison
#'
#' @examples
#' list1 <- list("apple", "orange", "unknown")
#' list2 <- list("apple", "orange", "unknown")
#' list3 <- list("apple", "pineapple", "banana")
#' result <- same_factor(list1, list2, list3, levels = c("apple", "orange", "banana"))
#' print(result)
#'
#' # Compare with multiple methods
#' result2 <- same_factor(list1, list2, method = c("exact", "jaccard"))
#' @export
same_factor <- function(..., method = "exact", levels) {
  valid_methods <- c(
    "exact", "jaccard", "overlap", "matching"
  )

  inputs <- list(...)

  validate_factor_inputs(..., levels = levels)

  method <- unique(if (length(method) == 1) c(method) else method)

  invalid_methods <- method[!method %in% valid_methods]
  if (length(invalid_methods) > 0) {
    cli::cli_abort(c(
      "All methods must be one of: {paste(valid_methods, collapse = ', ')}"
    ))
  }

  dots <- as.list(substitute(list(...)))[-1]
  list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    names(dots)
  } else {
    purrr::map_chr(dots, ~ deparse(.x)[1])
  }

  flatten_list <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    unlist(lapply(x, flatten_list))
  }

  flattened_inputs <- lapply(inputs, flatten_list)

  lengths <- purrr::map_int(flattened_inputs, length)
  if (length(unique(lengths)) > 1) {
    cli::cli_abort("All lists must have same length after flattening")
  }

  pairs <- get_pairwise_combinations(length(flattened_inputs))

  scores <- purrr::map(method, function(m) {
    pair_scores <- purrr::map2(pairs$first, pairs$second, function(idx1, idx2) {
      pair_name <- paste0(list_names[idx1], "_", list_names[idx2])

      pair_result <- calculate_factor_scores(
        flattened_inputs[[idx1]],
        flattened_inputs[[idx2]],
        method = m,
        levels = levels
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

  # Check for nested structures and prepare structured scores if needed
  structured_scores <- scores
  has_nested <- FALSE

  for (i in seq_along(inputs)) {
    if (length(names(inputs[[i]])) > 0) {
      has_nested <- TRUE
      break
    }
  }

  if (has_nested) {
    # Find all unique keys across all nested inputs
    all_keys <- unique(unlist(lapply(inputs, names)))

    # Create structured scores for each key
    structured_scores <- list()

    for (key in all_keys) {
      structured_scores[[key]] <- list()

      for (m in method) {
        structured_scores[[key]][[m]] <- scores[[m]]
      }
    }
  }

  # If we have structured data, modify the scores to include the structure
  final_scores <- if (has_nested) structured_scores else scores

  # Return the S7 object directly
  similar_factor(
    scores = final_scores,
    summary = summaries,
    methods = method,
    list_names = list_names,
    levels = levels
  )
}
