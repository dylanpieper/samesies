#' Calculate String Similarity Score
#'
#' @param str1 First string to compare
#' @param str2 Second string to compare
#' @param method Method for comparison: "osa", "lv", "dl", "hamming", "lcs", "qgram",
#'   "cosine", "jaccard", "jw", or "soundex"
#' @param q Size of q-gram for q-gram based methods (default: 1)
#' @param p Winkler scaling factor for "jw" method (default: 0.1)
#' @param bt Booth matching threshold
#' @param weight Vector of weights for operations (deletion, insertion, substitution, transposition)
#'
#' @return Normalized similarity score between 0 and 1
#' @noRd
calculate_similarity <- function(str1, str2, method, q = 1, p = NULL, bt = 0,
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
}

#' Calculate Similarity Scores Between Two Lists
#'
#' @param list1 First list of strings to compare
#' @param list2 Second list of strings to compare
#' @param method Method for comparison (e.g., "jw", "lv")
#' @param ... Additional arguments passed to calculate_similarity()
#'
#' @return Named numeric vector of similarity scores
#' @noRd
calculate_scores <- function(list1, list2, method, ...) {
  if (length(list1) != length(list2)) {
    cli::cli_abort("All lists must have same length")
  }

  scores <- purrr::map2_dbl(
    list1,
    list2,
    function(x, y) calculate_similarity(x, y, method = method, ...)
  )

  names(scores) <- unlist(list1)
  scores
}

#' Validate Text Input Lists
#'
#' @param ... Lists of character strings to validate
#'
#' @return TRUE if all inputs are valid, error message otherwise
#'
#' @details
#' Checks that:
#' - At least 2 inputs are provided
#' - All inputs are lists containing only character strings
#'
#' @noRd
validate_text_inputs <- function(...) {
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
}

#' Compare Text Similarity Across Multiple Lists
#'
#' @param ... Lists of character strings to compare
#' @param method Character vector of similarity methods. Choose from: "osa",
#'   "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"
#'   (default: "jw")
#' @param q Size of q-gram for q-gram based methods (default: 1)
#' @param p Winkler scaling factor for "jw" method (default: 0.1)
#' @param bt Booth matching threshold
#' @param weight Vector of weights for operations: deletion (d), insertion (i),
#'   substitution (s), transposition (t)
#'
#' @return An S7 class object of type "similar_text" containing:
#'   - scores: Numeric similarity scores by method and comparison
#'   - summary: Summary statistics by method and comparison
#'   - methods: Methods used for comparison
#'   - list_names: Names of compared lists
#'
#' @examples
#' list1 <- list("hello", "world")
#' list2 <- list("helo", "word")
#' result <- same_text(list1, list2, method = "jw")
#' print(result)
#'
#' # Compare with multiple methods
#' result2 <- same_text(list1, list2, method = c("jw", "lv"))
#'
#' # Compare multiple lists
#' list3 <- list("hllo", "wrld")
#' result3 <- same_text(list1, list2, list3, method = "jw")
#' @export
same_text <- function(..., method = "jw", q = 1, p = NULL, bt = 0,
                      weight = c(d = 1, i = 1, s = 1, t = 1)) {
  valid_methods <- c(
    "osa", "lv", "dl", "hamming", "lcs", "qgram",
    "cosine", "jaccard", "jw", "soundex"
  )

  inputs <- list(...)

  if (length(inputs) < 2) {
    cli::cli_abort("At least two inputs required")
  }

  is_valid_element <- function(x) {
    if (is.list(x)) {
      return(all(vapply(x, is_valid_element, logical(1))))
    } else {
      return(is.character(x) && length(x) == 1)
    }
  }

  is_valid_list <- function(x) {
    if (!is.list(x)) {
      return(FALSE)
    }
    return(all(vapply(x, is_valid_element, logical(1))))
  }

  invalid_inputs <- which(!vapply(inputs, is_valid_list, logical(1)))

  if (length(invalid_inputs) > 0) {
    cli::cli_abort(c(
      "All inputs must be lists containing only character strings",
      "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
    ))
  }

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

      pair_result <- calculate_scores(
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

  similar_text(
    scores = scores,
    summary = summaries,
    methods = method,
    list_names = list_names
  )
}
