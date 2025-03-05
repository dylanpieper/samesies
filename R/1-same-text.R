#' Compare Text Similarity Across Lists
#'
#' @param ... Lists of character strings to compare
#' @param method Character vector of similarity methods from `stringdist`. Choose from:
#'   "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"
#'   (default: all)
#' @param q Size of q-gram for q-gram based methods (default: 1)
#' @param p Winkler scaling factor for "jw" method (default: 0.1)
#' @param bt Booth matching threshold
#' @param weight Vector of weights for operations: deletion (d), insertion (i),
#'   substitution (s), transposition (t)
#' @param digits Number of digits to round results (default: 3)
#'
#' @return An S3 class object of type "similar_text" containing:
#'   - scores: Numeric similarity scores by method and comparison
#'   - summary: Summary statistics by method and comparison
#'   - methods: Methods used for comparison
#'   - list_names: Names of compared lists
#'
#' @examples
#' list1 <- list("hello", "world")
#' list2 <- list("helo", "word")
#' result <- same_text(list1, list2)
#' @export
same_text <- function(..., method = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
                      q = 1, p = NULL, bt = 0,
                      weight = c(d = 1, i = 1, s = 1, t = 1),
                      digits = 3) {
  valid_methods <- c(
    "osa", "lv", "dl", "hamming", "lcs", "qgram",
    "cosine", "jaccard", "jw", "soundex"
  )

  inputs <- list(...)

  if (length(inputs) < 2) {
    cli::cli_abort("At least two inputs required")
  }

  invalid_inputs <- which(!vapply(inputs, function(x) is_valid_list(x, "text"), logical(1)))

  if (length(invalid_inputs) > 0) {
    cli::cli_abort(c(
      "All inputs must be lists containing only character strings",
      "x" = "Invalid inputs at position(s): {paste(invalid_inputs, collapse = ', ')}"
    ))
  }

  method <- unique(if (length(method) == 1) c(method) else method)

  invalid_methods <- method[!method %in% valid_methods]
  if (length(invalid_methods) > 0) {
    cli::cli_abort(sprintf(
      "Invalid methods for text similarity: %s. Valid methods are: %s.",
      paste(invalid_methods, collapse = ", "),
      paste(valid_methods, collapse = ", ")
    ))
  }

  dots <- as.list(substitute(list(...)))[-1]
  list_names <- if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    names(dots)
  } else {
    purrr::map_chr(dots, ~ deparse(.x)[1])
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

      pair_result <- calculate_text_scores(
        flattened_inputs[[idx1]],
        flattened_inputs[[idx2]],
        method = m,
        q = q,
        p = p,
        bt = bt,
        weight = weight
      )

      mean_score <- round(mean(pair_result), digits)
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
    list_names = list_names,
    digits = digits
  )
}
