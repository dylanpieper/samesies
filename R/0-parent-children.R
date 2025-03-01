#' Abstract parent class for similarity comparison
#'
#' @description
#' `similar` is an S7 abstract parent class for all similarity comparison objects.
#' This class defines common properties and validation logic shared among child classes
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
#' It also implements common validation logic for ensuring scores are numeric
#' and fall within the valid range of 0 to 1.
#'
#' Child classes should inherit from this class and implement their own
#' specific validation logic for allowed methods and any additional properties.
#'
#' @export
similar <- S7::new_class("similar",
  properties = list(
    scores = S7::class_list,
    summary = S7::class_list,
    methods = S7::class_character,
    list_names = S7::class_character,
    digits = S7::class_numeric
  ),
  validator = function(self) {
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

    if (length(self@list_names) == 0) {
      return("list_names cannot be empty.")
    }

    NULL
  },
  abstract = TRUE
)

#' Text similarity comparison class
#'
#' @description
#' `similar_text` is an S7 class for text similarity comparisons.
#'
#' @param scores List of similarity scores per method and comparison
#' @param summary Summary statistics by method and comparison
#' @param methods Character vector of methods used for comparison
#' @param list_names Character vector of names for the compared lists
#' @param digits Number of digits to round results (default: 3)
#'
#' @details
#' This class extends the abstract `similar` class and implements
#' text-specific similarity comparison methods.
#'
#' @export
similar_text <- S7::new_class("similar_text",
  parent = similar,
  properties = list(),
  validator = function(self) {
    valid_methods <- c(
      "osa", "lv", "dl", "hamming", "lcs", "qgram",
      "cosine", "jaccard", "jw", "soundex"
    )

    invalid_methods <- self@methods[!self@methods %in% valid_methods]
    if (length(invalid_methods) > 0) {
      return(sprintf(
        "Invalid methods for text similarity: %s. Valid methods are: %s.",
        paste(invalid_methods, collapse = ", "),
        paste(valid_methods, collapse = ", ")
      ))
    }

    NULL
  }
)

#' Factor similarity comparison class
#'
#' @description
#' `similar_factor` is an S7 class for categorical/factor similarity comparisons.
#'
#' @param scores List of similarity scores per method and comparison
#' @param summary Summary statistics by method and comparison
#' @param methods Character vector of methods used for comparison
#' @param list_names Character vector of names for the compared lists
#' @param digits Number of digits to round results (default: 3)
#' @param levels Character vector of factor levels
#'
#' @details
#' This class extends the abstract `similar` class and implements
#' categorical data-specific similarity comparison methods.
#'
#' @export
similar_factor <- S7::new_class("similar_factor",
  parent = similar,
  properties = list(
    levels = S7::class_character
  ),
  validator = function(self) {
    valid_methods <- c("exact", "order")

    invalid_methods <- self@methods[!self@methods %in% valid_methods]
    if (length(invalid_methods) > 0) {
      return(sprintf(
        "Invalid methods for factor similarity: %s. Valid methods are: %s.",
        paste(invalid_methods, collapse = ", "),
        paste(valid_methods, collapse = ", ")
      ))
    }

    NULL
  }
)

#' Numeric similarity comparison class
#'
#' @description
#' `similar_number` is an S7 class for numeric similarity comparisons.
#'
#' @param scores List of similarity scores per method and comparison
#' @param summary Summary statistics by method and comparison
#' @param methods Character vector of methods used for comparison
#' @param list_names Character vector of names for the compared lists
#' @param digits Number of digits to round results (default: 3)
#' @param raw_values List of raw numeric values being compared
#'
#' @details
#' This class extends the abstract `similar` class and implements
#' numeric data-specific similarity comparison methods.
#'
#' @export
similar_number <- S7::new_class("similar_number",
  parent = similar,
  properties = list(
    raw_values = S7::class_list
  ),
  validator = function(self) {
    valid_methods <- c("exact", "pct_diff", "normalized", "fuzzy")

    invalid_methods <- self@methods[!self@methods %in% valid_methods]
    if (length(invalid_methods) > 0) {
      return(sprintf(
        "Invalid methods for numeric similarity: %s. Valid methods are: %s.",
        paste(invalid_methods, collapse = ", "),
        paste(valid_methods, collapse = ", ")
      ))
    }

    NULL
  }
)
