# samesies ![](man/figures/samesies-hex.png){align="right" width="140"}

[![CRAN status](https://www.r-pkg.org/badges/version/samesies)](https://cran.r-pkg.org/package=samesies) [![R-CMD-check](https://github.com/dylanpieper/samesies/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dylanpieper/samesies/actions/workflows/R-CMD-check.yaml)

Compare lists of texts, factors, or numerical values to measure their similarity. The motivating use case is evaluating the similarity of large language model (LLM) responses across models, providers, or prompts.

## Installation

You can install `samesies` from CRAN with:

``` r
install.packages("samesies")
```

## Basic Usage

`samesies` provides three main functions for measuring similarity:

### `same_text()`

Compare similarity between multiple lists of character strings.

``` r
library(samesies)

r1 <- list("R is a statistical computing software", 
           "R enables grammar of graphics using ggplot2", 
           "R supports advanced statistical models")
r2 <- list("R is a full-stack programming language",
           "R enables advanced data visualizations", 
           "R supports machine learning algorithms")

tex <- same_text(r1, r2)
```

Methods available via [stringdist](https://github.com/markvanderloo/stringdist) (e.g., `method = "osa"`):

-   Transformational Algorithms

    -   **osa**: Adjacent transposition edits
    -   **lv**: Basic edit distance
    -   **dl**: Comprehensive edit distance with transpositions

-   Structural Comparison

    -   **hamming**: Position-wise character differences
    -   **lcs**: Longest shared subsequence
    -   **qgram**: Subsequence matching
    -   **cosine**: Vector-space string similarity
    -   **jaccard**: Set-based string comparison

-   Linguistic Matching

    -   **jw**: Prefix-weighted string matching
    -   **soundex**: Phonetic encoding

### `same_factor()`

Compare similarity between multiple lists of categorical data.

``` r
cats1 <- list("R", "R", "Python")
cats2 <- list("R", "Python", "R")

fct <- same_factor(cats1, cats2, 
                   levels = c("R", "Python"))
```

Methods available (e.g., `method = "exact"`):

-   **exact**: Exact matching
-   **order**: Distances across ordered factor levels

### `same_number()`

Compare similarity between multiple lists of numeric values.

``` r
n1 <- list(1, 2, 3)
n2 <- list(1, 2.1, 3.2)

num <- same_number(n1, n2)
```

Methods available (e.g., `method = "exact"`):

-   **exact**: Exact matching
-   **raw**: Absolute difference
-   **exp**: Exponential decay on the absolute difference
-   **pct_diff**: Percentage difference
-   **normalized**: Normalized difference (`max_diff` is computed automatically by default)

``` r
num <- same_number(n1, n2, 
                   method = "normalized", 
                   max_diff = 2.2)
```

-   **fuzzy**: Fuzzy matching with dual tolerance system:
    -   Uses both relative and absolute tolerance thresholds
    -   The fuzzy matching method calculates numeric similarity by using both a relative (default 2%) and absolute (default 0.05) tolerance
    -   Values within the maximum of these two epsilon thresholds are considered exact matches (score of 1)
    -   Similarity scores gradually decrease as the difference grows beyond the threshold

``` r
num <- same_number(n1, n2, 
                   method = "fuzzy", 
                   epsilon = 0.05,
                   epsilon_pct = 0.02)
```

## More Lists

When you input more than two lists, compute pairwise comparisons across lists.

## Nested Lists

Nested lists are supported as long as they share the same names and lengths.

## Methods

All three functions return `similar` objects that support the following methods:

-   `print(x)`
-   `summary(x)`
-   `average_similarity(x, method = NULL)`
-   `pair_averages(x, method = NULL)`

## Accessing Object Data

The package uses S3 objects, allowing access to the underlying data:

-   `$scores`: A list of similarity scores for each method and comparison pair
-   `$summary`: A list of statistical summaries for each method and comparison pair
-   `$methods`: The similarity methods used in the analysis
-   `$list_names`: Names of the input lists
-   `$raw_values`: The original input values
-   `$digits`: Number of decimal places for rounding results in output

## Credits

The Spiderman image in the hex logo is fan art created by the Reddit user [WistlerR15](https://www.reddit.com/r/Spiderman/comments/k3pcj3/remade_the_spiderman_meme_with_my_favorite/).
