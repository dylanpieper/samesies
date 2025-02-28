# samesies <img src="man/figures/samesies-hex.png" align="right" width="140"/>

Samesies is a reliability tool for comparing the similarity of texts, factors, or numbers across two or more lists. The motivating use case is to quantify the reliability of LLM responses across models, providers, or prompts.

## Installation

``` r
devtools::install_github("dylanpieper/samesies")
```

## Interface

Samesies provides three main functions for similarity analysis:

### `same_text()`

Compare similarity between multiple lists of character strings.

``` r
r1 <- list("R is a statistical computing software", 
           "R enables grammar of graphics using ggplot2", 
           "R supports advanced statistical models")
r2 <- list("R is a full-stack programming language",
           "R enables advanced data visualizations", 
           "R supports machine learning algorithms")

tex <- same_text(r1, r2)
```

By default, all methods will be used. Opt-in to specific methods.

```         
tex <- same_text(r1, r2, method = "cosine")
```

Methods available via [stringdist](https://github.com/markvanderloo/stringdist):

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

Methods available:

-   **exact**: Exact matching
-   **order**: Distances across ordered factor levels

### `same_number()`

Compare similarity between multiple lists of numeric values.

``` r
n1 <- list(1, 2, 3)
n2 <- list(1, 2.1, 3.2)

num <- same_number(n1, n2)
```

Methods available:

-   **exact**: Exact matching
-   **percent_diff**: Percentage difference
-   **normalized**: Normalized difference (set `max_diff` or else auto-calculate)

``` r
num <- same_number(n1, n2, 
                   method = "normalized", 
                   max_diff = 2)
```

-   **fuzzy**: Fuzzy matching with tolerance (set `epsilon` or else auto-calculate)

``` r
num <- same_number(n1, n2, 
                   method = "fuzzy", 
                   epsilon = 0.1)
```

## Nested Lists

Nested lists are supported as long as they share the same names and lengths.

## Working with Results

All three functions return `similar` objects that support the following methods:

``` r
print()
summary()
average_similarity()
pair_averages()
```
