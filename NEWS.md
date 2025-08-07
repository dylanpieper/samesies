# samesies (development)

## Lifecycle Changes

* `same_factor()` no longer uses the `ordered` parameter (whether levels are treated as ordered is now determined automatically based on whether the `levels` parameter is provided) and the `levels` parameter is now optional (if not provided, only the "exact" method will be used for comparison).

## Internal Improvements

* Removed unnecessary imports (dplyr, ggbeeswarm, and ggplot2)

# samesies 0.1.0

* Initial CRAN submission
