# Simple cases ------------------------------------------------------------

# Case 1: Unstructured text strings
fruits1 <- list("apple", "banana", "orange")
fruits2 <- list("appel", "banana", "orange")
fruits3 <- list("appels", "bananas", "oranges")
result1 <- same_text(fruits1, fruits2, fruits3, method = c("jw", "lv"))
result2 <- same_text(fruits1, fruits2)

# Test the three core utility functions
print(result1)
average_similarity(result1)
pair_averages(result1)
plot_methods(result1)

# Test different plot types
plot_methods(result1, type = "boxplot")
plot_methods(result1, type = "violin")
plot_methods(result1, type = "point")

# Test method filtering
pair_averages(result1, method = "jw")

# Case 2: Factors / structured text strings
fruits1 <- list("apple", "orange", "unknown")
fruits2 <- list("apple", "orange", "unknown")
fruits3 <- list("apple", "pineapple", "banana")
result_factor <- same_factor(fruits1, fruits2, fruits3, levels = c("apple", "orange", "banana"))

# Test the factor-specific functions
print(result_factor)
average_similarity(result_factor)
pair_averages(result_factor)
plot_methods(result_factor)

# Test factor-specific plot types
plot_methods(result_factor, type = "heatmap")
plot_methods(result_factor, type = "boxplot")

# Test multiple methods for factors
result_factor2 <- same_factor(fruits1, fruits2,
  method = c("exact", "jaccard"),
  levels = c("apple", "orange", "banana")
)
print(result_factor2)

# Case 3: Numbers
nums1 <- list(0.95, 1, 0.60)
nums2 <- list(1, 1, 0.65)
nums3 <- list(0.90, 1, 0.55)
result_num <- same_number(nums1, nums2, nums3)

# Test number-specific functions
print(result_num)
average_similarity(result_num)
pair_averages(result_num)
plot_methods(result_num)

# Test number-specific plot types
plot_methods(result_num, type = "histogram")
plot_methods(result_num, type = "boxplot")

# Test multiple methods for numbers
result_num2 <- same_number(nums1, nums2, method = c("normalized", "fuzzy", "percent_diff"))
print(result_num2)
plot_methods(result_num2)

# Nested cases ------------------------------------------------------------

# Nested Case 1: Unstructured text strings
nested_fruits1 <- list(
  citrus = list("orange", "lemon", "lime"),
  berries = list("strawberry", "blueberry", "raspberry")
)
nested_fruits2 <- list(
  citrus = list("ornge", "lemmon", "lime"),
  berries = list("strawbery", "blueberry", "respberry")
)
nested_fruits3 <- list(
  citrus = list("oranges", "lemons", "limes"),
  berries = list("strawberries", "blueberries", "raspberries")
)
result3 <- same_text(nested_fruits1, nested_fruits2, method = "jw")
result4 <- same_text(nested_fruits1, nested_fruits2, nested_fruits3, method = c("jw", "lv"))

# Test with nested structures
print(result3)
print(result4)
average_similarity(result4)
pair_averages(result4)
plot_methods(result4)

# Test palette options
plot_methods(result4, palette = "Set1")
plot_methods(result4, palette = "Dark2")

# Nested Case 2: Factors with nested structure
nested_cats1 <- list(
  fruits = list("apple", "orange", "banana"),
  colors = list("red", "blue", "green")
)
nested_cats2 <- list(
  fruits = list("apple", "grape", "banana"),
  colors = list("red", "blue", "yellow")
)
result_nested_factor <- same_factor(nested_cats1, nested_cats2,
  levels = c(
    "apple", "orange", "banana", "grape",
    "red", "blue", "green", "yellow"
  )
)
print(result_nested_factor)
plot_methods(result_nested_factor, type = "heatmap")

# Nested Case 3: Numbers with nested structure
nested_nums1 <- list(
  weights = list(1.2, 2.5, 3.7),
  heights = list(160, 170, 180)
)
nested_nums2 <- list(
  weights = list(1.25, 2.45, 3.65),
  heights = list(162, 172, 178)
)
result_nested_num <- same_number(nested_nums1, nested_nums2)
print(result_nested_num)
plot_methods(result_nested_num, type = "histogram")
