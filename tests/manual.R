# Simple cases ------------------------------------------------------------

# Case 1: Unstructured text strings
fruits1 <- list("apple", "banana", "orange")
fruits2 <- list("appel", "banana", "orange")
fruits3 <- list("appels", "bananas", "oranges")
result_text <- same_text(fruits1, fruits2)

# Test the three core utility functions
print(result_text)
summary(result_text)
average_similarity(result_text)
pair_averages(result_text)

# Test method filtering
pair_averages(result_text, method = "jw")

# Case 2: Factors / structured text strings
fruits1 <- list("apple", "orange", "unknown")
fruits2 <- list("apple", "orange", "unknown")
fruits3 <- list("apple", "pineapple", "banana")
result_factor <- same_factor(fruits1, fruits2, fruits3, levels = c("apple", "orange", "banana"))

# Test the factor-specific functions
print(result_factor)
summary(result_factor)
average_similarity(result_factor)
pair_averages(result_factor)

# Case 3: Numbers
nums1 <- list(0.95, 1, 0.60)
nums2 <- list(1, 1, 0.65)
nums3 <- list(0.90, 1, 0.55)
result_num <- same_number(nums1, nums2, nums3)

# Test number-specific functions
print(result_num)
summary(result_num)
average_similarity(result_num)
pair_averages(result_num)

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
result_nested_text <- same_text(nested_fruits1, nested_fruits2)

# Test with nested structures
print(result_nested_text)
average_similarity(result_nested_text)
pair_averages(result_nested_text)

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
