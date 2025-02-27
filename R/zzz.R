# This empty method prevents documentation error
#' @export
summary.similar_number <- function(object, ...) {
  overall_avgs <- average_similarity(object)
  pair_avgs <- pair_averages(object)

  result <- list(
    methods = object@methods,
    list_names = object@list_names,
    overall_averages = overall_avgs,
    pair_averages = pair_avgs
  )

  class(result) <- "summary.similar_number"
  return(result)
}