
#' Title
#'
#' @param models A vector with multiple models
#' @param data data argument in cfa
#' @param ... other argument in cfa
#'
#' @return summaries as in cfa()
#' @export
#'
#' @examples
#' models <- c(model1, model2, model3)
#' summaries <- fit_multiple_cfa(models, HolzingerSwineford1939)
fit_multiple_cfa <- function(models, data, ...) {
  if (!is.character(models)) {
    stop("Models argument should be a character vector of model specifications.")
  }




  fit_model <- function(model) {
    fit <- lavaan::cfa(model, data = data, ...)
    return(fit)
  }

  results <- purrr::map(models, fit_model)
  names(results) <- paste0("Model_", seq_along(models))
  summaries <- purrr::map(results, lavaan::summary)

  return(summaries)
}

