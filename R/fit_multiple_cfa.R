
#' Fit a list of CFA models
#'
#' @param models A vector with multiple models
#' @param data data argument in cfa
#' @param ... other argument in cfa
#'
#' @return summaries as in cfa()
#' @export
#'
#' @examples
#' model1 <- 'A =~ x1 + x2 + x3'
#' model2 <- 'B =~ x4 + x5 + x6'
#' model3 <- 'C =~ x7 + x8 + x9'
#' models <- c(model1, model2, model3)
#' data("HolzingerSwineford1939", package = "lavaan")
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

