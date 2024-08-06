
fit_multiple_cfa <- function(models, data, ...) {
  if (!is.character(models)) {
    stop("Models argument should be a character vector of model specifications.")
  }

#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
  fit_model <- function(model) {
    fit <- lavaan::cfa(model, data = data, ...)
    return(fit)
  }

  results <- purrr::map(models, fit_model)
  names(results) <- paste0("Model_", seq_along(models))
  summaries <- purrr::map(results, lavaan::summary)

  return(summaries)
}

