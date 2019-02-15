#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname col_sd
#' @export 
col_sd <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x * x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
  sqrt(colVar * n / (n - 1))
}
