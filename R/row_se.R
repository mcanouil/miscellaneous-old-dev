#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' @rdname row_se
#' @export 
row_se <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- rowSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  rowVar <- rowMeans(x * x, na.rm = na.rm) - (rowMeans(x, na.rm = na.rm))^2
  sqrt(rowVar / n)
}
