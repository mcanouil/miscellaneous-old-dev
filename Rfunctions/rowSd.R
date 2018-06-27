#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowSd
#' @export
rowSd <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- rowSums(!is.na(x))
  } else {
    n <- ncol(x)
  }
  rowVar <- rowMeans(x * x, na.rm = na.rm) - (rowMeans(x, na.rm = na.rm))^2
  return(sqrt(rowVar * n / (n - 1)))
}
