#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sem
#' @export 
sem <- function (x, na.rm = TRUE) {
    sqrt(
        (mean(x^2, na.rm = na.rm)-mean(x, na.rm = na.rm)^2) / sum(!is.na(x))
    )
}