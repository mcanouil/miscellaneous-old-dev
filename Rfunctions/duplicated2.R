#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname duplicated2
#' @export
duplicated2 <- function(x) {
  if (sum(dup <- duplicated(x)) == 0) {
    return(dup)
  }
  if (class(x) %in% c("data.frame", "matrix")) {
    return(duplicated(rbind(x[dup, ], x))[-(1:sum(dup))])
  } else {
    return(duplicated(c(x[dup], x))[-(1:sum(dup))])
  }
}
