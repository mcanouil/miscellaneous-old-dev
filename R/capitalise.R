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
#' @rdname capitalise
#' @export 
capitalise <- function (x) {
  s <- strsplit(x, " ")[[1]]
  return(paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " "))
}
