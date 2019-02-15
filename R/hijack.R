#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param FUN PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname hijack
#' @export
hijack <- function(fun, ...) {
  .fun <- fun
  args <- list(...)
  invisible(lapply(seq_along(args), function(i) {
    formals(.fun)[[names(args)[i]]] <<- args[[i]]
  }))
  .fun
}
