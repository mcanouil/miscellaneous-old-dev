#' Title
#'
#' @param stat 
#'
#' @return
#' @export
#'
#' @examples
lambda_correction <- function(stat) {
  gc_corr_factor <- median(stat^2) / qchisq(0.5, 1)
  quantile_stat <- (stat^2) / gc_corr_factor
  return(pchisq(q = quantile_stat, df = 1, lower.tail = FALSE))
}
