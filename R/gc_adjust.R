#' gc_adjust
#'
#' @param stat [numeric]
#'
#' @return numeric
#' @export
gc_adjust <- function(stat) {
  gc_corr_factor <- stats::median(stat^2) / stats::qchisq(0.5, 1)
  quantile_stat <- (stat^2) / gc_corr_factor
  stats::pchisq(q = quantile_stat, df = 1, lower.tail = FALSE)
}
