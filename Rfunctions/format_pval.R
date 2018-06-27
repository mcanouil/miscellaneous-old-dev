format_pval <- function(x, thresh = 10^-2, digits = 3, eps = 1e-50, math_format = FALSE) {
  pout <- ifelse(
    x >= thresh,
    Hmisc::format.pval(x, digits = digits, eps = eps, nsmall = digits),
    base::format.pval(x, digits = digits, eps = eps, scientific = TRUE, nsmall = digits)
  )
  if (math_format) {
    return(gsub("e", " %*% 10^", pout))
  } else {
    return(pout)
  }
}
