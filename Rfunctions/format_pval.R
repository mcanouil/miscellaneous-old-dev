format_pval <- function (x, thresh = 10^-2, digits = 3, eps = 1e-50) {
    ifelse(
        x>=thresh, 
        Hmisc::format.pval(x, digits = digits, eps = eps, nsmall = digits), 
        base::format.pval(x, digits = digits, eps = eps, scientific = TRUE, nsmall = digits)
    )
}
