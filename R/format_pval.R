format_pval <- function(
  x,
  thresh = 10^-2,
  math_format = FALSE,
  digits = max(1L, getOption("digits") - 2L),
  eps = .Machine$double.eps,
  na.form = "NA",
  ...
) {

  format_thresh <- function(x, thresh, digits, ...) {
    if (x>=thresh) {
      format(round(x, digits = digits), ...)
    } else {
      format(x, digits = digits, scientific = TRUE, ...)
    }
  }

  if ((has.na <- any(ina <- is.na(x)))) {
    x <- x[!ina]
  }
  r <- character(length(is0 <- x < eps))
  if (any(!is0)) {
    rr <- x <- x[!is0]
    expo <- floor(log10(ifelse(x > 0, x, 1e-50)))
    fixp <- expo >= -3 | (expo == -4 & digits > 1)
    if (any(fixp)) {
      rr[fixp] <- format_thresh(
        x = x[fixp], 
        thresh = thresh, 
        digits = digits, 
        ...
      )
    }
    if (any(!fixp)) {
      rr[!fixp] <- format_thresh(
        x = x[!fixp], 
        thresh = thresh, 
        digits = digits, 
        ...
      )
    }
    r[!is0] <- rr
  }

  if (any(is0)) {
    digits <- max(1L, digits - 2L)
    if (any(!is0)) {
      nc <- max(nchar(rr, type = "w"))
      if (digits > 1L && digits + 6L > nc) {
        digits <- max(1L, nc - 7L)
      }
      sep <- if (digits == 1L && nc <= 6L) "" else " "
    } else {
      sep <- if (digits == 1) "" else " "
    }
    r[is0] <- paste("<", format(eps, digits = digits, ...), sep = sep)
  }

  if (has.na) {
    rok <- r
    r <- character(length(ina))
    r[!ina] <- rok
    r[ina] <- na.form
  }

  if (math_format) {
    r <- gsub("e", " %*% 10^", r)
  }
  
  r
}
