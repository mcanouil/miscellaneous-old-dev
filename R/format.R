#' format_pval
#'
#' @param x [numeric] A numeric vector.
#' @param thresh [numeric]
#' @param digits [numeric] How many significant digits are to be used.
#' @param eps [numeric] A numerical tolerance.
#' @param math_format [logical]
#' @param na.form [character] Character representation of NAs.
#' @param ... [misc] Further arguments to be passed to [format()] such as nsmall.
#'
#' @return A character vector.
#' @export
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

  if ((has_na <- any(ina <- is.na(x)))) {
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

  if (has_na) {
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


#' capitalise
#'
#' @param x [character]
#'
#' @return character
#' @export
capitalise <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}


#' pretty_kable
#'
#' @param data [data.frame] An R object, typically a matrix or data frame.
#' @param font_size [numeric]
#' @param format_args [list]
#' @param col.names [character] A character vector of column names to be used in the table.
#' @param pval_cols [character]
#' @param full_width [logical]
#' @param echo [logical]
#' @param ... [misc]
#'
#' @return kable
#' @export
pretty_kable <- function (
  data,
  font_size = 12,
  format_args = list(scientific = -1, digits = 3, big.mark = ","),
  col.names = NA,
  pval_cols = NULL,
  full_width,
  echo = TRUE,
  ...
) {

  if (!is.null(pval_cols)) {
    data[, pval_cols] <- format_pval(
      x = data[, pval_cols],
      digits = format_args$digits
    )
  }
  colnames(data) <- capitalise(colnames(data))

  out <- if (knitr::is_latex_output()) {
    options(knitr.table.format = "latex")
    output <- knitr::kable(x = data, booktabs = TRUE, format.args = format_args, col.names = col.names, ...)
    kableExtra::kable_styling(
      kable_input = output,
      latex_options = c("striped", "hold_position"),
      full_width = ifelse(missing(full_width), FALSE, full_width),
      position = "center",
      font_size = font_size
    )
  } else {
    options(knitr.table.format = "html")
    output <- knitr::kable(x = data, format.args = format_args, col.names = col.names, ...)
    kableExtra::kable_styling(
      kable_input = output,
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = ifelse(missing(full_width), TRUE, full_width),
      position = "center",
      font_size = font_size
    )
  }

  if (echo) print(out)

  invisible(out)
}
