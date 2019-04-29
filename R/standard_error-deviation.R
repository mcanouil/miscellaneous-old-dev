#' se
#'
#' @param x [numeric] A numeric vector or an R object which is coercible to one by as.double(x).
#' @param na.rm [logical] Should missing values be removed?
#'
#' @return numeric
#' @export
se <- function(x, na.rm = TRUE) {
  sqrt(
    (mean(x^2, na.rm = na.rm) - mean(x, na.rm = na.rm)^2) / sum(!is.na(x))
  )
}


#' row_se
#'
#' @inheritParams se
#'
#' @return numeric
#' @export
row_se <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- rowSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  rowVar <- rowMeans(x * x, na.rm = na.rm) - (rowMeans(x, na.rm = na.rm))^2
  sqrt(rowVar / n)
}

#' col_se
#'
#' @inheritParams se
#'
#' @return numeric
#' @export
col_se <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x * x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
  sqrt(colVar / n)
}

#' row_sd
#'
#' @inheritParams se
#'
#' @return numeric
#' @export
row_sd <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- rowSums(!is.na(x))
  } else {
    n <- ncol(x)
  }
  rowVar <- rowMeans(x * x, na.rm = na.rm) - (rowMeans(x, na.rm = na.rm))^2
  sqrt(rowVar * n / (n - 1))
}

#' col_sd
#'
#' @inheritParams se
#'
#' @return numeric
#' @export
col_sd <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x * x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
  sqrt(colVar * n / (n - 1))
}
