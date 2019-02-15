#' se
#'
#' @inheritParams x sd
#' @inheritParams na.rm sd
#'
#' @return
#' @export
se <- function(x, na.rm = TRUE) {
  sqrt(
    (mean(x^2, na.rm = na.rm) - mean(x, na.rm = na.rm)^2) / sum(!is.na(x))
  )
}


#' row_se
#'
#' @inheritParams x rowMeans
#' @inheritParams na.rm rowMeans
#'
#' @return
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
#' @inheritParams x colMeans
#' @inheritParams na.rm colMeans
#'
#' @return
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
#' @inheritParams x rowMeans
#' @inheritParams na.rm rowMeans
#'
#' @return
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
#' @inheritParams x colMeans
#' @inheritParams na.rm colMeans
#'
#' @return
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
