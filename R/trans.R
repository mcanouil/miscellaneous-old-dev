#' pow_trans
#'
#' @param power [numeric]
#'
#' @return trans
#' @export
pow_trans <- function(power) {
  negpow_breaks <- function(n = 5) {
    function(x, y = power) {
      rng <- range(x, na.rm = TRUE)^y
      max <- signif(rng[2], digits = 3)
      min <- signif(rng[1], digits = 3)
      signif(seq(min, max, length.out = n)^(1 / y), digits = 3)
    }
  }
  scales::trans_new(
    name = "pow",
    transform = function(x, y = power) {
      ifelse(x <= 0, 0, x^y)
    },
    inverse = function(x, y = power) {
      ifelse(x <= 0, 0, x^(1 / y))
    },
    breaks = negpow_breaks(),
    domain = c(0, Inf)
  )
}


#' pval_trans
#'
#' @return trans
#' @export
pval_trans <- function() {
  neglog10_breaks <- function(n = 5) {
    function(x) {
      rng <- -log(range(x, na.rm = TRUE), base = 10)
      min <- 0
      max <- floor(rng[1])
      if (max == min) {
        10^-min
      } else {
        by <- floor((max - min) / n) + 1
        10^-seq(min, max, by = by)
      }
    }
  }
  scales::trans_new(
    name = "pval",
    transform = function(x) {-log(x, 10)},
    inverse = function(x) {10^-x},
    breaks = neglog10_breaks(),
    format = function(x) {
      parse(
        text = gsub("e", " %*% 10^", gsub("1e+00", "1", scales::scientific_format()(x), fixed = TRUE))
      )
    },
    domain = c(0, 1)
  )
}


#' sqrtabs_trans
#'
#' @return trans
#' @export
sqrtabs_trans <- function() {
  f <- function(x) {sign(x) * sqrt(abs(x))}
  g <- function(x) {sign(x) * x^2}
  sqrtabs_breaks <- function (n = 5) {
    function (x) {
      rng <- f(range(x, na.rm = TRUE))
      max <- ceiling(rng[2])
      min <- floor(rng[1])
      by <- (max - min) / n
      by <- if (by>1) floor(by) else by
      sort(unique(c(0, g(seq(min, max, by = by)))))
    }
  }
  scales::trans_new(
    name = "sqrtabs",
    transform = f,
    inverse = g,
    breaks = sqrtabs_breaks(),
    domain = c(-Inf, Inf)
  )
}
