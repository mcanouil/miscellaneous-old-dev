#' pval_trans
#'
#' @return trans
#' @export
pval_trans <- function(alpha = NULL, md = FALSE, prefix = FALSE) {
  scales::trans_new(
    name = "pval",
    domain = c(0, 1),
    transform = function(x) {x[x < .Machine$double.xmin] <- .Machine$double.xmin; -log(x, 10)},
    inverse = function(x) {10^-x},
    breaks = (function(n = 5) {
      function(x) {
        max <- floor(-log(min(c(x, alpha), na.rm = TRUE), base = 10))
        if (max == 0) 1 else sort(unique(c(10^-seq(0, max, by = floor(max / n) + 1), alpha)))
      }
    })(),
    format = (function(x) {
      g <- function(x) {
        gsub(
          "1 %*% ", "",
          gsub(
            "(.*)e([-+]*)0*(.*)", "\\1 %*% 10^\\2\\3",
            gsub(
             "1e+00", "1",
              scales::scientific(x),
             fixed = TRUE
            )),
          fixed = TRUE
        )
      }
      highlight_alpha <- function(x, md = FALSE, prefix = FALSE) {
        if (md & nchar(system.file(package = "ggtext")) != 0) {
          prefix_text <- if (prefix) "&alpha; = " else ""
          out <- paste0(
            "<b style = 'color:firebrick2;'>", 
            gsub("(.*) [%][*][%] .*\\^(.*)", paste(prefix_text, "\\1 &times; 10<sup>\\2</sup>"), g(x)), 
            "</b>"
          )
        } else {
          prefix_text <- if (prefix) "alpha ==" else ""
          out <- parse(text = paste(prefix_text, g(x)))
        }
        
        out
      }
      
      if (!is.null(alpha)) {
        ifelse(
          test = scales::scientific(x) == scales::scientific(alpha), 
          yes = highlight_alpha(x, md, prefix), 
          no = g(x)
        )
      } else {
        parse(text = g(x))
      }
    })
  )
}


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
