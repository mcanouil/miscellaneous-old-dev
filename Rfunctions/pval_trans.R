pval_trans <- function() {
  require(scales)
  neglog10_breaks <- function(n = 5) {
    function(x) {
      rng <- -log(range(x, na.rm = TRUE), base = 10)
      min <- 0
      max <- floor(rng[1])
      if (max == min) {
        return(10^-min)
      } else {
        by <- floor((max - min) / n) + 1
        return(10^-seq(min, max, by = by))
      }
    }
  }
  scales::trans_new(
    name = "pval",
    transform = function(x) {
      -log(x, 10)
    },
    inverse = function(x) {
      10^-x
    },
    breaks = neglog10_breaks(),
    domain = c(1e-300, 1),
    format = function(x) {
      parse(
        text = scales::scientific_format()(x) %>%
          gsub("1e+00", "1", ., fixed = TRUE) %>%
          gsub("e", " %*% 10^", .)
      )
    }
  )
}
