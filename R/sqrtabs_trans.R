sqrtabs_trans <- function() {
  f <- function(x) {sign(x) * sqrt(abs(x))}
  g <- function(x) {sign(x) * x^2}
  h <- function (n = 5) {
    function (x) {
      rng <- f(range(x, na.rm = TRUE))
      max <- ceiling(rng[2])
      min <- floor(rng[1])
      by <- floor((max - min)/n)
      g(seq(min, max, by = by))
    }
  }
  scales::trans_new(
    name = "sqrtabs", 
    transform = f, 
    inverse = g, 
    breaks = h(), 
    domain = c(-Inf, Inf)
  )
}
