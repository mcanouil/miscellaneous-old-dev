pow_trans <- function(power) {
  negpow_breaks <- function(n = 5) {
    function(x, y = power) {
      rng <- range(x, na.rm = TRUE) ^ y
      max <- ceiling(rng[2])
      min <- floor(rng[1])
      by <- floor((max - min) / n) + 1
      seq(min, max, by = by) ^ (1 / y)
    }
  }
  trans_new(
    name = "pow", 
    transform = function(x, y = power) {
      x ^ y
    }, 
    inverse = function(x, y = power) {
      x ^ (1 / y)
    }, 
    breaks = negpow_breaks(), 
    domain = c(-Inf, Inf)
  )
}
