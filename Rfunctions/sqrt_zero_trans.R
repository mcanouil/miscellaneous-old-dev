sqrt_zero_trans <- function() {
  trans_new(
    name = "sqrt_zero",
    transform = base::sqrt,
    inverse = function(x) ifelse(x==0, 0, x^2),
    domain = c(0, Inf)
  )
}
