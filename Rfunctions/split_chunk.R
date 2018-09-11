split_chunk <- function(x, n) {
  split(
    x = x, 
    f = cut_interval(x = seq_along(x), n = n)
  )
}
