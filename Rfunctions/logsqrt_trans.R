logsqrt_trans <- function () {
    f <- function (x) {log(sqrt(x+1), 10)}
    g <- function (x) {((10^(x))^2)-1}
    neglogsqrt_breaks <- function (n = 5) {
        function (x) {
            rng <- log(sqrt(range(x+1, na.rm = TRUE)), base = 10)
            max <- ceiling(rng[2])
            min <- floor(rng[1])
            if (max == min) {
                return(g(min)+1)
            } else {}
            by <- floor((max - min)/n) + 1
            return(g(seq(min, max, by = by))+1)
        }
    }
    trans_new(name = "logsqrt", transform = f, inverse = g, breaks = neglogsqrt_breaks(), domain = c(0, Inf))
}