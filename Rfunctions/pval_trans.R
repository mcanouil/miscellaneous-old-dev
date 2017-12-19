pval_trans <- function () {
    neglog10_breaks <- function (n = 5) {
        function (x) {
            rng <- -log(range(x, na.rm = TRUE), base = 10)
            min <- ceiling(rng[2])
            max <- floor(rng[1])
            if (max == min) {
                return(10^-min)
            } else {}
            by <- floor((max - min)/n) + 1
            10^-seq(min, max, by = by)
        }
    }
    trans_new(name = "pval", transform = function (x) {-log(x, 10)}, inverse = function (x) {10^-x}, breaks = neglog10_breaks(), domain = c(1e-100, Inf))
}