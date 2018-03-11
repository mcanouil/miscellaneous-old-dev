pow_trans <- function () {
    negpow_breaks <- function (n = 5) {
        function (x, y = 0.5) {
            rng <- range(x, na.rm = TRUE)^y
            min <- ceiling(rng[2])
            max <- floor(rng[1])
            by <- floor((max - min)/n) + 1
            seq(min, max, by = by)^(1/y)
        }
    }
    trans_new(name = "pow", transform = function (x, y = 0.5) {x^y}, inverse = function (x, y = 0.5) {x^(1/y)}, breaks = negpow_breaks(), domain = c(0, Inf))
}