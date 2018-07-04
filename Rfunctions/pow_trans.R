pow_trans <- function(power, n = 5) {
    negpow_breaks <- function(n = 5) {
        function(x, y = power) {
            rng <- range(x, na.rm = TRUE) ^ y
            max <- signif(rng[2], digits = 3)
            min <- signif(rng[1], digits = 3)
            signif(seq(min, max, length.out = n) ^ (1 / y), digits = 3)
        }
    }
    
    trans_new(
        name = "pow", 
        transform = function(x, y = power) {
            ifelse(x<=0, 0, x ^ y)
        }, 
        inverse = function(x, y = power) {
            ifelse(x<=0, 0, x ^ (1 / y))
        }, 
        breaks = negpow_breaks(n = n), 
        domain = c(0, Inf)#,
        # format = function(x) {
        #     parse(text = gsub("e", " %*% 10^", scientific(x)))
        # }
    )
}
