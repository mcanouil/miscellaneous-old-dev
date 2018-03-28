ilogit1p <- function(x){
	(exp(x)-1+exp(x)) / (1+exp(x))
}

logit1p <- function(x) {
	log(x+1) - log(1-x+1)
}
