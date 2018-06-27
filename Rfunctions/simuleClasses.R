#---------------------------------------------------------------------------------
# Name - simuleClasses.R
# Desc - Produces repeated mesures for different individuals in different groups
# Author - Mickael Canouil
# Source code - https://github.com/mcanouil/DEV/simuleClasses.R
#---------------------------------------------------------------------------------


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nbClasses PARAM_DESCRIPTION, Default: 2
#' @param nbMesures PARAM_DESCRIPTION, Default: 5
#' @param prop.x PARAM_DESCRIPTION, Default: 0.5
#' @param parClasses PARAM_DESCRIPTION, Default: list(c(nbIndividus = 100, intercept = 1, beta.x = 3, sd = 2,
#'    se = 1.25, beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0),
#'    c(nbIndividus = 100, intercept = 11, beta.x = 3, sd = 2,
#'        se = 1.25, beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0,
#'        beta5 = 0))
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname simuleClasses
#' @export
simuleClasses <- function(
                          nbClasses = 2,
                          nbMesures = 5,
                          prop.x = 0.5,
                          parClasses = list(
                            c(nbIndividus = 100, intercept = 1, beta.x = 3, sd = 2, se = 1.25, beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0),
                            c(nbIndividus = 100, intercept = 11, beta.x = 3, sd = 2, se = 1.25, beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0)
                          )) {
  trajPoly <- function(x, intercept, parPoly) {
    beta <- matrix(c(intercept, parPoly["beta1"], parPoly["beta2"], parPoly["beta3"], parPoly["beta4"], parPoly["beta5"]), ncol = 1)
    traj <- matrix(c(1, x, x^2, x^3, x^4, x^5), ncol = 6, nrow = 1, byrow = FALSE)
    traj %*% beta
  }

  # plotLong <- function (X, Y, data, id, ...) {
  # temp <- data[!is.na(data[Y]), c(id, X, Y)]
  # temp <- temp[order(temp[X]), ]
  # tempWide <- reshape(temp, timevar = X, idvar = id, direction = "wide")
  # eval(parse(text = paste0(X, " <- unique(temp['", X, "'])")))
  # eval(parse(text = paste0(Y, " <- as.data.frame(t(tempWide)[-1, ])")))
  # eval(parse(text = paste0("matplot(", X, ", ", Y, ", ...)")))
  # }

  resultData <- NULL
  nbIndividusTotal <- 0
  for (classe in seq(nbClasses)) {
    dataMesures <- NULL
    dataMesures$id <- rep(seq(parClasses[[classe]]["nbIndividus"]), each = nbMesures)
    dataMesures <- as.data.frame(dataMesures)
    colnames(dataMesures) <- "id"

    dataIndividus <- NULL
    dataIndividus$x <- rbinom(parClasses[[classe]]["nbIndividus"], 1, prop.x)
    dataIndividus <- as.data.frame(dataIndividus)
    colnames(dataIndividus) <- "x"
    dataIndividus$id <- seq(parClasses[[classe]]["nbIndividus"])
    dataIndividus$x.beta <- dataIndividus$x * parClasses[[classe]]["beta.x"]
    dataIndividus$intercept <- parClasses[[classe]]["intercept"]
    dataIndividus$classe <- classe
    dataIndividus$ecart.individu <- rnorm(length(dataIndividus$id), mean = 0, sd = parClasses[[classe]]["sd"])
    dataIndividus$intercept.i <- dataIndividus$intercept + dataIndividus$ecart.individu
    dataIndividus$mu <- dataIndividus$intercept + dataIndividus$x.beta

    data <- merge(dataMesures, dataIndividus)
    data$ecart.mesure <- rnorm(parClasses[[classe]]["nbIndividus"] * nbMesures, mean = 0, sd = parClasses[[classe]]["se"])
    data$mu <- data$intercept + data$x.beta
    data$mu.i <- apply(
      matrix(seq(nbMesures), ncol = 1),
      1,
      trajPoly,
      intercept = parClasses[[classe]]["intercept"],
      parPoly = parClasses[[classe]][paste0("beta", 1:5)]
    ) + data$ecart.individu
    data$y <- data$mu.i + data$ecart.mesure + data$x.beta
    data$time <- seq(nbMesures)
    nbIndividusTotal <- nbIndividusTotal + parClasses[[classe]]["nbIndividus"]
    resultData <- rbind(resultData, data) # [, c("id", "time", "x", "y", "classe")])
  }

  # resultData$id <- as.factor(rep(seq(nbIndividusTotal), each = nbMesures))
  # resultData$x <- as.factor(resultData$x)
  # resultData$classe <- as.factor(resultData$classe)
  # plotLong(X = "time", Y = "y", id = "id", data = resultData, type = "l", lty = 1, main = paste(nbClasses, "Classes", sep = " "))
  return(resultData)
}


### Run test
# data <- simuleClasses(
# nbClasses = 3,
# nbMesures = 6,
# prop.x = 0.65,
# parClasses = list(
# c(nbIndividus = 200, intercept = 25, beta.x = 10, sd = 3, se = 0.5, beta1 = 0.15, beta2 = 0.05, beta3 = 0, beta4 = 0, beta5 = 0),
# c(nbIndividus = 300, intercept = 95, beta.x = 10, sd = 3, se = 0.5, beta1 = 0.15, beta2 = 0.05, beta3 = 0, beta4 = 0, beta5 = 0),
# c(nbIndividus = 100, intercept = 66, beta.x = 10, sd = 3, se = 0.5, beta1 = 0.15, beta2 = 0.05, beta3 = 0, beta4 = 0, beta5 = 0)
# )
# )

# prop.table(table(data[, "classe"], data[, "x"]), 1)

# library(lme4)
# mod1 <- lmer(y ~ x*time + (1|id), data)
# summary(mod1)

# data[, "time2"] <- data[, "time"]^2
# mod2 <- lmer(y ~ x + time + time2 + classe + (1|id), data)
# summary(mod2)
