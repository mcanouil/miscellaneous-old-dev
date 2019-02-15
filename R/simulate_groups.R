#---------------------------------------------------------------------------------
# Name - simulate_groups.R
# Desc - Produces repeated mesures for different individuals in different groups
# Author - Mickaël Canouil
#---------------------------------------------------------------------------------

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param n_groups PARAM_DESCRIPTION, Default: 2
#' @param n_measures PARAM_DESCRIPTION, Default: 5
#' @param prop.x PARAM_DESCRIPTION, Default: 0.5
#' @param par_groups PARAM_DESCRIPTION, Default: list(c(n_samples = 100, intercept = 1, beta.x = 3, sd = 2, 
#'    se = 1.25, beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0), 
#'    c(n_samples = 100, intercept = 11, beta.x = 3, sd = 2, 
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
#' @rdname simulate_groups
#' @export 
simulate_groups <- function(
  n_groups = 2,
  n_measures = 5,
  prop.x = 0.5,
  par_groups = list(
    c(
      n_samples = 100, intercept = 1, beta.x = 3, sd = 2, se = 1.25, 
      beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0
    ),
    c(
      n_samples = 100, intercept = 11, beta.x = 3, sd = 2, se = 1.25, 
      beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0
    )
  )
) {

  traj_poly <- function(x, intercept, parPoly) {
    beta <- matrix(
      data = c(
        intercept, 
        parPoly["beta1"], 
        parPoly["beta2"], 
        parPoly["beta3"], 
        parPoly["beta4"], 
        parPoly["beta5"]
      ), 
      ncol = 1
    )
    traj <- matrix(
      data = c(1, x, x^2, x^3, x^4, x^5), 
      ncol = 6, 
      nrow = 1, 
      byrow = FALSE
    )
    traj %*% beta
  }

  do.call("rbind", lapply(X = seq_len(n_groups), FUN = function(igroup) {
    measures <- NULL
    measures$id <- rep(seq(par_groups[[igroup]]["n_samples"]), each = n_measures)
    measures <- as.data.frame(measures)
    colnames(measures) <- "id"

    samples <- NULL
    samples$x <- rbinom(par_groups[[igroup]]["n_samples"], 1, prop.x)
    samples <- as.data.frame(samples)
    colnames(samples) <- "x"
    samples$id <- seq(par_groups[[igroup]]["n_samples"])
    samples$beta_x <- samples$x * par_groups[[igroup]]["beta.x"]
    samples$intercept <- par_groups[[igroup]]["intercept"]
    samples$group <- igroup
    samples$sample_sd <- rnorm(
      n = length(samples$id), 
      mean = 0, 
      sd = par_groups[[igroup]]["sd"]
    )
    samples$intercept_i <- samples$intercept + samples$sample_sd
    samples$mu <- samples$intercept + samples$beta_x

    data <- merge(measures, samples)
    data$measure_sd <- rnorm(
      n = par_groups[[igroup]]["n_samples"] * n_measures, 
      mean = 0, 
      sd = par_groups[[igroup]]["se"]
    )
    
    data$mu <- data$intercept + data$beta_x
    data$mu_i <- apply(
      X = matrix(seq(n_measures), ncol = 1),
      MARGIN = 1,
      FUN = traj_poly,
      intercept = par_groups[[igroup]]["intercept"],
      parPoly = par_groups[[igroup]][paste0("beta", 1:5)]
    )
    data$mu_i <- data$mu_i + data$sample_sd
    data$y <- data$mu_i + data$measure_sd + data$beta_x
    data$time <- seq_len(n_measures)
    
    data
  }))
}


### Run test
# data <- simulate_groups(
    # n_groups = 3,
    # n_measures = 6,
    # prop.x = 0.65,
    # par_groups = list(
        # c(n_samples = 200, intercept = 25, beta.x = 10, sd = 3, se = 0.5, beta1 = 0.15, beta2 = 0.05, beta3 = 0, beta4 = 0, beta5 = 0),
        # c(n_samples = 300, intercept = 95, beta.x = 10, sd = 3, se = 0.5, beta1 = 0.15, beta2 = 0.05, beta3 = 0, beta4 = 0, beta5 = 0),
        # c(n_samples = 100, intercept = 66, beta.x = 10, sd = 3, se = 0.5, beta1 = 0.15, beta2 = 0.05, beta3 = 0, beta4 = 0, beta5 = 0)
    # )
# )

# prop.table(table(data[, "group"], data[, "x"]), 1)

# library(lme4)
# mod1 <- lmer(y ~ x*time + (1|id), data)
# summary(mod1)

# data[, "time2"] <- data[, "time"]^2
# mod2 <- lmer(y ~ x + time + time2 + group + (1|id), data)
# summary(mod2)
