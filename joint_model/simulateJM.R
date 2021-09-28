# See jointModelSimulation.R for "eventTime", "simulateJM" and "plotJM"

SJM <- compiler::cmpfun(function(settings) {
  smldta <- simulateJM(
    n = settings$n,
    times = settings$times,
    thetas = settings$thetas,
    gamma = settings$gamma,
    maf = settings$maf,
    Epsilon_thetas = settings$Epsilon_thetas,
    sigma = settings$sigma,
    beta = settings$beta,
    Epsilon_beta = settings$Epsilon_beta,
    alpha = settings$alpha,
    nu = settings$nu,
    lambda = settings$lambda,
    cens = settings$cens,
    seed = settings$seed
  )
  smldtaclean <- smldta[smldta[, "ID"] %in% subset(smldta, get("Time") == 0)[, "ID"] & !is.na(smldta[, "Yobs"]), ]

  lmeFit <- lme(settings$Yfixed, random = settings$Yrandom, data = smldtaclean, control = lmeControl(opt = "optim"))
  coxFit <- coxph(settings$Ysurvival, data = subset(smldtaclean, get("Time") == 0), x = TRUE)
  jointFit <- jointModel(lmeObject = lmeFit, survObject = coxFit, timeVar = "Time", method = "piecewise-PH-aGH", scaleWB = settings$nu)
  sjmf <- summary(jointFit)
  obs <- rbind(
    sjmf$`CoefTable-Long`[, c("Value", "Std.Err")],
    sjmf$`CoefTable-Event`[, c("Value", "Std.Err")]
  )[c(1, 2, 3, 4, 5), ]
  th <- c(
    settings$thetas,
    settings$gamma,
    settings$alpha,
    settings$beta,
    log(settings$nu)
  )[c(1, 2, 3, 5, 6)]
  out <- as.data.frame(cbind(obs, th))
  dimnames(out) <- list(c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)")[c(1, 2, 3, 5, 6)], c("Obs.mean", "Obs.se", "Th"))
  out <- out[c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), ]
  out[, "RE.mean"] <- (out[, "Obs.mean"] - out[, "Th"]) / out[, "Th"]
  out[, "RE.lower"] <- ((out[, "Obs.mean"] - out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "RE.upper"] <- ((out[, "Obs.mean"] + out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "p"] <- rownames(out)

  out <- unlist(out[, c("RE.mean", "RE.lower", "RE.upper")], use.names = FALSE)
  names(out) <- apply(expand.grid(c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), c(".mean", ".lower", ".upper")), 1, paste0, collapse = "")
  return(out)
})

SJM2 <- compiler::cmpfun(function(settings, data) {
  smldtaclean <- data[data[, "ID"] %in% subset(data, get("Time") == 0)[, "ID"] & !is.na(data[, "Yobs"]), ]

  lmeFit <- lme(settings$Yfixed, random = settings$Yrandom, data = smldtaclean, control = lmeControl(opt = "optim"))
  coxFit <- coxph(settings$Ysurvival, data = subset(smldtaclean, get("Time") == 0), x = TRUE)
  jointFit <- jointModel(lmeObject = lmeFit, survObject = coxFit, timeVar = "Time", method = "piecewise-PH-aGH", scaleWB = settings$nu)
  sjmf <- summary(jointFit)
  obs <- rbind(
    sjmf$`CoefTable-Long`[, c("Value", "Std.Err")],
    sjmf$`CoefTable-Event`[, c("Value", "Std.Err")]
  )[c(1, 2, 3, 4, 5), ]
  th <- c(
    settings$thetas,
    settings$gamma,
    settings$alpha,
    settings$beta,
    log(settings$nu)
  )[c(1, 2, 3, 5, 6)]
  out <- as.data.frame(cbind(obs, th))
  dimnames(out) <- list(c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)")[c(1, 2, 3, 5, 6)], c("Obs.mean", "Obs.se", "Th"))
  out <- out[c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), ]
  out[, "RE.mean"] <- (out[, "Obs.mean"] - out[, "Th"]) / out[, "Th"]
  out[, "RE.lower"] <- ((out[, "Obs.mean"] - out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "RE.upper"] <- ((out[, "Obs.mean"] + out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "p"] <- rownames(out)

  out <- unlist(out[, c("RE.mean", "RE.lower", "RE.upper")], use.names = FALSE)
  names(out) <- apply(expand.grid(c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), c(".mean", ".lower", ".upper")), 1, paste0, collapse = "")
  return(out)
})

SjoineR <- compiler::cmpfun(function(settings) {
  smldta <- simulateJM(
    n = settings$n,
    times = settings$times,
    thetas = settings$thetas,
    gamma = settings$gamma,
    maf = settings$maf,
    Epsilon_thetas = settings$Epsilon_thetas,
    sigma = settings$sigma,
    beta = settings$beta,
    Epsilon_beta = settings$Epsilon_beta,
    alpha = settings$alpha,
    nu = settings$nu,
    lambda = settings$lambda,
    cens = settings$cens,
    seed = settings$seed
  )
  smldtaclean <- smldta[smldta[, "ID"] %in% subset(smldta, get("Time") == 0)[, "ID"] & !is.na(smldta[, "Yobs"]), ]

  data.long <- smldtaclean[, c("ID", "Time", "Yobs")]
  data.surv <- UniqueVariables(smldtaclean, var.col = c("DiscreteEventTime", "Event"), id.col = "ID")
  data.baseline <- UniqueVariables(smldtaclean, var.col = c("SNP"), id.col = "ID")
  data.jd <- jointdata(
    longitudinal = data.long,
    survival = data.surv,
    baseline = data.baseline,
    id.col = "ID",
    time.col = "Time"
  )
  model.jointrandom <- joint(
    data = data.jd,
    long.formula = settings$Yfixed,
    surv.formula = settings$Ysurvival,
    model = "int"
  )

  obs <- cbind(rbind(
    model.jointrandom$coefficients$fixed$longitudinal[1, 1],
    model.jointrandom$coefficients$fixed$longitudinal[2, 1],
    model.jointrandom$coefficients$fixed$longitudinal[3, 1],
    log(mean(model.jointrandom$hazard)),
    model.jointrandom$coefficients$fixed$survival,
    model.jointrandom$coefficients$latent,
    nu = 0
  ), NA)
  # model.jointrandom2 <- jointSE(fitted = model.jointrandom, n.boot = 100)
  # obs <- rbind.data.frame(
  # as.numeric(model.jointrandom2[1, c(3, 4)]),
  # as.numeric(model.jointrandom2[2, c(3, 4)]),
  # as.numeric(model.jointrandom2[3, c(3, 4)]),
  # as.numeric(c(log(mean(model.jointrandom$hazard)), NA)),
  # as.numeric(model.jointrandom2[4, c(3, 4)]),
  # as.numeric(model.jointrandom2[5, c(3, 4)]),
  # as.numeric(c(0, NA))
  # )
  th <- c(
    settings$thetas,
    settings$gamma,
    settings$alpha,
    settings$beta,
    log(settings$nu)
  )
  rownames(obs) <- c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)")
  names(th) <- c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)")
  out <- cbind.data.frame(obs, th)
  dimnames(out) <- list(c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)"), c("Obs.mean", "Obs.se", "Th"))
  out <- out[c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta", "log(nu)", "log(lambda)"), ]
  out[, "RE.mean"] <- (out[, "Obs.mean"] - out[, "Th"]) / out[, "Th"]
  out[, "RE.lower"] <- ((out[, "Obs.mean"] - out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "RE.upper"] <- ((out[, "Obs.mean"] + out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "p"] <- rownames(out)

  out <- unlist(out[, c("RE.mean", "RE.lower", "RE.upper")], use.names = FALSE)
  names(out) <- apply(expand.grid(c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta", "log(nu)", "log(lambda)"), c(".mean", ".lower", ".upper")), 1, paste0, collapse = "")
  return(out)
})

SjoineR2 <- compiler::cmpfun(function(settings, data) {
  smldtaclean <- data[data[, "ID"] %in% subset(data, get("Time") == 0)[, "ID"] & !is.na(data[, "Yobs"]), ]

  data.long <- smldtaclean[, c("ID", "Time", "Yobs")]
  data.surv <- UniqueVariables(smldtaclean, var.col = c("DiscreteEventTime", "Event"), id.col = "ID")
  data.baseline <- UniqueVariables(smldtaclean, var.col = c("SNP"), id.col = "ID")
  data.jd <- jointdata(
    longitudinal = data.long,
    survival = data.surv,
    baseline = data.baseline,
    id.col = "ID",
    time.col = "Time"
  )
  model.jointrandom <- joint(
    data = data.jd,
    long.formula = settings$Yfixed,
    surv.formula = settings$Ysurvival,
    model = "int"
  )

  obs <- cbind(rbind(
    model.jointrandom$coefficients$fixed$longitudinal[1, 1],
    model.jointrandom$coefficients$fixed$longitudinal[2, 1],
    model.jointrandom$coefficients$fixed$longitudinal[3, 1],
    log(mean(model.jointrandom$hazard)),
    model.jointrandom$coefficients$fixed$survival,
    model.jointrandom$coefficients$latent,
    nu = 0
  ), NA)
  th <- c(
    settings$thetas,
    settings$gamma,
    settings$alpha,
    settings$beta,
    log(settings$nu)
  )
  out <- as.data.frame(cbind(obs, th))
  dimnames(out) <- list(c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)"), c("Obs.mean", "Obs.se", "Th"))
  out <- out[c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta", "log(nu)", "log(lambda)"), ]
  out[, "RE.mean"] <- (out[, "Obs.mean"] - out[, "Th"]) / out[, "Th"]
  out[, "RE.lower"] <- ((out[, "Obs.mean"] - out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "RE.upper"] <- ((out[, "Obs.mean"] + out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  out[, "p"] <- rownames(out)

  out <- unlist(out[, c("RE.mean", "RE.lower", "RE.upper")], use.names = FALSE)
  names(out) <- apply(expand.grid(c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta", "log(nu)", "log(lambda)"), c(".mean", ".lower", ".upper")), 1, paste0, collapse = "")
  return(out)
})
