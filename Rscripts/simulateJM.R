eventTime <- compiler::cmpfun(function(data, lambda, nu, maxtime = max(data[, "Time"]) + IQR(data[, "Time"])) {
  F <- function(survivaltime, data, lambda, nu) {
    whichinterval <- findInterval(survivaltime, data[, "Time"])
    h <- function(t, data, lambda, nu) {
      lambda * nu * t^(nu - 1) * exp(data[whichinterval, "Ysurv"])
    }
    return(
      integrate(
        h,
        lower = 0, upper = survivaltime,
        lambda = lambda, nu = nu, data = data
      )$value + log(data[whichinterval, "U"])
    )
  }
  res <- tryCatch(uniroot(F, interval = c(1e-10, maxtime), lambda = lambda, nu = nu, data = data)$root, error = function(c) {
    maxtime
  })
  # res <- uniroot(F, interval = c(1e-10, maxtime), lambda = lambda, nu = nu, data = data, extendInt = "upX")$root
  return(res)
})

simulateJM <- compiler::cmpfun(function(n, times, thetas, gamma, maf, Epsilon_thetas, sigma, beta, Epsilon_beta, alpha, nu, lambda, cens, seed) {
  if (!is.null(seed)) {
    # R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    # RNGstate <- structure(seed, kind = as.list(RNGkind()))
    # on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    on.exit(rm(".Random.seed", envir = .GlobalEnv))
  } else {}

  dta <- data.frame(ID = rep(seq_len(n), each = length(times)))
  dta[, "Time"] <- rep(times, times = n)
  dta[, "SNP"] <- rep(sample(c(0, 1, 2), n, replace = TRUE, prob = c(maf^2, 2 * maf * (1 - maf), (1 - maf)^2)), each = length(times))

  Xfixed <- model.matrix.default(reformulate(c("Time", "SNP")), data = dta)
  Xrandom <- model.matrix.default(reformulate(c("Time", "SNP")[1]), data = dta)
  Z <- model.matrix.default(reformulate(c("Time", "SNP")[2]), data = dta)

  thetasi <- matrix(rep(mvrnorm(n, rep(0, ncol(Z)), Epsilon_thetas), each = length(times)), ncol = ncol(Z))
  dta[, "Ytrue"] <- Xfixed %*% c(thetas, gamma) + rowSums(Xrandom * thetasi)
  dta[, "Y"] <- rnorm(nrow(dta), mean = dta[, "Ytrue"], sd = sigma)
  if (!(beta == 0 & all(alpha == 0) & nu == 1 & lambda == 0)) {
    dta[, "Ysurv"] <- dta[, "Ytrue"] * beta + Z %*% alpha
    dta[, "U"] <- rep(runif(n), each = length(times))
    dta[, "TrueEventTime"] <- rep(
      by(dta, dta[, "ID"], eventTime, lambda = lambda, nu = nu, maxtime = max(dta[, "Time"]) + IQR(dta[, "Time"])),
      each = length(times)
    )

    dta[, "CensorTime"] <- switch(cens,
      "uniform" = {
        rep(runif(n, 0, max(times)), each = length(times))
      },
      "constant" = {
        max(times)
      }
    )
    dta[, "CensorTime"] <- .Internal(pmin(dta[, "TrueEventTime"], dta[, "CensorTime"]))
    dta[, "Event"] <- as.numeric(dta[, "TrueEventTime"] <= dta[, "CensorTime"])

    dta[, "DiscreteEventTime"] <- c(times, max(times))[findInterval(dta[, "TrueEventTime"], times) + 1]
    dta[, "Yobs"] <- NA
    dta[dta[, "Time"] < dta[, "DiscreteEventTime"], "Yobs"] <- dta[dta[, "Time"] < dta[, "DiscreteEventTime"], "Y"]

    # 1-prop.table(table(subset(dta, Time==0)[, "Event"]))["0"]
  } else {}
  return(dta)
})

plotJM <- compiler::cmpfun(function(data, drawse = TRUE, rangepercent = c(0.1, 1), theme = theme_bw(noGrid = TRUE)) {
  myPalettes <- rbind(
    c(Name = "dodgerblue", Hex = "#1E90FF", RGB = "rgb(30/255, 144/255, 255/255)"),
    c(Name = "firebrick2", Hex = "#EE2C2C", RGB = "rgb(238/255, 44/255, 44/255)"),
    c(Name = "springgreen3", Hex = "#008B45", RGB = "rgb(0/255, 139/255, 69/255)"),
    c(Name = "maroon2", Hex = "#EE30A7", RGB = "rgb(238/255, 48/255, 167/255)"),
    c(Name = "goldenrod2", Hex = "#EEB422", RGB = "rgb(238/255, 180/255, 34/255)"),
    c(Name = "deepskyblue", Hex = "#00BFFF", RGB = "rgb(0/255, 191/255, 255/255)"),
    c(Name = "mediumpurple1", Hex = "#AB82FF", RGB = "rgb(171/255, 130/255, 255/255)"),
    c(Name = "tan1", Hex = "#FFA54F", RGB = "rgb(255/255, 165/255, 79/255)")
  )
  myPalette <- myPalettes[, "Name"]
  if ("ARG" %in% colnames(data)) {
    out <- do.call("rbind", by(data, data[, "ARG"], function(data) {
      out <- as.data.frame(do.call("rbind", lapply(seq_len(nrow(data))[-1], function(irow) {
        colMeans(data[seq_len(irow), sapply(c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), grep, colnames(data))])
      })))
      out <- Reduce("merge", lapply(c(".mean", ".lower", ".upper"), function(iname) {
        tmp <- stack(out[, grep(iname, colnames(out))])
        tmp[, 2] <- gsub("\\..*", "", tmp[, 2])
        tmp <- cbind(tmp, rep(seq_len(nrow(out)), length(unique(tmp[, 2]))))
        colnames(tmp) <- toupper(c(gsub(".", "", iname, fixed = TRUE), "Parameter", "simulation"))
        return(tmp)
      }))
      out[, "PARAMETER"] <- factor(out[, "PARAMETER"], levels = c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"))


      out <- subset(out, get("SIMULATION") >= rangepercent[1] * max(out[, "SIMULATION"]) & get("SIMULATION") <= rangepercent[2] * max(out[, "SIMULATION"]))
      return(cbind.data.frame(out, ARG = unique(data[, "ARG"])))
    }))
  } else {
    out <- as.data.frame(do.call("rbind", lapply(seq_len(nrow(data))[-1], function(irow) {
      colMeans(data[seq_len(irow), sapply(c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), grep, colnames(data))])
    })))
    out <- Reduce("merge", lapply(c(".mean", ".lower", ".upper"), function(iname) {
      tmp <- stack(out[, grep(iname, colnames(out))])
      tmp[, 2] <- gsub("\\..*", "", tmp[, 2])
      tmp <- cbind(tmp, rep(seq_len(nrow(out)), length(unique(tmp[, 2]))))
      colnames(tmp) <- toupper(c(gsub(".", "", iname, fixed = TRUE), "Parameter", "simulation"))
      return(tmp)
    }))
    out[, "PARAMETER"] <- factor(out[, "PARAMETER"], levels = c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"))

    out <- subset(out, get("SIMULATION") >= rangepercent[1] * max(out[, "SIMULATION"]) & get("SIMULATION") <= rangepercent[2] * max(out[, "SIMULATION"]))
  }
  collabs <- sapply(levels(out[, "PARAMETER"]), function(icol) {
    if (icol %in% c("theta_0", "theta_1")) {
      eval(parse(text = paste0("bquote(E(", gsub("_(.*)", "[\\1]", icol), "))")))
    } else {
      eval(parse(text = paste0("bquote(", gsub("_(.*)", "[\\1]", icol), ")")))
    }
  })
  p <- ggplot(data = out, aes_string(x = "SIMULATION", y = "MEAN", colour = "PARAMETER", fill = "PARAMETER")) +
    labs(x = "Simulation", y = "Relative error") +
    geom_hline(yintercept = 0, colour = "black", linetype = 2) +
    scale_colour_manual(name = "PARAMETER", labels = collabs, values = myPalette[seq_along(levels.default(out[, "PARAMETER"]))]) +
    scale_fill_manual(name = "PARAMETER", labels = collabs, values = myPalette[seq_along(levels.default(out[, "PARAMETER"]))]) +
    scale_y_continuous(labels = percent)
  if (drawse) {
    p <- p + geom_smooth(aes_string(ymin = "LOWER", ymax = "UPPER"), stat = "identity", alpha = 0.25)
  } else {
    p <- p + geom_line()
  }
  if ("ARG" %in% colnames(data)) {
    p <- p + theme + facet_wrap(~ARG)
  } else {
    p <- p + theme
  }
  return(p)
})

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
