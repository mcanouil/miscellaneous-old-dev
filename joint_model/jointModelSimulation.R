restore <- function(env) {
  for (n in ls(env, all.names = TRUE)) {
    assign(n, get(n, env), .GlobalEnv)
  }
}


findAlphai <- function(x, settings) {
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
    alpha = c(x, settings$alpha[2]),
    nu = settings$nu,
    lambda = settings$lambda,
    cens = settings$cens,
    seed = 27051988
  )
  return(prop.table(table(factor(subset(smldta, Time == 0)[, "Event"], levels = c(0, 1))))[["1"]] - settings$d)
}

findAlphai2 <- function(x, settings) {
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
    alpha = c(x, settings$alpha[2]),
    nu = settings$nu,
    lambda = settings$lambda,
    cens = settings$cens,
    seed = settings$seed
  )
  return(prop.table(table(factor(subset(smldta, Time == 0)[, "Event"], levels = c(0, 1))))[["1"]] - settings$d)
}

missingUniform <- function(data, missingdta) {
  if (missingdta > 0) {
    datatmp <- na.exclude(data)
    # missingdta <- imissingdta # (length(times)*n / (length(times)*n - n)) * imissingdta
    X <- datatmp[, "Time"] != 0
    nna <- missingdta * length(which(X)) - sum(is.na(datatmp[which(X), "Yobs"]))
    Y <- which(X & !is.na(datatmp[, "Time"]))
    datatmp[sample(Y, min(nna, length(Y))), "Yobs"] <- NA
    rnames <- rownames(datatmp)
    datatmp <- do.call("rbind", by(datatmp, datatmp[, "ID"], function(DD) {
      tmp <- DD[grep(max(DD[!is.na(DD[, "Yobs"]), "Time"]), DD[, "Time"]) + 1, "Time"]
      if (!is.na(tmp) && (unique(DD[, "Event"]) == 1 & tmp < unique(DD[, "DiscreteEventTime"]))) {
        DD[, "DiscreteEventTime"] <- max(DD[, "Time"])
        DD[, "Event"] <- 0
      }
      return(DD)
    }))
    rownames(datatmp) <- rnames
    data[gsub(".\\.", "", rownames(datatmp)), ] <- datatmp
  }
  return(data)
}


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
    set.seed(seed)
    on.exit(rm(".Random.seed", envir = .GlobalEnv))
  }

  dta <- data.frame(ID = rep(seq_len(n), each = length(times)))
  dta[, "Time"] <- rep(times, times = n)
  dta[, "SNP"] <- rep(sample(c(0, 1, 2), n, replace = TRUE, prob = c(maf^2, 2 * maf * (1 - maf), (1 - maf)^2)), each = length(times))

  Xfixed <- model.matrix.default(reformulate(c("Time", "SNP")), data = dta)
  Xrandom <- model.matrix.default(reformulate(c("Time", "SNP")[1]), data = dta)
  Z <- model.matrix.default(reformulate(c("Time", "SNP")[2]), data = dta)

  thetasi <- matrix(rep(MASS::mvrnorm(n, rep(0, ncol(Z)), Epsilon_thetas), each = length(times)), ncol = ncol(Z))
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
    dta[, "Yobs"] <- ifelse(dta[, "Time"] < dta[, "DiscreteEventTime"] | dta[, "Event"] == 0, dta[, "Y"], NA)
  }
  return(dta)
})

plotJM <- compiler::cmpfun(function(data, drawse = TRUE, rangepercent = c(0.1, 1), theme = theme_bw(noGrid = TRUE)) {
  myPalette <- c("dodgerblue", "firebrick2", "springgreen3", "maroon2", "goldenrod2", "deepskyblue", "mediumpurple1", "tan1")
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


SJM <- compiler::cmpfun(function(settings, data) {
  smldtaclean <- data[data[, "ID"] %in% subset(data, get("Time") == 0)[, "ID"] & !is.na(data[, "Yobs"]), ]

  lmeFit <- lme(settings$Yfixed, random = settings$Yrandom, data = smldtaclean, control = lmeControl(opt = "optim"))
  coxFit <- coxph(settings$Ysurvival, data = subset(smldtaclean, get("Time") == 0), x = TRUE)
  jointFit <- try(jointModel(lmeObject = lmeFit, survObject = coxFit, timeVar = "Time", method = "piecewise-PH-aGH", scaleWB = settings$nu), silent = TRUE)
  sjmf <- try(summary(jointFit), silent = TRUE)

  if (is(sjmf, "try-error")) {
    jointFit <- try(jointModel(lmeObject = lmeFit, survObject = coxFit, timeVar = "Time", method = "piecewise-PH-GH", scaleWB = settings$nu), silent = TRUE)
    sjmf <- try(summary(jointFit), silent = TRUE)
  }

  th <- c(
    settings$thetas,
    settings$gamma,
    settings$alpha,
    settings$beta,
    log(settings$nu)
  )[c(1, 2, 3, 5, 6)]

  if (is(jointFit, "try-error") | is(sjmf, "try-error")) {
    obs <- matrix(NA, nrow = 5, ncol = 2, dimnames = list(NULL, c("Value", "Std.Err")))

    out <- as.data.frame(cbind(obs, th))
    dimnames(out) <- list(c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)")[c(1, 2, 3, 5, 6)], c("Obs.mean", "Obs.se", "Th"))
    out <- out[c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), ]
    out[, "RE.mean"] <- NA
    out[, "RE.lower"] <- NA
    out[, "RE.upper"] <- NA
  } else {
    obs <- rbind(
      sjmf$`CoefTable-Long`[, c("Value", "Std.Err")],
      sjmf$`CoefTable-Event`[, c("Value", "Std.Err")]
    )[c(1, 2, 3, 4, 5), ]

    out <- as.data.frame(cbind(obs, th))
    dimnames(out) <- list(c("theta_0", "theta_1", "SNP_L", "log(lambda)", "SNP_S", "beta", "log(nu)")[c(1, 2, 3, 5, 6)], c("Obs.mean", "Obs.se", "Th"))
    out <- out[c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), ]
    out[, "RE.mean"] <- (out[, "Obs.mean"] - out[, "Th"]) / out[, "Th"]
    out[, "RE.lower"] <- ((out[, "Obs.mean"] - out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
    out[, "RE.upper"] <- ((out[, "Obs.mean"] + out[, "Obs.se"]) - out[, "Th"]) / out[, "Th"]
  }
  out[, "p"] <- rownames(out)
  out <- unlist(out[, c("RE.mean", "RE.lower", "RE.upper")], use.names = FALSE)
  names(out) <- apply(expand.grid(c("theta_0", "theta_1", "SNP_L", "SNP_S", "beta"), c(".mean", ".lower", ".upper")), 1, paste0, collapse = "")
  return(out)
})
