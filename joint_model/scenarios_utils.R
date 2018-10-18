eventTime <- function(mtime, lambda0, v, theta0i, theta1i, beta, gamma, alpha, Z, u) {
  F <- function(s, lambda0, v, theta0i, theta1i, beta, gamma, alpha, Z, u) {
    h <- function(s, lambda0, v, theta0i, theta1i, beta, gamma, alpha, Z) {
      lambda0 * v * s^(v - 1) * exp(beta * (theta0i + theta1i * s) + (beta * gamma + alpha) * Z)
    }
    return(
      integrate(
        h,
        lower = 0, upper = s,
        lambda0 = lambda0,
        v = v,
        theta0i = theta0i,
        theta1i = theta1i,
        beta = beta,
        gamma = gamma,
        alpha = alpha,
        Z = Z
      )$value + log(1 - u)
    )
  }
  res <- try({
    uniroot(
      F,
      interval = c(1e-10, mtime),
      lambda0 = lambda0,
      v = v,
      theta0i = theta0i,
      theta1i = theta1i,
      beta = beta,
      gamma = gamma,
      alpha = alpha,
      Z = Z,
      u = u
    )$root
  }, silent = TRUE)
  if (class(res) == "try-error") {
    res <- mtime
  }
  return(res)
}



findlambda0 <- function(lambda0, v = 1, n, I, censor, maf, theta0, theta0.sigma, theta1, theta1.sigma, gamma, alpha, beta) {
  f <- function(lambda0) {
    trueTimes <- sapply(seq(n), function(i) {
      eventTime(
        mtime = max(times) + 2 * IQR(times),
        lambda0 = lambda0,
        v = v,
        theta0i = rnorm(1, mean = theta0, sd = theta0.sigma),
        theta1i = rnorm(1, mean = theta1, sd = theta1.sigma),
        beta = beta,
        gamma = gamma,
        alpha = alpha,
        Z = sample(c(0, 1, 2), 1, prob = c(maf^2, 2 * maf * (1 - maf), (1 - maf)^2)),
        u = runif(1)
      )
    })
    event <- as.numeric(trueTimes <= censor)
    x <- sum(event) / length(event)
    return(x - I)
  }
  return(uniroot(f, interval = c(lambda0 / 100, lambda0 * 100), tol = lambda0 / 100)$root)
}

simulateDTA <- function(
                        nsample, times, maf, I, censor = max(times),
                        theta0, theta0.sigma, theta1, theta1.sigma,
                        gamma, alpha, beta,
                        epsilon, lambda0 = NULL, v = 1) {
  # censor <- runif(nsample, 0, max(times)+2*IQR(times))
  if (is.null(lambda0)) {
    if (is.function(censor)) {
      mcensor <- mean(censor(10000))
    } else {
      mcensor <- censor
    }
    lambda0init <- ((log(1 / (1 - I)) * beta * theta1) / (exp(beta * theta1 * censor) - 1)) * exp(-beta * theta0 - (beta * gamma + alpha) * 2 * maf)
    lambda0 <- mean(sapply(seq(10), function(i) {
      findlambda0(
        lambda0init,
        v = v, n = 1000, I, censor, maf,
        theta0, theta0.sigma, theta1, theta1.sigma,
        gamma, alpha, beta
      )
    }))
  }

  u <- runif(nsample)
  Zi <- sample(c(0, 1, 2), nsample, replace = TRUE, prob = c(maf^2, 2 * maf * (1 - maf), (1 - maf)^2))
  theta0i <- rnorm(nsample, mean = theta0, sd = theta0.sigma)
  theta1i <- rnorm(nsample, mean = theta1, sd = theta1.sigma)
  Yit <- matrix(rep(theta0i, length(times)), ncol = length(times)) +
    matrix(rep(theta1i, length(times)), ncol = length(times)) * matrix(times, nrow = nsample, ncol = length(times), byrow = TRUE) +
    matrix(rep(gamma * Zi, length(times)), ncol = length(times)) +
    matrix(rnorm(nsample * length(times), mean = theta1, sd = epsilon), ncol = length(times))
  # mvrnorm(n = nsample,
  # mu = rep(0, length(times)),
  # Sigma = diag(x = epsilon^2, nrow = length(times), ncol = length(times))
  # )

  dtaY <- matrix(NA, nrow = nsample, ncol = length(times) + 4)
  colnames(dtaY) <- c("ID", paste0("T", times), "Z", "Event", "EventTime")
  dtaY[, "ID"] <- seq(nsample)
  dtaY[, paste0("T", times)] <- Yit
  dtaY[, "Z"] <- Zi

  for (i in seq(nsample)) {
    trueTime <- eventTime(
      mtime = max(times) + 2 * IQR(times),
      lambda0 = lambda0,
      v = v,
      theta0i = theta0i[i],
      theta1i = theta1i[i],
      beta = beta,
      gamma = gamma,
      alpha = alpha,
      Z = Zi[i],
      u = u[i]
    )

    if (is.function(censor)) {
      event <- as.numeric(trueTime <= censor(1))
    } else {
      event <- as.numeric(trueTime <= censor)
    }

    timeEvent <- times[findInterval(trueTime, times, all.inside = TRUE) + 1]
    if (event == 1) {
      dtaY[i, paste0("T", times)[grep(timeEvent, times):length(times)]] <- NA
      dtaY[i, c("Event", "EventTime")] <- c(event, timeEvent)
    } else {
      dtaY[i, c("Event", "EventTime")] <- c(event, timeEvent)
    }
  }

  smldta <- reshape(
    as.data.frame(dtaY),
    idvar = "ID",
    varying = list(paste0("T", times)),
    direction = "long",
    timevar = "Time",
    v.names = "Y",
    times = times
  )
  smldta <- smldta[order(smldta[, "ID"], smldta[, "Time"]), ]
  return(smldta)
}


get.JM <- function(data) {
  inc <- prop.table(table(subset(data, Time == 0)[, "Event"]))["1"]
  lmeFit.s <- lme(Y ~ Time + Z, random = ~1 | ID, data = data, na.action = "na.omit", control = lmeControl(opt = "optim"))
  coxFit.s <- coxph(Surv(EventTime, Event) ~ Z, data = subset(data, Time == 0), na.action = "na.omit", x = TRUE)
  jointFit.s <- jointModel(lmeObject = lmeFit.s, survObject = coxFit.s, timeVar = "Time", method = "weibull-PH-aGH", control = list(knots = c(max(times) - 1e-5)))
  tmpres <- c(unlist(jointFit.s$coefficients), inc = inc)[-max(grep("(Intercept)", names(c(unlist(jointFit.s$coefficients), inc = inc)), fixed = TRUE))]
  tmpres <- tmpres[c(1, 2, 3, 5, 6, 7, 4, 8)]
  names(tmpres) <- c("theta_0", "theta_1", "gamma", "alpha", "beta", "sigma", "epsilon", "incidence")
  tmpres["sigma"] <- sqrt(tmpres["sigma"])
  return(tmpres)
}


get.joineR <- function(data) {
  inc <- prop.table(table(subset(data, Time == 0)[, "Event"]))["1"]
  data.long <- data[, c("ID", "Time", "Y")]
  data.surv <- UniqueVariables(data, var.col = c("EventTime", "Event"), id.col = "ID")
  data.baseline <- UniqueVariables(data, var.col = c("Z"), id.col = "ID")
  data.jd <- jointdata(
    longitudinal = data.long,
    survival = data.surv,
    baseline = data.baseline,
    id.col = "ID",
    time.col = "Time"
  )
  model.jointrandom <- joint(
    data = data.jd,
    long.formula = Y ~ Time + Z,
    surv.formula = Surv(EventTime, Event) ~ Z,
    model = "int"
  )
  tmpres <- c(
    model.jointrandom$coefficients$fixed$longitudinal[1, 1],
    model.jointrandom$coefficients$fixed$longitudinal[2, 1],
    model.jointrandom$coefficients$fixed$longitudinal[3, 1],
    model.jointrandom$coefficients$fixed$survival,
    model.jointrandom$coefficients$latent,
    as.vector(sqrt(model.jointrandom$sigma.u)),
    sqrt(model.jointrandom$sigma.z),
    inc
  )
  names(tmpres) <- c("theta_0", "theta_1", "gamma", "alpha", "beta", "sigma", "epsilon", "incidence")
  return(tmpres)
}

get.TS <- function(data) {
  inc <- prop.table(table(subset(data, Time == 0)[, "Event"]))["1"]
  lmeFit.s <- lme(Y ~ Time, random = ~1 | ID, data = data, na.action = "na.omit", control = lmeControl(opt = "optim"))
  data[, "Yfit"] <- data.frame(lmeFit.s$fitted)[rownames(data), "ID"]
  coxFit.s <- coxph(Surv(EventTime, Event) ~ Z + Yfit, data = data, na.action = "na.omit", x = TRUE)
  lmeFit.su <- summary(lmeFit.s)
  coxFit.su <- summary(coxFit.s)
  # tmpres <- c(lmeFit.su$tTable[, 1], NA, coxFit.su$coefficients[, 1], sqrt(as.numeric(VarCorr(lmeFit.su)[1, 1])), lmeFit.su$sigma, inc)
  tmpres <- c(NA, NA, NA, coxFit.su$coefficients[, 1], NA, NA, inc)
  names(tmpres) <- c("theta_0", "theta_1", "gamma", "alpha", "beta", "sigma", "epsilon", "incidence")
  return(tmpres)
}

get.COX <- function(data) {
  inc <- prop.table(table(subset(data, Time == 0)[, "Event"]))["1"]
  coxFit.s <- coxph(Surv(EventTime, Event) ~ Z + Y, data = data, na.action = "na.omit", x = TRUE)
  coxFit.su <- summary(coxFit.s)
  tmpres <- c(NA, NA, NA, coxFit.su$coefficients[, 1], NA, NA, inc)
  names(tmpres) <- c("theta_0", "theta_1", "gamma", "alpha", "beta", "sigma", "epsilon", "incidence")
  return(tmpres)
}

get.LMM <- function(data) {
  inc <- prop.table(table(subset(data, Time == 0)[, "Event"]))["1"]
  lmeFit.s <- lme(Y ~ Time + Z, random = ~1 | ID, data = data, na.action = "na.omit", control = lmeControl(opt = "optim"))
  lmeFit.su <- summary(lmeFit.s)
  tmpres <- c(lmeFit.su$tTable[, 1], NA, NA, sqrt(as.numeric(VarCorr(lmeFit.su)[1, 1])), lmeFit.su$sigma, inc)
  names(tmpres) <- c("theta_0", "theta_1", "gamma", "alpha", "beta", "sigma", "epsilon", "incidence")
  return(tmpres)
}

simulate <- function(data, model = c("JM", "joineR"), f = NULL) {
  smldta <- data
  if (!is.null(f)) {
    smldta <- f(smldta)
  }

  result <- lapply(model, function(imodel) {
    switch(EXPR = imodel,
      "JM" = {
        get.JM(smldta)
      },
      "joineR" = {
        get.joineR(smldta)
      },
      "TS" = {
        get.TS(smldta)
      },
      "COX" = {
        get.COX(smldta)
      },
      "LMM" = {
        get.LMM(smldta)
      }
    )
  })
  names(result) <- model
  return(result)
}

plotsimu <- function(model = c("JM", "joineR"), name, title = "", pattern, parameter, drawse = TRUE, truevalues, width = 16.5, height = 12) {
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
  ldta <- lapply(model, function(imodel) {
    pattern <- paste0(pattern, "_", imodel, ".Rdata")
    dta <- do.call("rbind", lapply(list.files(path = ".", pattern = pattern), function(x) {
      dtatmp <- get(load(x))
      cnames <- intersect(colnames(dtatmp), names(truevalues))
      dtatmp[, cnames] <- t((t(dtatmp[, cnames]) - truevalues[cnames]) / truevalues[cnames])

      simu_mean <- matrix(0, nrow = nrow(dtatmp), ncol = ncol(dtatmp))
      simu_se <- matrix(0, nrow = nrow(dtatmp), ncol = ncol(dtatmp))
      colnames(simu_mean) <- colnames(dtatmp)
      colnames(simu_se) <- colnames(dtatmp)
      for (irow in seq(1, nrow(dtatmp))) {
        simu_mean[irow, ] <- c(irow, colMeans(dtatmp[1:irow, -1, drop = FALSE]))
        simu_se[irow, ] <- c(irow, colSe(dtatmp[1:irow, -1, drop = FALSE]))
      }
      simu_dta <- merge(simu_mean, simu_se, by = "ID", suffixes = c(".mean", ".se"))
      simu_dta <- data.frame(
        simu_dta,
        p = gsub(pattern, "\\1", x)
      )
      return(simu_dta)
    }))
  })
  names(ldta) <- model

  plotrange <- do.call("rbind", lapply(ldta, function(dta) {
    dta <- dta[!dta[, "ID"] %in% seq(round(0.1 * max(dta[, "ID"]))), ]
    rbind(
      dta[, grep(".mean", colnames(dta), value = TRUE)] + dta[, grep(".se", colnames(dta), value = TRUE)],
      dta[, grep(".mean", colnames(dta), value = TRUE)] - dta[, grep(".se", colnames(dta), value = TRUE)]
    )
  }))
  colnames(plotrange) <- gsub(".mean", "", colnames(plotrange))
  plotrange <- range(pretty_breaks()(range(plotrange[, seq(5)])))


  for (imodel in model) {
    dta <- ldta[[imodel]]
    dta <- dta[!dta[, "ID"] %in% seq(round(0.1 * max(dta[, "ID"]))), ]
    plotlist <- lapply(gsub(".mean", "", grep(".mean", colnames(dta), value = TRUE)), function(icol) {
      simu_dta <- dta[, c("ID", "p", grep(paste0(icol, ".mean"), colnames(dta), value = TRUE), grep(paste0(icol, ".se"), colnames(dta), value = TRUE))]
      colnames(simu_dta) <- c("ID", "Parameter", "Mean", "SE")
      simu_dta[, "Parameter"] <- factor(simu_dta[, "Parameter"], levels = sort(as.numeric(unique(simu_dta[, "Parameter"]))))

      p <- ggplot(data = simu_dta, aes(x = ID, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Parameter, fill = Parameter)) + theme_bw(base_size = 16, noGrid = TRUE)
      if (drawse) {
        p <- p + geom_smooth(stat = "identity", alpha = 0.25)
      }
      p <- p + geom_line() +
        geom_hline(yintercept = 0, colour = "black", linetype = 2)
      if (icol %in% c("theta_0", "theta_1")) {
        p <- p + labs(title = eval(parse(text = paste0("bquote(E(", gsub("_(.*)", "[\\1]", icol), "))"))), x = "Simulation", y = NULL)
      } else {
        p <- p + labs(title = eval(parse(text = paste0("bquote(", gsub("_(.*)", "[\\1]", icol), ")"))), x = "Simulation", y = NULL)
      }
      p <- p + scale_colour_manual(name = parameter, values = myPalette[seq(length(unique(simu_dta[, "Parameter"])), 1)]) +
        scale_fill_manual(name = parameter, values = myPalette[seq(length(unique(simu_dta[, "Parameter"])), 1)]) +
        coord_cartesian(ylim = plotrange, xlim = range(dta[, "ID"]))
      if (icol != "incidence") {
        p <- p + theme(axis.title.y = element_text(angle = 0))
      }
      return(p)
    })
    names(plotlist) <- gsub(".mean", "", grep(".mean", colnames(dta), value = TRUE))

    plotlist <- plotlist[names(plotlist) %in% c("theta_0", "theta_1", "gamma", "alpha", "beta")]

    plotlabel <- paste0("textGrob(expression('Model", paste0(" ", title), ":  ' ~ X['i'](t) == theta['0i'] + theta['1i']*t + gamma*Z['i'] ~ '  |  ' ~ Y['i'](t) == X['i'](t) + epsilon['ij'] ~ '  |  ' ~ lambda['i'](t) == lambda['0'](t)*exp(beta*X['i'](t) + alpha*Z['i'])), gp = gpar(fontsize = 20, font = 3))")

    png(file = paste0(name, "_", imodel, ".png"), width = width, height = height, units = "in", res = 300)
    eval(parse(text = paste0(
      "grid.arrange(",
      paste(paste0("plotlist[[", seq(length(plotlist)), "]]"), collapse = ", "),
      ", nrow = 2, ncol = 3, main = ",
      plotlabel,
      ")"
    )))
    dev.off()
  }
  return(invisible(NULL))
}

plotsimumodel <- function(name, title = "", file, drawse = TRUE, truevalues, width = 16.5, height = 12) {
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

  dtatmp <- get(load(file))
  dta <- do.call("rbind", by(dtatmp, dtatmp[, "Model"], function(dtatmp) {
    simu_mean <- as.data.frame(matrix(0, nrow = nrow(dtatmp), ncol = ncol(dtatmp)))
    simu_se <- as.data.frame(matrix(0, nrow = nrow(dtatmp), ncol = ncol(dtatmp)))
    colnames(simu_mean) <- colnames(dtatmp)
    colnames(simu_se) <- colnames(dtatmp)
    for (irow in seq(1, nrow(dtatmp))) {
      simu_mean[irow, -1] <- c(irow, colMeans(dtatmp[1:irow, -seq(2), drop = FALSE], na.rm = TRUE))
      simu_se[irow, -1] <- c(irow, colSe(dtatmp[1:irow, -seq(2), drop = FALSE], na.rm = TRUE))
    }
    simu_mean[, 1] <- unique(dtatmp[, "Model"])
    simu_se[, 1] <- unique(dtatmp[, "Model"])
    return(merge(simu_mean, simu_se, by = c("ID", "Model"), suffixes = c(".mean", ".se")))
  }))
  dta <- dta[!dta[, "ID"] %in% seq(round(0.1 * max(dta[, "ID"]))), ]

  plotrange <- rbind(
    dta[, grep(".mean", colnames(dta), value = TRUE)] + dta[, grep(".se", colnames(dta), value = TRUE)],
    dta[, grep(".mean", colnames(dta), value = TRUE)] - dta[, grep(".se", colnames(dta), value = TRUE)]
  )
  colnames(plotrange) <- gsub(".mean", "", colnames(plotrange))
  plotrange <- rbind(plotrange, truevalues[colnames(plotrange)] * 0.999, truevalues[colnames(plotrange)] * 1.001)
  plotrange <- apply(plotrange, 2, function(idta) {
    range(pretty_breaks()(range(idta, na.rm = TRUE)))
  })

  plotlist <- lapply(gsub(".mean", "", grep(".mean", colnames(dta), value = TRUE)), function(icol) {
    simu_dta <- dta[, c("ID", "Model", grep(paste0(icol, ".mean"), colnames(dta), value = TRUE), grep(paste0(icol, ".se"), colnames(dta), value = TRUE))]
    colnames(simu_dta) <- c("ID", "Model", "Mean", "SE")
    simu_dta[, "Model"] <- as.factor(simu_dta[, "Model"])

    p <- ggplot(data = simu_dta, aes(x = ID, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model, fill = Model)) +
      theme_bw(base_size = 16, noGrid = TRUE)
    if (drawse) {
      p <- p + geom_smooth(stat = "identity", alpha = 0.25)
    }
    p <- p + geom_line() +
      geom_hline(yintercept = truevalues[icol], colour = "black", linetype = 2)
    if (icol %in% c("theta_0", "theta_1")) {
      p <- p + labs(title = eval(parse(text = paste0("bquote(E(", gsub("_(.*)", "[\\1]", icol), "))"))), x = "Simulation", y = NULL)
    } else {
      p <- p + labs(title = eval(parse(text = paste0("bquote(", gsub("_(.*)", "[\\1]", icol), ")"))), x = "Simulation", y = NULL)
    }
    p <- p + scale_colour_manual(name = "Model", values = myPalette[seq(length(unique(simu_dta[, "Model"])), 1)]) +
      scale_fill_manual(name = "Model", values = myPalette[seq(length(unique(simu_dta[, "Model"])), 1)]) +
      coord_cartesian(ylim = plotrange[, icol], xlim = range(dta[, "ID"]))
    if (icol != "incidence") {
      p <- p + theme(axis.title.y = element_text(angle = 0))
    }
    return(p)
  })
  names(plotlist) <- gsub(".mean", "", grep(".mean", colnames(dta), value = TRUE))

  plotlist <- plotlist[names(plotlist) %in% c("theta_0", "theta_1", "gamma", "alpha", "beta")]

  plotlabel <- paste0("textGrob(expression('Model", paste0(" ", title), ":  ' ~ X['i'](t) == theta['0i'] + theta['1i']*t + gamma*Z['i'] ~ '  |  ' ~ Y['i'](t) == X['i'](t) + epsilon['ij'] ~ '  |  ' ~ lambda['i'](t) == lambda['0'](t)*exp(beta*X['i'](t) + alpha*Z['i'])), gp = gpar(fontsize = 20, font = 3))")

  png(file = paste0(name, ".png"), width = width, height = height, units = "in", res = 300)
  eval(parse(text = paste0(
    "grid.arrange(",
    paste(paste0("plotlist[[", seq(length(plotlist)), "]]"), collapse = ", "),
    ", nrow = 2, ncol = 3, main = ",
    plotlabel,
    ")"
  )))
  dev.off()
  return(invisible(NULL))
}
