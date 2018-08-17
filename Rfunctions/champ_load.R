# Function from ChAMP package customised to allow multiple csv files in root directory

check_sample_sheet <- function(
  base, 
  pattern = "csv$", 
  ignore.case = TRUE, 
  recursive = TRUE, 
  full.names = TRUE
) {
  require(glue)
  list_files <- list.files(
    path = base, 
    pattern = pattern, 
    full.names = full.names, 
    ignore.case = ignore.case, 
    recursive = recursive
  )
  if (length(list_files)>1) {
    warnings("More than one CSV file have been found, please check the 'csvpattern' parameter!")
    list_files <- list_files[1]
    message("File '", list_files, "' will be used.")
  }
  dataheader <- grep("^\\[DATA\\]", readLines(list_files), ignore.case = TRUE)
  if (length(dataheader) == 0) {
    dataheader <- 0
  }
  col_names <- colnames(read.csv(file = list_files, stringsAsFactor = FALSE, skip = dataheader, nrows = 1))
  default_cols <- c(
    "Sample_ID", "Sample_Plate", "Sample_Well", 
    "Sentrix_ID", "Sentrix_Position"
  )
  cols_missing <- default_cols[!default_cols%in%col_names]
  if (length(cols_missing)!=0) {
    stop(
      paste0(
        "Sample Sheet don't have all mandatory columns:\n    ", 
        glue::collapse(cols_missing, last = " and ", sep = ", ")
      )
    )
  }
  return(invisible())
}

read_metharray <- function(basenames, extended = FALSE, verbose = FALSE, force = FALSE, nCores = 1) {
  basenames <- sub("_Grn\\.idat.*", "", basenames)
  basenames <- sub("_Red\\.idat.*", "", basenames)
  stopifnot(!anyDuplicated(basenames))
  G.files <- paste(basenames, "_Grn.idat", sep = "")
  names(G.files) <- basename(basenames)
  these.dont.exists <- !file.exists(G.files)
  if (any(these.dont.exists)) {
    G.files[these.dont.exists] <- paste0(
      G.files[these.dont.exists],
      ".gz"
    )
  }
  if (!all(file.exists(G.files))) {
    noexits <- sub("\\.gz", "", G.files[!file.exists(G.files)])
    stop(
      "The following specified files do not exist:",
      paste(noexits, collapse = ", ")
    )
  }
  R.files <- paste(basenames, "_Red.idat", sep = "")
  names(R.files) <- basename(basenames)
  these.dont.exists <- !file.exists(R.files)
  if (any(these.dont.exists)) {
    R.files[these.dont.exists] <- paste0(
      R.files[these.dont.exists],
      ".gz"
    )
  }
  if (!all(file.exists(R.files))) {
    noexits <- R.files[!file.exists(G.files)]
    stop(
      "The following specified files do not exist:",
      paste(noexits, collapse = ", ")
    )
  }
  stime <- system.time({
    G.idats <- mclapply(G.files, mc.cores = nCores, mc.preschedule = FALSE, function(xx) {
      if (verbose) {
        message("[read.metharray] Reading", basename(xx))
      }
      illuminaio::readIDAT(xx)
    })
    R.idats <- mclapply(R.files, mc.cores = nCores, mc.preschedule = FALSE, function(xx) {
      if (verbose) {
        message("[read.metharray] Reading", basename(xx))
      }
      illuminaio::readIDAT(xx)
    })
  })[3]
  if (verbose) {
    message(sprintf(
      "[read.metharray] Read idat files in %.1f seconds",
      stime
    ))
  }
  if (verbose) {
    message(
      "[read.metharray] Creating data matrices ... ",
      appendLF = FALSE
    )
  }
  ptime1 <- proc.time()
  allNProbes <- sapply(G.idats, function(xx) nrow(xx$Quants))
  arrayTypes <- cbind(
    do.call(rbind, mclapply(allNProbes, mc.cores = nCores, mc.preschedule = FALSE, minfi:::.guessArrayTypes)),
    size = allNProbes
  )
  sameLength <- (length(unique(arrayTypes[, "size"])) == 1)
  sameArray <- (length(unique(arrayTypes[, "array"])) == 1)
  if (!sameLength && !sameArray) {
    cat("[read.metharray] Trying to parse IDAT files from different arrays.\n")
    cat("  Inferred Array sizes and types:\n")
    print(arrayTypes[, c("array", "size")])
    stop("[read.metharray] Trying to parse different IDAT files, of different size and type.")
  }
  if (!sameLength && sameArray && !force) {
    stop("[read.metharray] Trying to parse IDAT files with different array size but seemingly all of the same type.\n  You can force this by 'force=TRUE', see the man page ?read.metharray")
  }
  commonAddresses <- as.character(Reduce("intersect", mclapply(
    G.idats, mc.cores = nCores, mc.preschedule = FALSE,
    function(xx) rownames(xx$Quants)
  )))
  GreenMean <- do.call(cbind, mclapply(G.idats, mc.cores = nCores, mc.preschedule = FALSE, function(xx) xx$Quants[
      commonAddresses,
      "Mean"
    ]))
  RedMean <- do.call(cbind, mclapply(R.idats, mc.cores = nCores, mc.preschedule = FALSE, function(xx) xx$Quants[
      commonAddresses,
      "Mean"
    ]))
  if (extended) {
    GreenSD <- do.call(cbind, mclapply(G.idats, mc.cores = nCores, mc.preschedule = FALSE, function(xx) xx$Quants[
        commonAddresses,
        "SD"
      ]))
    RedSD <- do.call(cbind, mclapply(R.idats, mc.cores = nCores, mc.preschedule = FALSE, function(xx) xx$Quants[
        commonAddresses,
        "SD"
      ]))
    NBeads <- do.call(cbind, mclapply(G.idats, mc.cores = nCores, mc.preschedule = FALSE, function(xx) xx$Quants[
        commonAddresses,
        "NBeads"
      ]))
  }
  ptime2 <- proc.time()
  stime <- (ptime2 - ptime1)[3]
  if (verbose) {
    message(sprintf("done in %.1f seconds", stime))
  }
  if (verbose) {
    message(
      "[read.metharray] Instantiating final object ... ",
      appendLF = FALSE
    )
  }
  ptime1 <- proc.time()
  if (extended) {
    out <- RGChannelSetExtended(
      Red = RedMean, Green = GreenMean,
      RedSD = RedSD, GreenSD = GreenSD, NBeads = NBeads
    )
  } else {
    out <- RGChannelSet(Red = RedMean, Green = GreenMean)
  }
  rownames(out) <- commonAddresses
  out@annotation <- c(array = arrayTypes[1, 1], annotation = arrayTypes[
    1,
    2
  ])
  ptime2 <- proc.time()
  stime <- (ptime2 - ptime1)[3]
  if (verbose) {
    message(sprintf("done in %.1f seconds", stime))
  }
  out
}

read_metharray_exp <- function(base = NULL, targets = NULL, extended = FALSE, recursive = FALSE,
                               verbose = FALSE, force = FALSE, nCores = 1) {
  if (!is.null(targets)) {
    if (!"Basename" %in% names(targets)) {
      stop("Need 'Basename' amongst the column names of 'targets'")
    }
    if (!is.null(base)) {
      files <- file.path(base, basename(targets$Basename))
    } else {
      files <- targets$Basename
    }
    rgSet <- read_metharray(
      files, extended = extended,
      verbose = verbose, force = force, nCores = nCores
    )
    pD <- targets
    pD$filenames <- files
    rownames(pD) <- colnames(rgSet)
    colData(rgSet) <- as(pD, "DataFrame")
    return(rgSet)
  }
  Grn.files <- list.files(
    base, pattern = "_Grn.idat$", recursive = recursive,
    ignore.case = TRUE, full.names = TRUE
  )
  Red.files <- list.files(
    base, pattern = "_Red.idat$", recursive = recursive,
    ignore.case = TRUE, full.names = TRUE
  )
  if (length(Grn.files) == 0 || length(Red.files) == 0) {
    stop("No IDAT files were found")
  }
  commonFiles <- intersect(
    sub("_Grn.idat$", "", Grn.files),
    sub("_Red.idat$", "", Red.files)
  )
  if (length(commonFiles) == 0) {
    stop("No IDAT files with both Red and Green channel were found")
  }
  commonFiles.Grn <- paste(commonFiles, "_Grn.idat", sep = "")
  commonFiles.Red <- paste(commonFiles, "_Red.idat", sep = "")
  if (!setequal(commonFiles.Grn, Grn.files)) {
    warning(sprintf(
      "the following files only exists for the green channel: %s",
      paste(setdiff(Grn.files, commonFiles.Grn), collapse = ", ")
    ))
  }
  if (!setequal(commonFiles.Red, Red.files)) {
    warning(sprintf(
      "the following files only exists for the red channel: %s",
      paste(setdiff(Red.files, commonFiles.Red), collapse = ", ")
    ))
  }
  rgSet <- read_metharray(
    basenames = commonFiles, extended = extended,
    verbose = verbose, force = force, nCores = nCores
  )
  rgSet
}

read_metharray_sheet <- function(base, pattern = "csv$", ignore.case = TRUE, recursive = TRUE, verbose = TRUE, nCores = 1) {
  readSheet <- function(file) {
    dataheader <- grep("^\\[DATA\\]", readLines(file), ignore.case = TRUE)
    if (length(dataheader) == 0) {
      dataheader <- 0
    }
    df <- read.csv(file, stringsAsFactor = FALSE, skip = dataheader)
    if (length(nam <- grep(
      "Sentrix_Position", names(df),
      ignore.case = TRUE, value = TRUE
    )) == 1) {
      df$Array <- as.character(df[, nam])
      df[, nam] <- NULL
    }
    if (length(nam <- grep(
      "Array[\\._]ID", names(df), ignore.case = TRUE,
      value = TRUE
    )) == 1) {
      df$Array <- as.character(df[, nam])
      df[, nam] <- NULL
    }
    if (!"Array" %in% names(df)) {
      warning(sprintf(
        "Could not infer array name for file: %s",
        file
      ))
    }
    if (length(nam <- grep(
      "Sentrix_ID", names(df), ignore.case = TRUE,
      value = TRUE
    )) == 1) {
      df$Slide <- as.character(df[, nam])
      df[, nam] <- NULL
    }
    if (length(nam <- grep(
      "Slide[\\._]ID", names(df), ignore.case = TRUE,
      value = TRUE
    )) == 1) {
      df$Slide <- as.character(df[, nam])
      df[, nam] <- NULL
    }
    if (!"Slide" %in% names(df)) {
      warning(sprintf(
        "Could not infer slide name for file: %s",
        file
      ))
    } else {
      df[, "Slide"] <- as.character(df[, "Slide"])
    }
    if (length(nam <- grep(
      "Plate[\\._]ID", names(df), ignore.case = TRUE,
      value = TRUE
    )) == 1) {
      df$Plate <- as.character(df[, nam])
      df[, nam] <- NULL
    }
    for (nam in c("Pool_ID", "Sample_Plate", "Sample_Well")) {
      if (nam %in% names(df)) {
        df[[nam]] <- as.character(df[[nam]])
      }
    }
    if (!is.null(df$Array)) {
      patterns <- sprintf(
        "%s_%s_Grn.idat", df$Slide,
        df$Array
      )
      allfiles <- list.files(
        dirname(file), recursive = recursive,
        full.names = TRUE
      )
      basenames <- sapply(patterns, function(xx) grep(
          xx,
          allfiles, value = TRUE
        ))
      names(basenames) <- NULL
      basenames <- sub(
        "_Grn\\.idat.*", "", basenames,
        ignore.case = TRUE
      )
      df$Basename <- basenames
    }
    df
  }
  if (!all(file.exists(base))) {
    stop("'base' does not exists")
  }
  info <- file.info(base)
  if (!all(info$isdir) && !all(!info$isdir)) {
    stop("'base needs to be either directories or files")
  }
  if (all(info$isdir)) {
    csvfiles <- list.files(
      base, recursive = recursive,
      pattern = pattern, ignore.case = ignore.case, full.names = TRUE
    )
    if (verbose) {
      message("[read.metharray.sheet] Found the following CSV files:")
      print(csvfiles)
    }
  }
  else {
    csvfiles <- list.files(base, full.names = TRUE)
  }
  dfs <- mclapply(csvfiles, mc.cores = nCores, mc.preschedule = FALSE, readSheet)
  namesUnion <- Reduce(union, lapply(dfs, names))
  df <- do.call(rbind, lapply(dfs, function(df) {
    newnames <- setdiff(namesUnion, names(df))
    newdf <- matrix(
      NA, ncol = length(newnames), nrow = nrow(df),
      dimnames = list(NULL, newnames)
    )
    cbind(df, as.data.frame(newdf))
  }))
  df
}

champ_load <- function(directory = getwd(), method = "ChAMP", methValue = "B",
                       autoimpute = TRUE, filterDetP = TRUE, ProbeCutoff = 0, SampleCutoff = 0.1,
                       detPcut = 0.01, filterBeads = TRUE, beadCutoff = 0.05, filterNoCG = TRUE,
                       filterSNPs = TRUE, population = NULL, filterMultiHit = TRUE,
                       filterXY = TRUE, force = FALSE, arraytype = "450K", csvpattern = "csv$", nCores = 1) {
  message("[===========================]")
  message("[<<<< ChAMP.LOAD START >>>>>]")
  message("-----------------------------")
  mybeadcount <- function(x) {
    nb <- getNBeads(x)
    typeIadd <- getProbeInfo(x, type = "I")
    typeImatchA <- match(typeIadd$AddressA, rownames(nb))
    typeImatchB <- match(typeIadd$AddressB, rownames(nb))
    typeIIadd <- getProbeInfo(x, type = "II")
    typeIImatch <- match(typeIIadd$Address, rownames(nb))
    nbcg <- nb
    locusNames <- getManifestInfo(x, "locusNames")
    bc_temp <- matrix(
      NA_real_, ncol = ncol(x), nrow = length(locusNames),
      dimnames = list(locusNames, sampleNames(x))
    )
    TypeII.Name <- getProbeInfo(x, type = "II")$Name
    bc_temp[TypeII.Name, ] <- nbcg[getProbeInfo(x, type = "II")$AddressA, ]
    TypeI <- getProbeInfo(x, type = "I")
    bcB <- bc_temp
    bcA <- bc_temp
    bcB[TypeI$Name, ] <- nbcg[TypeI$AddressB, ]
    bcA[TypeI$Name, ] <- nbcg[TypeI$AddressA, ]
    bcB3 <- which(bcB < 3)
    bcA3 <- which(bcA < 3)
    bcA2 <- bcA
    bcB2 <- bcB
    bcA2[bcA3] <- NA
    bcA2[bcB3] <- NA
    bc <- data.frame(bcA2)
    bc
  }
  if (method == "minfi") {
    message("\n[ Loading Data with Minfi Method ]")
    message("----------------------------------")
    message("Loading data from ", directory)
    check_sample_sheet(base = directory, pattern = csvpattern)
    suppressWarnings(targets <- read.metharray.sheet(base = directory, pattern = csvpattern))
    rgSet <- read_metharray_exp(
      targets = targets,
      extended = TRUE,
      force = force,
      nCores = nCores
    )
    if (arraytype == "EPIC") {
      rgSet@annotation <- c(
        array = "IlluminaHumanMethylationEPIC",
        annotation = "ilm10b3.hg19"
      )
    }
    sampleNames(rgSet) <- rgSet[[1]]
    pd <- pData(rgSet)
    mset <- preprocessRaw(rgSet)
    detP <- detectionP(rgSet)
    message("<< Read DataSet Success. >>\n")
    if (methValue == "B") {
      tmp <- getBeta(mset, "Illumina")
    } else {
      tmp <- getM(mset)
    }
    tmp[detP >= detPcut] <- NA
    message("The fraction of failed positions per sample\n \n            (You may need to delete samples with high proportion of failed probes\n): ")
    numfail <- matrix(colMeans(is.na(tmp)))
    rownames(numfail) <- colnames(detP)
    colnames(numfail) <- "Failed CpG Fraction."
    print(numfail)
    RemainSample <- which(numfail < SampleCutoff)
    if (any(numfail >= SampleCutoff)) {
      message(
        "The detSamplecut parameter is : ", SampleCutoff,
        "\nSamples : ", paste(rownames(numfail)[which(numfail >=
          SampleCutoff)], collapse = ","), " will be deleted.\n",
        "There are ", length(RemainSample), " samples left for analysis.\n"
      )
    }
    rgSet <- rgSet[, RemainSample]
    detP <- detP[, RemainSample]
    mset <- mset[, RemainSample]
    pd <- pd[RemainSample, ]
    tmp <- tmp[, RemainSample]
    if (filterDetP) {
      mset.f <- mset[rowSums(is.na(tmp)) <= ProbeCutoff *
        ncol(detP), ]
      if (ProbeCutoff == 0) {
        message(
          "Filtering probes with a detection p-value above ",
          detPcut, " in one or more samples has removed ",
          dim(mset)[1] - dim(mset.f)[1], " probes from the analysis. If a large number of probes have been removed, ChAMP suggests you to identify potentially bad samples."
        )
      } else {
        message(
          "Filtering probes with a detection p-value above ",
          detPcut, " in at least ", ProbeCutoff * 100,
          "% of samples has removed ", dim(mset)[1] -
            dim(mset.f)[1], " probes from the analysis. If a large number of probes have been removed, ChAMP suggests you look at the failedSample file to identify potentially bad samples."
        )
      }
      mset <- mset.f
      tmp <- tmp[rowSums(is.na(tmp)) <= ProbeCutoff *
        ncol(detP), ]
      message("<< Filter DetP Done. >>\n")
    }
    if (sum(is.na(tmp)) == 0) {
      message("\nThere is no NA values in your matrix, there is no need to imputation.\n")
    } else {
      message("\nThere are ", sum(is.na(tmp)), " NA remain in filtered Data Set. Impute can be done for remain NAs, but not suitable for small number samples. For small Data Set (like only 20 samples), we suggest you set parameter ProbeCutoff as 0 in champ.load() here, which would remove all NA involved probe no matter how many samples of those probes are NA.\n")
    }
    if (autoimpute & sum(is.na(tmp)) > 0) {
      message(
        "Impute will be conducted here for remain ",
        sum(is.na(tmp)), "  NAs. Note that if you don't do this, NA values would be kept in your data set. You may use champ.impute() function to do more complex imputation as well."
      )
      message("\nImpute function is working now, it may need couple minutes...")
      zz <- file("ImputeMessage.Rout", open = "wt")
      sink(zz)
      sink(zz, type = "message")
      tmp <- impute.knn(tmp, k = 5)$data
      sink(type = "message")
      sink()
      message("<< Imputation Done. >>\n")
    }
    if (filterBeads) {
      bc <- mybeadcount(rgSet)
      bc2 <- bc[rowSums(is.na(bc)) < beadCutoff * (ncol(bc)), ]
      mset.f2 <- mset[featureNames(mset) %in% row.names(bc2), ]
      tmp <- tmp[rownames(tmp) %in% row.names(bc2), ]
      message(
        "Filtering probes with a beadcount <3 in at least ",
        beadCutoff * 100, "% of samples, has removed ",
        dim(mset)[1] - dim(mset.f2)[1], " from the analysis."
      )
      mset <- mset.f2
      message("<< Filter Beads Done. >>\n")
    }
    if (filterNoCG) {
      mset.f2 <- dropMethylationLoci(mset, dropCH = T)
      tmp <- tmp[rownames(tmp) %in% featureNames(mset.f2), ]
      message(
        "Filtering non-cg probes, has removed ",
        dim(mset)[1] - dim(mset.f2)[1], " from the analysis."
      )
      mset <- mset.f2
      message("<< Filter NoCG Done. >>\n")
    }
    if (filterSNPs) {
      if (arraytype == "450K") {
        if (is.null(population)) {
          message("Using general 450K SNP list for filtering.")
          data(hm450.manifest.hg38)
          maskname <- rownames(hm450.manifest.hg38)[which(hm450.manifest.hg38$MASK.general ==
            TRUE)]
        } else if (!population %in% c(
          "AFR", "EAS", "EUR",
          "SAS", "AMR", "GWD", "YRI", "TSI", "IBS",
          "CHS", "PUR", "JPT", "GIH", "CHB", "STU",
          "ITU", "LWK", "KHV", "FIN", "ESN", "CEU",
          "PJL", "ACB", "CLM", "CDX", "GBR", "BEB",
          "PEL", "MSL", "MXL", "ASW"
        )) {
          message("Using general 450K SNP list for filtering.")
          data(hm450.manifest.hg38)
          maskname <- rownames(hm450.manifest.hg38)[which(hm450.manifest.hg38$MASK.general ==
            TRUE)]
        } else {
          message("Using ", population, " specific 450K SNP list for filtering.")
          data(hm450.manifest.pop.hg38)
          maskname <- rownames(hm450.manifest.pop.hg38)[which(hm450.manifest.pop.hg38[
            ,
            paste("MASK.general", population, sep = ".")
          ] ==
            TRUE)]
        }
      } else {
        if (is.null(population)) {
          message("Using general EPIC SNP list for filtering.")
          data(EPIC.manifest.hg38)
          maskname <- rownames(EPIC.manifest.hg38)[which(EPIC.manifest.hg38$MASK.general ==
            TRUE)]
        } else if (!population %in% c(
          "AFR", "EAS", "EUR",
          "SAS", "AMR", "GWD", "YRI", "TSI", "IBS",
          "CHS", "PUR", "JPT", "GIH", "CHB", "STU",
          "ITU", "LWK", "KHV", "FIN", "ESN", "CEU",
          "PJL", "ACB", "CLM", "CDX", "GBR", "BEB",
          "PEL", "MSL", "MXL", "ASW"
        )) {
          message("Using general EPIC SNP list for filtering.")
          data(EPIC.manifest.hg38)
          maskname <- rownames(EPIC.manifest.hg38)[which(EPIC.manifest.hg38$MASK.general ==
            TRUE)]
        } else {
          message("Using ", population, " specific EPIC SNP list for filtering.")
          data(EPIC.manifest.pop.hg38)
          maskname <- rownames(EPIC.manifest.pop.hg38)[which(EPIC.manifest.pop.hg38[
            ,
            paste("MASK.general", population, sep = ".")
          ] ==
            TRUE)]
        }
      }
      mset.f2 <- mset[!featureNames(mset) %in% maskname, ]
      tmp <- tmp[!rownames(tmp) %in% maskname, ]
      message(
        "Filtering probes with SNPs as identified in Zhou's Nucleic Acids Research Paper, 2016, has removed ",
        dim(mset)[1] - dim(mset.f2)[1], " from the analysis."
      )
      mset <- mset.f2
      message("<< Filter SNP Done. >>\n")
    }
    if (filterMultiHit) {
      data(multi.hit)
      mset.f2 <- mset[!featureNames(mset) %in% multi.hit$TargetID, ]
      tmp <- tmp[!rownames(tmp) %in% multi.hit$TargetID, ]
      message(
        "Filtering probes that align to multiple locations as identified in Nordlund et al, has removed ",
        dim(mset)[1] - dim(mset.f2)[1], " from the analysis."
      )
      mset <- mset.f2
      message("<< Filter MultiHit Done. >>\n")
    }
    if (filterXY) {
      if (arraytype == "EPIC") {
        data(probe.features.epic)
      } else {
        data(probe.features)
      }
      autosomes <- probe.features[!probe.features$CHR %in%
        c("X", "Y"), ]
      mset.f2 <- mset[featureNames(mset) %in% row.names(autosomes), ]
      tmp <- tmp[rownames(tmp) %in% row.names(autosomes), ]
      message(
        "Filtering probes on the X or Y chromosome has removed ",
        dim(mset)[1] - dim(mset.f2)[1], " from the analysis."
      )
      mset <- mset.f2
      message("<< Filter XY chromosome Done. >>\n")
    }
    message(paste(if (methValue == "B") {
      "[Beta"
    } else {
      "[M"
    }, "value is selected as output.]\n"))
    beta.raw <- tmp
    intensity <- minfi::getMeth(mset) + minfi::getUnmeth(mset)
    detP <- detP[which(row.names(detP) %in% row.names(beta.raw)), ]
    if (min(beta.raw, na.rm = TRUE) <= 0) {
      beta.raw[beta.raw <= 0] <- min(beta.raw[beta.raw >
        0])
    }
    message("Zeros in your dataset have been replaced with smallest positive value.\n")
    if (max(beta.raw, na.rm = TRUE) >= 0) {
      beta.raw[beta.raw >= 1] <- max(beta.raw[beta.raw <
        1])
    }
    message("One in your dataset have been replaced with largest value below 1.\n")
    message(
      "The analysis will be proceed with ", dim(beta.raw)[1],
      " probes and ", dim(beta.raw)[2], " samples.\n"
    )
    message(
      "Current Data Set contains ", sum(is.na(beta.raw)),
      " NA in ", if (methValue == "B") {
        "[Beta]"
      } else {
        "[M]"
      }, " Matrix.\n"
    )
    message("[<<<<< ChAMP.LOAD END >>>>>>]")
    message("[===========================]")
    message("[You may want to process champ.QC() next.]\n")
    return(list(
      mset = mset, rgSet = rgSet, pd = pd, intensity = intensity,
      beta = beta.raw, detP = detP
    ))
  } else {
    message("\n[ Loading Data with ChAMP Method ]")
    message("----------------------------------")
    message("Note that ChAMP method will NOT return rgSet or mset, they object defined by minfi. Which means, if you use ChAMP method to load data, you can not use SWAN or FunctionNormliazation method in champ.norm() (you can use BMIQ or PBC still). But All other function should not be influenced.\n")
    myImport <- champ.import(directory, arraytype = arraytype)
    if (methValue == "B") {
      myLoad <- champ.filter(
        beta = myImport$beta, M = NULL,
        pd = myImport$pd, intensity = myImport$intensity,
        Meth = NULL, UnMeth = NULL, detP = myImport$detP,
        beadcount = myImport$beadcount, autoimpute = autoimpute,
        filterDetP = filterDetP, ProbeCutoff = ProbeCutoff,
        SampleCutoff = SampleCutoff, detPcut = detPcut,
        filterBeads = filterBeads, beadCutoff = beadCutoff,
        filterNoCG = filterNoCG, filterSNPs = filterSNPs,
        population = population, filterMultiHit = filterMultiHit,
        filterXY = filterXY, arraytype = arraytype
      )
    } else {
      myLoad <- champ.filter(
        beta = NULL, M = myImport$M,
        pd = myImport$pd, intensity = myImport$intensity,
        Meth = NULL, UnMeth = NULL, detP = myImport$detP,
        beadcount = myImport$beadcount, autoimpute = autoimpute,
        filterDetP = filterDetP, ProbeCutoff = ProbeCutoff,
        SampleCutoff = SampleCutoff, detPcut = detPcut,
        filterBeads = filterBeads, beadCutoff = beadCutoff,
        filterNoCG = filterNoCG, filterSNPs = filterSNPs,
        population = population, filterMultiHit = filterMultiHit,
        filterXY = filterXY, arraytype = arraytype
      )
    }
    message("[<<<<< ChAMP.LOAD END >>>>>>]")
    message("[===========================]")
    message("[You may want to process champ.QC() next.]\n")
    return(myLoad)
  }
}

champ_DMP <- function(
  beta = myNorm, 
  pheno = myLoad$pd$Sample_Group, 
  covariate = NULL,
  compare.group = NULL,
  adjPVal = 0.05, 
  adjust.method = "BH", 
  arraytype = "450K"
) {
  message("[===========================]")
  message("[<<<<< ChAMP.DMP START >>>>>]")
  message("-----------------------------")
  CalculateDMP <- function(
    beta, 
    pheno, 
    covariate, 
    tmp_compare, 
    adjPVal = adjPVal,
    adjust.method = adjust.method
  ) {
    message("  -----------------------------")
    message("  Start to Compare : ", tmp_compare[1], ", ", tmp_compare[2])
    p <- pheno[which(pheno %in% tmp_compare)]
    tmpbeta <- beta[, which(pheno %in% tmp_compare)]
    if (is.null(covariate)) {
      df <- data.frame(pheno = p)
      design <- model.matrix(~0 + pheno, data = df)
    } else {
      tmp_covariate <- covariate[which(pheno %in% tmp_compare), ]
      df <- data.frame(pheno = p, tmp_covariate)
      form <- as.formula(paste(c("~0+pheno", colnames(tmp_covariate)), collapse = "+"))
      design <- model.matrix(form, data = df)
    }
    
    contrast.matrix <- makeContrasts(
      contrasts = paste(colnames(design)[2:1], collapse = "-"), 
      levels = colnames(design)
    )
    message("  Contrast Matrix")
    print(contrast.matrix)
    fit <- lmFit(tmpbeta, design)
    fit2 <- contrasts.fit(fit, contrast.matrix)
    tryCatch(fit3 <- eBayes(fit2), warning = function(w) {
      stop("limma failed, No sample variance.\n")
    })
    DMP <- topTable(
      fit3, 
      coef = 1, 
      number = nrow(tmpbeta), 
      adjust.method = adjust.method, 
      p.value = adjPVal
    )
    message(
      "  You have found ", sum(DMP$adj.P.Val <= adjPVal),
      " significant MVPs with a ", adjust.method, 
      " adjusted P-value below ", adjPVal, "."
    )
    message("  Calculate DMP for ", tmp_compare[1], " and ", tmp_compare[2], " done.")
    return(DMP)
  }
  message("!!! Important !!! New Modification has been made on champ.DMP(): \n")
  message("    (1): In this version champ.DMP() if your pheno parameter contains more than two groups of phenotypes, champ.DMP() would do pairewise differential methylated analysis between each pair of them. But you can also specify compare.group to only do comparasion between any two of them.\n")
  message("    (2): champ.DMP() now support numeric as pheno, and will do linear regression on them. So covariates like age could be inputted in this function. You need to make sure your inputted \"pheno\" parameter is \"numeric\" type.\n")
  Compare <- NULL
  message("--------------------------------")
  
  if (is.null(pheno) | length(unique(pheno)) <= 1) {
    stop("pheno parameter is invalid. Please check the input, pheno MUST contain at least two phenotypes.")
  }
  if (length(pheno) != ncol(beta)) {
    stop("Your Pheno's length is not in accord with your beta value's ncol.")
  }
  
  message("\n[ Section 1:  Check Input Pheno Start ]\n")
  if (class(pheno) == "numeric") {
    message("  You pheno is numeric type.")
    message("    pheno parameter contains :", length(pheno), " values.")
    message("    pheno parameter ranges from ", min(pheno), " to ", max(pheno))
  } else {
    message("  You pheno is ", class(pheno), " type.")
    message("    Your pheno information contains following groups. >>")
    sapply(unique(pheno), function(x) message("    <", x, ">:", sum(pheno == x), " samples."))
    message("    [The power of statistics analysis on groups contain very few samples may not strong.]")
    if (length(unique(pheno)) == 2) {
      message("    pheno contains only 2 phenotypes")
      if (is.null(compare.group)) {
        message("    compare.group parameter is NULL, two pheno types will be added into Compare List.")
        Compare <- list(x1 = unique(pheno))
      } else if (sum(compare.group %in% unique(pheno)) == 2) {
        message("    Your compare.group parameter is in accord with your pheno. Two pheno types has been added into Compare List.")
        Compare <- list(x1 = unique(pheno))
      } else {
        stop(" You have specified compare.group, but it's not in accord with your pheno parameter. Please recheck your compare.group or pheno.")
      }
    } else if (length(unique(pheno)) > 2) {
      message("    pheno contains ", length(unique(pheno)), " phenotypes")
      if (is.null(compare.group)) {
        message("    compare.group parameter is NULL, EACH PAIR of phenotypes will be added into Compare List.")
        Compare <- as.list(data.frame(combn(unique(pheno), 2)))
      } else if (sum(compare.group %in% unique(pheno)) == 2) {
        message("    Your compare.group parameter is in accord with your pheno. Two pheno types has been added into Compare List.")
        Compare <- list(x1 = sort(compare.group))
      } else {
        stop("    Your pheno parameter contains multiple phenotypes, but values in your compare.group parameter are not all find in them. Please recheck your compare.group or pheno.")
      }
    } else {
      stop("    !!! Something wrong with your pheno. Please check your input.")
    }
    tmpnamelist <- vector()
    for (i in 1:length(Compare)) {
      tmpname <- paste(Compare[[i]][1], Compare[[i]][2], sep = "_to_")
      message("    ", tmpname, " compare group : ", Compare[[i]][1], ", ", Compare[[i]][2])
      tmpnamelist <- c(tmpnamelist, tmpname)
    }
    names(Compare) <- tmpnamelist
  }
  message("\n[ Section 1:  Check Input Pheno Done ]\n")

  DMPs <- list()
  if (is.null(Compare)) {
    message("\n[ Section 2:  Find Numeric Covariates Linear Regression CpGs Start ]\n")
    if (is.null(covariate)) {
      df <- data.frame(pheno = pheno)
      model.matrix <- model.matrix(~pheno, data = df)
    } else {
      df <- data.frame(pheno = pheno, covariate)
      form <- as.formula(paste(c("~pheno", colnames(covariate)), collapse = "+"))
      model.matrix <- model.matrix(form, data = df)
    }
    
    fit1 <- lmFit(beta, model.matrix)
    fit2 <- eBayes(fit1)
    DMP <- topTable(fit2,
      coef = 2, number = nrow(beta),
      adjust.method = adjust.method, p.value = adjPVal
    )
    message(
      "  You have found ", sum(DMP$adj.P.Val <= adjPVal),
      " significant MVPs with a ", adjust.method, " adjusted P-value below ",
      adjPVal, "."
    )
    if (sum(DMP$adj.P.Val <= adjPVal) != 0) {
      DMPs[["NumericVariable"]] <- DMP
    }
  } else {
    message("\n[ Section 2:  Find Differential Methylated CpGs Start ]\n")
    for (i in names(Compare)) {
      DMP <- CalculateDMP(beta, pheno, covariate, tmp_compare = Compare[[i]], adjPVal, adjust.method)
      if (sum(DMP$adj.P.Val <= adjPVal) != 0) {
        DMPs[[i]] <- DMP
      }
    }
  }
  message("\n[ Section 2:  Find Numeric Vector Related CpGs Done ]\n")
  if (length(DMPs) == 0) {
    stop("ChAMP.DMP Have not detected even one significant CpGs. You may try other threshold.")
  }
  message("\n[ Section 3:  Match Annotation Start ]\n")
  if (arraytype == "EPIC") {
    data(probe.features.epic)
  } else {
    data(probe.features)
  }
  for (i in names(DMPs)) {
    com.idx <- intersect(rownames(DMPs[[i]]), rownames(probe.features))
    if (!is.null(Compare)) {
      avg <- cbind(
        rowMeans(beta[com.idx, which(pheno == Compare[[i]][1])]), 
        rowMeans(beta[com.idx, which(pheno == Compare[[i]][2])])
      )
      avg <- cbind(avg, avg[, 2] - avg[, 1])
      colnames(avg) <- c(
        paste(Compare[[i]], "AVG", sep = "_"),
        "deltaBeta"
      )
      DMPs[[i]] <- data.frame(
        DMPs[[i]][com.idx, ], 
        avg,
        probe.features[com.idx, ]
      )
    } else {
      DMPs[[i]] <- data.frame(DMPs[[i]][com.idx, ], probe.features[com.idx, ])
    }
  }
  message("\n[ Section 3:  Match Annotation Done ]\n")
  message("[<<<<<< ChAMP.DMP END >>>>>>]")
  message("[===========================]")
  message("[You may want to process DMP.GUI() or champ.GSEA() next.]\n")
  return(DMPs)
}
