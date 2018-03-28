options(stringsAsFactors = FALSE)
require(Biobase)
require(Hmisc)
require(cowplot)
require(viridis)
require(flashpcaR)
require(tidyverse)
theme_set(theme_light())

setClass(
  "NanoStringSet",
  contains = "ExpressionSet",
  representation = representation(
    positiveFactor = "numeric",
    negativeFactor = "numeric",
    housekeepingFactor = "numeric",
    positiveControl = "matrix",
    negativeControl = "matrix",
    housekeepingControl = "matrix"
  )
)
### normalization factor positiveFactor
setGeneric("positiveFactor", function(object) standardGeneric("positiveFactor"))
setGeneric("positiveFactor<-", function(object, value) standardGeneric("positiveFactor<-"))
setMethod("positiveFactor", signature(object = "NanoStringSet"), function(object) {
  object@positiveFactor
})
setReplaceMethod(
  "positiveFactor", signature(object = "NanoStringSet", value = "numeric"),
  function(object, value) {
    n <- ncol(exprs(object))
    if (length(value) != n) {
      stop("wrong length for positive factor vector!")
    }
    object@positiveFactor <- value
    object
  }
)
### negativeFactor
setGeneric("negativeFactor", function(object) standardGeneric("negativeFactor"))
setGeneric("negativeFactor<-", function(object, value) standardGeneric("negativeFactor<-"))
setMethod("negativeFactor", signature(object = "NanoStringSet"), function(object) {
  object@negativeFactor
})
setReplaceMethod(
  "negativeFactor", signature(object = "NanoStringSet", value = "numeric"),
  function(object, value) {
    n <- ncol(exprs(object))
    if (length(value) != n) {
      stop("wrong length for negative factor vector!")
    }
    object@negativeFactor <- value
    object
  }
)
### housekeepingFactor
setGeneric("housekeepingFactor", function(object) standardGeneric("housekeepingFactor"))
setGeneric("housekeepingFactor<-", function(object, value) standardGeneric("housekeepingFactor<-"))
setMethod("housekeepingFactor", signature(object = "NanoStringSet"), function(object) {
  object@housekeepingFactor
})
setReplaceMethod(
  "housekeepingFactor", signature(object = "NanoStringSet", value = "numeric"),
  function(object, value) {
    n <- ncol(exprs(object))
    if (length(value) != n) {
      stop("wrong length for housekeeping factor vector!")
    }
    object@housekeepingFactor <- value
    object
  }
)
################## control genes positive control genes
setGeneric("positiveControl", function(object) standardGeneric("positiveControl"))
setGeneric("positiveControl<-", function(object, value) standardGeneric("positiveControl<-"))
setMethod("positiveControl", signature(object = "NanoStringSet"), function(object) {
  object@positiveControl
})
setReplaceMethod(
  "positiveControl", signature(object = "NanoStringSet", value = "matrix"),
  function(object, value) {
    object@positiveControl <- value
    object
  }
)
### negative control genes
setGeneric("negativeControl", function(object) standardGeneric("negativeControl"))
setGeneric("negativeControl<-", function(object, value) standardGeneric("negativeControl<-"))
setMethod("negativeControl", signature(object = "NanoStringSet"), function(object) {
  object@negativeControl
})
setReplaceMethod(
  "negativeControl", signature(object = "NanoStringSet", value = "matrix"),
  function(object, value) {
    object@negativeControl <- value
    object
  }
)
### housekeeping control genes
setGeneric("housekeepingControl", function(object) standardGeneric("housekeepingControl"))
setGeneric("housekeepingControl<-", function(object, value) standardGeneric("housekeepingControl<-"))
setMethod("housekeepingControl", signature(object = "NanoStringSet"), function(object) {
  object@housekeepingControl
})
setReplaceMethod(
  "housekeepingControl", signature(object = "NanoStringSet", value = "matrix"),
  function(object, value) {
    object@housekeepingControl <- value
    object
  }
)
################# read data#####
createNanoStringSet <- function(endogenous, positiveControl, negativeControl, housekeepingControl, designs) {
  ## work on input design matrix. Carefully about the input design.
  if (is(designs, "vector")) {
    designs <- as.data.frame(designs)
    colnames(designs) <- "designs"
  } else if (is(designs, "matrix")) {
    ## multiple factor design still single factor
    if (ncol(designs) == 1) {
      designs <- as.data.frame(designs)
      colnames(designs) <- "designs"
    } else {
      ## input is a data frame
      designs <- as.data.frame(designs)
    }
  }

  rownames(designs) <- colnames(endogenous)


  if (is(designs, "data.frame") || is(designs, "AnnotatedDataFrame")) {
    stopifnot(nrow(designs) == ncol(endogenous))
    designs <- as(designs, "AnnotatedDataFrame")
  }
  res <- new(
    "NanoStringSet", exprs = endogenous, phenoData = designs,
    positiveControl = positiveControl,
    negativeControl = negativeControl,
    housekeepingControl = housekeepingControl
  )

  res
}

createNanoStringSetFromCsv <- function(path, header = TRUE, designs) {

  ## read data from csv file

  data <- read.table(path, header = header, sep = ",")

  if (is.null(data)) {
    stop("There is no counts data")
  }

  selectcol <- !(names(data) %in% c("Code.Class", "Name", "Accession"))

  ## remove NA from data set
  counts <- data[, selectcol]
  counts <- as.matrix(counts)
  id <- which(is.na(rowSums(counts)))
  if (length(id) > 0) {
    data <- data[-id, ]
  }

  code.class <- tolower(data[, c("Code.Class")])
  name <- data[, c("Name")]
  accession <- data[, c("Accession")]

  counts <- data[, selectcol]
  counts <- as.matrix(counts)
  rownames(counts) <- name

  pos.id <- grep("positive", code.class, fixed = TRUE)
  neg.id <- grep("negative", code.class, fixed = TRUE)
  house.id <- grep("housekeeping", code.class, fixed = TRUE)
  spikein.id <- grep("spikein", code.class, fixed = TRUE)

  positive <- counts[pos.id, ]
  negative <- counts[neg.id, ]
  housekeeping <- counts[house.id, ]
  spikein <- counts[spikein.id, ]
  endogenous <- counts[-c(pos.id, neg.id, house.id, spikein.id), ]

  cat(paste(
    "There are", ncol(counts), "samples imported; \n There are ",
    nrow(counts), "genes imported with:"
  ))

  print(table(code.class))

  ## work on input design matrix. Carefully about the input design.
  if (is(designs, "vector")) {
    designs <- as.data.frame(designs)
    colnames(designs) <- "designs"
  } else if (is(designs, "matrix")) {
    ## multiple factor design still single factor
    if (ncol(designs) == 1) {
      designs <- as.data.frame(designs)
      colnames(designs) <- "designs"
    } else {
      ## input is a data frame
      designs <- as.data.frame(designs)
    }
  }

  rownames(designs) <- colnames(counts)

  if (is(designs, "data.frame") || is(designs, "AnnotatedDataFrame")) {
    stopifnot(nrow(designs) == ncol(counts))
    designs <- as(designs, "AnnotatedDataFrame")
  }

  rownames(designs) <- colnames(counts)

  res <- new(
    "NanoStringSet", 
    exprs = endogenous, 
    phenoData = designs,
    positiveControl = positive,
    negativeControl = negative,
    housekeepingControl = housekeeping
  )

  return(res)
}

setHousekeeping <- function(NanoStringData, housekeeping) {
    if (length(housekeeping)!=nrow(housekeepingControl(NanoStringData))) {
        tmp <- housekeepingControl(NanoStringData)
        housekeepingControl(NanoStringData) <- tmp[pmatch(housekeeping, rownames(tmp)), ]
    } else {}
    return(NanoStringData)
}

estNormalizationFactors <- function (NanoStringData, kit = c("PlexSet", "miRNA")) {
    if (!is(NanoStringData, "NanoStringSet")) {
        stop("Input must be an object of NanoStringSet class!")
    }
    pos <- positiveControl(NanoStringData)
    pos[pos <= 0] <- 1
    neg <- negativeControl(NanoStringData)
    
    if (nrow(pos)==6 | kit=="miRNA") {
        if (nrow(pos)!=6 & kit=="miRNA") {
            warning(paste0('With kit=="miRNA", 6 positive controls should be available and ', nrow(pos), ' are provided in "NanoStringData"!'))
        } else {}
        x <- c(128, 32, 8, 2, 0.5, 0.125, rep(0, nrow(neg)))
    } else {
        x <- c(
            rep(32, nrow(pos)), 
            rep(0, nrow(neg))
        )
    }
    
    Y <- rbind(pos, neg)
    n <- ncol(Y)
    lamda_i <- rep(0, n)
    c <- rep(0, n)
    for (i in 1:n) {
        model <- glm(Y[, i] ~ x, family = poisson(link = identity))
        lamda_i[i] <- round(model$coeff[1])
        c[i] <- model$coeff[2]
    }
    c <- c / mean(c)
    names(lamda_i) <- colnames(pos)
    names(c) <- colnames(pos)
    lamda_i[lamda_i == 0] <- 1
    
    positiveFactor(NanoStringData) <- c
    negativeFactor(NanoStringData) <- lamda_i
    
    if (nrow(housekeepingControl(NanoStringData))==0) {
        message(paste0('No housekeeping genes were available. Housekeeping normalisation factors are set to 1.'))
        d <- rep(1, n)
        names(d) <- colnames(pos)
        housekeepingFactor(NanoStringData) <- d
    } else {
        house1 <- sweep(housekeepingControl(NanoStringData), 2, lamda_i, FUN = "-")
        house1[house1 <= 0] <- 1
        house2 <- sweep(house1, 2, c, FUN = "/")
        temp2 <- sweep(house2, 1, rowMeans(house2), FUN = "/")
        d <- apply(temp2, 2, median)
        housekeepingFactor(NanoStringData) <- d
    }
    
    return(NanoStringData)
}

normaliseExprsNanoString <- function (NanoStringData, x = exprs(NanoStringData)) {
    c <- positiveFactor(NanoStringData)[colnames(x)]
    d <- housekeepingFactor(NanoStringData)[colnames(x)]
    k <- c * d
    lamda_i <- negativeFactor(NanoStringData)[colnames(x)]
    Y_n <- sweep(x, 2, lamda_i, FUN = "-")
    Y_nph <- sweep(Y_n, 2, k, FUN = "/")
    Y_nph[Y_nph <= 0] <- 0.1
    return(floor(Y_nph))
}

read_mirna <- function (samplesheetpath, design, verbose = TRUE) {
    formatLog <- function (output) {
        NanoStringData.log <- capture.output({output}) %>% 
            gsub("^ ", "", .) %>% 
            gsub(" $", "", .) %>% 
            gsub(";", ".", .) %>% 
            gsub("code.class", "", .) %>% 
            gsub("  ", " ", .)
        
        NanoStringData.log <- c(
            paste0(NanoStringData.log[1], "  \n"),
            NanoStringData.log[2],
            "\n\n",
            c(paste0(
                "- ",
                capitalize(
                    unlist(
                        strsplit(NanoStringData.log[seq(3, length(NanoStringData.log), 2)], " ")
                    )[unlist(strsplit(NanoStringData.log[seq(3, length(NanoStringData.log), 2)], " "))!=""]
                ),
                ": ",
                unlist(
                    strsplit(NanoStringData.log[seq(4, length(NanoStringData.log), 2)], " ")
                )[unlist(strsplit(NanoStringData.log[seq(4, length(NanoStringData.log), 2)], " "))!=""],
                "  \n"
            ), "\n")
        ) %>% 
            return()
    }
    
    RCC.dataList <- lapply(seq_len(nrow(samplesheetpath)), function (irow) {
        allLines <- readLines(con = samplesheetpath[irow, "Path"])
        block.end <- grep("</", allLines)
        block.start <- setdiff(grep("<", allLines), block.end)
        block.names <- gsub("<(.*)>", "\\1", allLines[block.start])
        block.names <- block.names[-length(block.names)]
        block.data <- lapply(seq_along(block.names), function (i) {
            tmp <- read.delim(
                file = samplesheetpath[irow, "Path"], 
                header = block.names[i]=="Code_Summary", 
                sep = ",", 
                nrows = block.end[i]-block.start[i]-1-(block.names[i]=="Code_Summary"), 
                skip = block.start[i]
            )
            if (block.names[i]=="Code_Summary") {
                colnames(tmp)[4] <- paste(samplesheetpath[irow, "Run"], samplesheetpath[irow, "Col"], sep = "_")
            } else {}
            return(tmp)
        })
        names(block.data) <- block.names
        return(block.data)
    })
    
    RCC.data <- Reduce(
        f = function (x, y) {merge(x = x, y = y, by = c("CodeClass", "Name", "Accession"), all = TRUE)}, 
        x = lapply(RCC.dataList, "[[", 4)
    )
    colnames(RCC.data)[seq_len(3)] <- c("Code.Class", "Name", "Accession")
    
    write.csv(
        x = RCC.data,
        file = paste0(tempdir(), "/NanoString_Full.csv"),
        row.names = FALSE
    )
    NS_log <- formatLog({NanoStringData <- createNanoStringSetFromCsv(
        path = paste0(tempdir(), "/NanoString_Full.csv"),
        header = TRUE,
        designs = design
    )})
    
    if (verbose) {
        NS_log %>% sapply(., cat)
    }
    
    return(NanoStringData = NanoStringData)
}

read_plexset <- function (samplesheetpath, design, verbose = TRUE) {
    formatLog <- function (output) {
        NanoStringData.log <- capture.output({output}) %>% 
            gsub("^ ", "", .) %>% 
            gsub(" $", "", .) %>% 
            gsub(";", ".", .) %>% 
            gsub("code.class", "", .) %>% 
            gsub("  ", " ", .)
        
        NanoStringData.log <- c(
            paste0(NanoStringData.log[1], "  \n"),
            NanoStringData.log[2],
            "\n\n",
            c(paste0(
                "- ",
                capitalize(
                    unlist(
                        strsplit(NanoStringData.log[seq(3, length(NanoStringData.log), 2)], " ")
                    )[unlist(strsplit(NanoStringData.log[seq(3, length(NanoStringData.log), 2)], " "))!=""]
                ),
                ": ",
                unlist(
                    strsplit(NanoStringData.log[seq(4, length(NanoStringData.log), 2)], " ")
                )[unlist(strsplit(NanoStringData.log[seq(4, length(NanoStringData.log), 2)], " "))!=""],
                "  \n"
            ), "\n")
        ) %>% 
            return()
    }
    
    RCC.dataList <- lapply(seq_len(nrow(samplesheetpath)), function (irow) {
        allLines <- readLines(con = samplesheetpath[irow, "Path"])
        block.end <- grep("</", allLines)
        block.start <- setdiff(grep("<", allLines), block.end)
        block.names <- gsub("<(.*)>", "\\1", allLines[block.start])
        block.names <- block.names[-length(block.names)]
        block.data <- lapply(seq_along(block.names), function (i) {
            tmp <- read.delim(
                file = samplesheetpath[irow, "Path"], 
                header = i==4, 
                sep = ",", 
                nrows = block.end[i]-block.start[i]-1-(i==4), 
                skip = block.start[i]
            ) 
            if (i==4) {
                tmp <- tmp %>% 
                    mutate(
                        Plate = samplesheetpath[irow, "Plate"],
                        Row = ifelse(
                            CodeClass%in%paste0("Endogenous", seq_len(8), "s"), 
                            LETTERS[as.numeric(as.factor(gsub("Endogenous(.)s", "\\1", CodeClass)))], 
                            CodeClass
                        ),
                        Col = samplesheetpath[irow, "Col"]
                    ) %>% 
                    `colnames<-`(c("CodeClass", "Name", "Accession", paste(unique(.[, c("Plate", "Col")]), collapse = "_"), "Plate", "Row", "Col"))
                
                tmp %>% 
                    filter(Row%in%LETTERS) %>% 
                    group_by(CodeClass) %>% 
                    nest() %>% 
                    mutate(
                        newdata = map(.x = data, .f = function (x) {
                            out <- bind_rows(
                                tmp %>% filter(!Row%in%LETTERS), 
                                x %>% mutate(CodeClass = "Endogenous")
                            )
                            colnames(out)[4] <- out %>% 
                                filter(CodeClass=="Endogenous") %>% 
                                select(Plate, Row, Col) %>% 
                                distinct() %>% 
                                paste(collapse = "")
                            
                            out %>% 
                                select(-Plate, -Row, -Col) %>% 
                                return()
                        })
                    ) %>% 
                    select(newdata) %>% 
                    `[[`(1) %>% 
                    Reduce(
                        f = function (x, y) {merge(x = x, y = y, by = c("CodeClass", "Name", "Accession"), all = TRUE)}, 
                        x = .
                    ) %>% 
                    return()
            } else {
                return(tmp)
            }
        })
        names(block.data) <- block.names
        return(block.data)
    })
    RCC.data <- Reduce(
        f = function (x, y) {merge(x = x, y = y, by = c("CodeClass", "Name", "Accession"), all = TRUE)}, 
        x = lapply(RCC.dataList, "[[", 4)
    )
    colnames(RCC.data)[seq_len(3)] <- c("Code.Class", "Name", "Accession")
    
    write.csv(
        x = RCC.data,
        file = paste0(tempdir(), "/NanoString_Full.csv"),
        row.names = FALSE
    )
    NS_log <- formatLog({NanoStringData <- createNanoStringSetFromCsv(
        path = paste0(tempdir(), "/NanoString_Full.csv"),
        header = TRUE,
        designs = design
    )})
    
    if (verbose) {
        NS_log %>% sapply(., cat)
    }
    
    return(NanoStringData = NanoStringData)
}

pow_trans <- function(power, n = 5) {
    negpow_breaks <- function(n = 5) {
        function(x, y = power) {
            rng <- range(x, na.rm = TRUE) ^ y
            max <- ceiling(rng[2])
            min <- floor(rng[1])
            signif(seq(min, max, length.out = n) ^ (1 / y), digits = 2)
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
        domain = c(0, Inf),
        format = function(x) {
            scientific(x)
            # parse(text = gsub("e", " %*% 10^", scientific(x)))
        }
    )
}


plotControls <- function (NanoStringData, kit = c("PlexSet", "miRNA"), normalised = TRUE, yScalePower = 1) {
    kit <- kit[1]
    
    if (length(positiveFactor(NanoStringData))==0 | length(negativeFactor(NanoStringData))==0) {
        normalised <- FALSE
    }
    
    if (normalised) {
        ggdata_neg <- normaliseExprsNanoString(NanoStringData = NanoStringData, x = negativeControl(NanoStringData))
        ggdata_pos <- normaliseExprsNanoString(NanoStringData = NanoStringData, x = positiveControl(NanoStringData))
    } else {
        ggdata_neg <- negativeControl(NanoStringData)
        ggdata_pos <- positiveControl(NanoStringData)
    }
    
    ggdata_neg <- ggdata_neg %>% 
        as.data.frame() %>% 
        select(matches(ifelse(kit=="PlexSet", "A", ".*"))) %>% 
        rownames_to_column(var = "G") %>% 
        mutate(
            Control = "Negative",
            G = factor(gsub("NEG_(.)\\(0\\)", "\\1", G) %>% gsub("NEG_", "", .))
        ) %>% 
        gather(.,
               -G,
               -Control,
               key = "IID",
               value = "Value"
        ) %>% 
        mutate(
            ID = as.numeric(factor(IID))
        )
    
    ggdata_pos <- ggdata_pos %>% 
        as.data.frame() %>% 
        select(matches(ifelse(kit=="PlexSet", "A", ".*"))) %>% 
        rownames_to_column(var = "G") %>% 
        mutate(
            Control = "Positive",
            G = factor(gsub("POS_(.)\\(0\\)", "\\1", G) %>% gsub("POS_", "", .))
        ) %>% 
        gather(.,
               -G,
               -Control,
               key = "IID",
               value = "Value"
        ) %>% 
        mutate(
            ID = as.numeric(factor(IID))
        )
    
    ggdata <- rbind(ggdata_neg, ggdata_pos)
    
    if (nrow(housekeepingControl(NanoStringData))!=0) {
        if (normalised) {
            ggdata_hk <- normaliseExprsNanoString(NanoStringData = NanoStringData, x = housekeepingControl(NanoStringData))
        } else {
            ggdata_hk <- housekeepingControl(NanoStringData)
        }
        
        ggdata_hk <- ggdata_hk %>% 
            as.data.frame() %>% 
            select(matches(ifelse(kit=="PlexSet", "A", ".*"))) %>% 
            rownames_to_column(var = "G") %>% 
            mutate(
                Control = "Housekeeping",
                G = factor(G)
            ) %>% 
            gather(.,
                   -G,
                   -Control,
                   key = "IID",
                   value = "Value"
            ) %>% 
            mutate(
                ID = as.numeric(factor(IID))
            )
        
        p <- cowplot::plot_grid(
            ggplot(data = ggdata_neg, aes(x = ID, y = Value, colour = G)) +
                geom_line() +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Negative", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_neg, aes(x = G, y = Value, colour = G)) +
                geom_boxplot(outlier.shape = NA, fill = NA) +
                geom_point(shape = 21, alpha = 0.25, position = position_jitter(width = 0.2)) +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Negative", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_pos, aes(x = ID, y = Value, colour = G)) +
                geom_line() +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Positive", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_pos, aes(x = G, y = Value, colour = G)) +
                geom_boxplot(outlier.shape = NA, fill = NA) +
                geom_point(shape = 21, alpha = 0.25, position = position_jitter(width = 0.2)) +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Positive", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_hk, aes(x = ID, y = Value, colour = G)) +
                geom_line() +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Housekeeping", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_hk, aes(x = G, y = Value, colour = G)) +
                geom_boxplot(outlier.shape = NA, fill = NA) +
                geom_point(shape = 21, alpha = 0.25, position = position_jitter(width = 0.2)) +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Housekeeping", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            nrow = 3,
            ncol = 2,
            labels = LETTERS, 
            align = "hv",
            axis = "tblr"
        )
    } else {
        p <- plot_grid(
            ggplot(data = ggdata_neg, aes(x = ID, y = Value, colour = G)) +
                geom_line() +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Negative", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_pos, aes(x = ID, y = Value, colour = G)) +
                geom_line() +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Positive", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_neg, aes(x = G, y = Value, colour = G)) +
                geom_boxplot(outlier.shape = NA, fill = NA) +
                geom_point(shape = 21, alpha = 0.25, position = position_jitter(width = 0.2)) +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Negative", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            ggplot(data = ggdata_pos, aes(x = G, y = Value, colour = G)) +
                geom_boxplot(outlier.shape = NA, fill = NA) +
                geom_point(shape = 21, alpha = 0.25, position = position_jitter(width = 0.2)) +
                facet_grid(Control~.) +
                scale_colour_viridis(name = "Positive", discrete = TRUE) +
                labs(x = NULL, y = "Count") +
                scale_y_continuous(trans = pow_trans(power = yScalePower)),
            nrow = 2,
            ncol = 2,
            labels = LETTERS, 
            align = "hv"
        )
    }

    return(p)
}

checkOutliers <- function (
    NanoStringData, 
    normalised = TRUE,
    what = c("controls", "counts")[1], 
    outliers.comp = c(1, 2), 
    outliers.thresh = 3, 
    n.comp = 5, 
    max.comp = ncol(exprs(NanoStringData))-1-n.comp
) {
    
    if (length(positiveFactor(NanoStringData))==0 | length(negativeFactor(NanoStringData))==0) {
        normalised <- FALSE
    }
    for (iwhat in what) {
        if (normalised) {
            if (iwhat=="controls") {
                if (nrow(housekeepingControl(NanoStringData))!=0) {
                    data <- rbind(
                        normaliseExprsNanoString(NanoStringData = NanoStringData, x = negativeControl(NanoStringData)),
                        normaliseExprsNanoString(NanoStringData = NanoStringData, x = positiveControl(NanoStringData)),
                        normaliseExprsNanoString(NanoStringData = NanoStringData, x = housekeepingControl(NanoStringData))
                    )
                } else {
                    data <- rbind(
                        normaliseExprsNanoString(NanoStringData = NanoStringData, x = negativeControl(NanoStringData)),
                        normaliseExprsNanoString(NanoStringData = NanoStringData, x = positiveControl(NanoStringData))
                    )
                }
            } else {
                data <- normaliseExprsNanoString(NanoStringData = NanoStringData, x = exprs(NanoStringData))
            }
        } else {
            if (iwhat=="controls") {
                if (nrow(housekeepingControl(NanoStringData))!=0) {
                    data <- rbind(
                        negativeControl(NanoStringData),
                        positiveControl(NanoStringData),
                        housekeepingControl(NanoStringData)
                    )
                } else {
                    data <- rbind(
                        negativeControl(NanoStringData),
                        positiveControl(NanoStringData)
                    )
                }
            } else {
                data <- exprs(NanoStringData)
            }
        }
        
        pca.dfxy <- data %>% 
            as.data.frame() %>% 
            do((function(.data) {
                .data %>% 
                    as.matrix() %>% 
                    flashpca(
                        X = .,
                        method = "eigen",
                        transpose = TRUE,
                        stand = "sd",
                        ndim = n.comp,
                        nextra = max.comp,
                        maxiter = 100
                    ) %>%
                    assign(x = "pca.res", value = ., envir = .GlobalEnv) %>% 
                    `[[`("projection") %>%
                    `rownames<-`(colnames(.data)) %>%
                    `colnames<-`(paste0("PC", seq_len(ncol(.)))) %>% 
                    as.data.frame() %>% 
                    return()
            })(.)) %>% 
            merge(x = design, y = ., by = "row.names", all = TRUE) %>% 
            column_to_rownames(var = "Row.names")
        
        pca.outliers <- pca.dfxy[, paste0("PC", outliers.comp), drop = FALSE] %>% 
            `^`(2) %>% 
            rowSums() %>% 
            sqrt() %>% 
            data.frame(EuclideanDistance = .) %>% 
            rownames_to_column(var = "SampleID") %>% 
            mutate(
                BadSamplesLogical = EuclideanDistance<=(median(EuclideanDistance)-outliers.thresh*IQR(EuclideanDistance)) |
                    EuclideanDistance>=(median(EuclideanDistance)+outliers.thresh*IQR(EuclideanDistance)),
                BadSamples = factor(ifelse(BadSamplesLogical, "BAD", "GOOD"), levels = c("BAD", "GOOD"))
            ) %>% 
            column_to_rownames(var = "SampleID")
        
        pca.dfxy <- merge(
            x = pca.dfxy,
            y = pca.outliers,
            by = "row.names"
        ) %>% 
            column_to_rownames(var = "Row.names")
        
        p <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(n.comp)), 2)), 1, function (icoord) {
            tmp <- pca.dfxy[, c("BadSamples", icoord)]
            tmp[, "BadSamples"] <- as.factor(tmp[, "BadSamples"])
            colnames(tmp)[-seq_len(ncol(tmp)-2)] <- c("X", "Y")
            tmp[, "X.PC"] <- icoord[1]
            tmp[, "Y.PC"] <- icoord[2]
            return(tmp)
        })) %>% 
            ggplot(data = , aes_string(x = "X", y = "Y", colour = "BadSamples")) +
            geom_hline(aes(yintercept = 0)) +
            geom_vline(aes(xintercept = 0)) +
            geom_point(shape = 4, size = 2) +
            stat_ellipse(type = "norm") +
            scale_colour_viridis(discrete = TRUE) +
            labs(x = NULL, y = NULL) +
            facet_grid(Y.PC~X.PC, scales = "free")
        
        if (iwhat=="controls") {
            p_controls <- p
            samples_to_keep_controls <- pca.dfxy %>% 
                filter(BadSamples!="BAD") %>% 
                `[[`("RCCID")
        } else {
            p_counts <- p
            samples_to_keep_counts <- pca.dfxy %>% 
                filter(BadSamples!="BAD") %>% 
                `[[`("RCCID")
        }
    }
    
    if (length(what)>1) {
        samples_to_keep <- intersect(samples_to_keep_controls, samples_to_keep_counts)
        plot_grid(
            p_controls,
            p_counts,
            labels = LETTERS,
            nrow = 2,
            ncol = 1,
            align = "hv"
        ) %>% 
            print()
    } else {
        samples_to_keep <- pca.dfxy %>% 
            filter(BadSamples!="BAD") %>% 
            `[[`("RCCID")
        print(p)
    }
        
    housekeepingControl(NanoStringData) <- housekeepingControl(NanoStringData)[, samples_to_keep]
    positiveControl(NanoStringData) <- positiveControl(NanoStringData)[, samples_to_keep]
    negativeControl(NanoStringData) <- negativeControl(NanoStringData)[, samples_to_keep]
    new_exprs <- new.env()
    assign(x = "exprs", value = exprs(NanoStringData)[, samples_to_keep], envir = new_exprs)
    NanoStringData@assayData <- new_exprs
    
    return(NanoStringData)
}

checkNegativeControls <- function (NanoStringData, kit = c("PlexSet", "miRNA"), k = 1, method = which.min) {
    kit <- kit[1]
    
    negData <- negativeControl(NanoStringData) %>% 
        as.data.frame() %>%
        select(matches(ifelse(kit=="PlexSet", "A", ".*")))

    groups <- negData %>% 
        dist() %>% 
        hclust() %>% 
        cutree(k = k)
    
    selectGroup <- sapply(unique(groups), function (igroup) {
            negData[names(which(groups==igroup)), ] %>% as.matrix() %>% mean()
        }) %>% 
            `names<-`(unique(groups)) %>% 
            method()

    ggdata <- negData %>% 
        dist() %>% 
        hclust() %>% 
        as.dendrogram() %>% 
        gap_data(
            d = ,
            mode = "quantitative",
            mapping = "exponential",
            ratio = 0,
            threshold = 0,
            verbose = FALSE,
            scale = 0.5
        )
    
    boxCoord <- data.frame(
        y = 0,
        yend = median(c(
            ggdata$segments %>% 
                filter((
                    x >= min(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    )) &
                    x <= max(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    ))
                )) %>% 
                summarise(y = max(y)) %>% 
                unlist(),
            ggdata$segments %>% 
                filter(!(
                    x >= min(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    )) &
                    x <= max(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    ))
                )) %>% 
                summarise(y = min(y)) %>% 
                unlist()
        )),
        x = ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist() %>% min() %>% `-`(0.5),
        xend = ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist() %>% max() %>% `+`(0.5)
    )
    
    p <- ggplot() +
        geom_segment(
            data = ggdata$segments, 
            aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
            colour = ifelse(theme_dark, viridis_pal(begin = 0.25, end = 0.75, direction = -1)(1), viridis_pal(begin = 0.25, end = 0.75)(1))
        ) +
        annotate("rect", 
                 xmin = boxCoord[, "x"], 
                 xmax = boxCoord[, "xend"], 
                 ymin = boxCoord[, "y"], 
                 ymax = boxCoord[, "yend"],
                 fill = viridis_pal(begin = 0.5, end = 0.5)(1),
                 alpha = 0.25
        ) +
        scale_x_continuous(
            expand = c(0, 0), 
            limits = c(min(ggdata$labels$x) - 0.5, max(ggdata$labels$x) + 0.5),
            breaks = ggdata$labels$x,
            labels = ggdata$labels$label
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            legend.position = "none"
        ) + 
        labs(x = NULL, y = NULL)
    
    Controls <- as.vector(names(groups)[groups==selectGroup])
    
    return(list(Controls = Controls, Plot = p))
}

checkPositiveControls <- function (NanoStringData, kit = c("PlexSet", "miRNA"), k = 1, method = which.max) {
    kit <- kit[1]
    
    posData <- positiveControl(NanoStringData) %>% 
        as.data.frame() %>%
        select(matches(ifelse(kit=="PlexSet", "A", ".*")))
    
    groups <- posData %>% 
        t() %>% 
        rcorr(type = "spearman") %>% 
        `[[`("r") %>% 
        as.dist() %>% 
        hclust() %>% 
        cutree(k = k, h = 0.95)

    
    selectGroup <- sapply(unique(groups), function (igroup) {
            posData[names(which(groups==igroup)), ] %>% as.matrix() %>% mean()
        }) %>% 
            `names<-`(unique(groups)) %>% 
            method()
    
    ggdata <- posData %>% 
        t() %>% 
        rcorr(type = "spearman") %>% 
        `[[`("r") %>% 
        as.dist() %>% 
        hclust() %>% 
        as.dendrogram() %>% 
        gap_data(
            d = ,
            mode = "quantitative",
            mapping = "exponential",
            ratio = 0,
            threshold = 0,
            verbose = FALSE,
            scale = 0.5
        )
    
    boxCoord <- data.frame(
        y = 0,
        yend = median(c(
            ggdata$segments %>% 
                filter((
                    x >= min(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    )) &
                    x <= max(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    ))
                )) %>% 
                summarise(y = max(y)) %>% 
                unlist(),
            ggdata$segments %>% 
                filter(!(
                    x >= min(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    )) &
                    x <= max(c(
                        ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist()
                    ))
                )) %>% 
                summarise(y = min(y)) %>% 
                unlist()
        )),
        x = ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist() %>% min() %>% `-`(0.5),
        xend = ggdata$labels %>% filter(label%in%names(groups)[groups==selectGroup]) %>% select(x) %>% unlist() %>% max() %>% `+`(0.5)
    )
    
    p <- ggplot() +
        geom_segment(
            data = ggdata$segments, 
            aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
            colour = ifelse(theme_dark, viridis_pal(begin = 0.25, end = 0.75, direction = -1)(1), viridis_pal(begin = 0.25, end = 0.75)(1))
        ) +
        annotate("rect", 
                 xmin = boxCoord[, "x"], 
                 xmax = boxCoord[, "xend"], 
                 ymin = boxCoord[, "y"], 
                 ymax = boxCoord[, "yend"],
                 fill = viridis_pal(begin = 0.5, end = 0.5)(1),
                 alpha = 0.25
        ) +
        scale_x_continuous(
            expand = c(0, 0), 
            limits = c(min(ggdata$labels$x) - 0.5, max(ggdata$labels$x) + 0.5),
            breaks = ggdata$labels$x,
            labels = ggdata$labels$label
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            legend.position = "none"
        ) + 
        labs(x = NULL, y = NULL)
    
    Controls <- as.vector(names(groups)[groups==selectGroup])
    
    return(list(Controls = Controls, Plot = p))
}
