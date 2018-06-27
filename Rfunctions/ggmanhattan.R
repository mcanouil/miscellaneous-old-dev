# @examples
# ggmanhattan()
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param chr PARAM_DESCRIPTION
#' @param position PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION, Default: 'Manhattan plot'
#' @param xlab PARAM_DESCRIPTION, Default: 'Chromosomes'
#' @param ylab PARAM_DESCRIPTION, Default: 'P-Value'
#' @param sep PARAM_DESCRIPTION, Default: 0.02
#' @param ytrans PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggmanhattan
#' @export
ggmanhattan <- function(data, chr, position, y, title = "Manhattan plot", xlab = "Chromosomes", ylab = "P-Value", sep = 0.02, ytrans = TRUE) {
  data[, chr] <- gsub("chr", "", tolower(data[, chr]))
  data[, chr] <- factor(toupper(data[, chr]), levels = c(seq(22), "X", "Y"))
  data <- data[order(data[, chr], data[, position]), ]
  notNA <- apply(data[, c(chr, position)], 1, function(iRow) {
    any(is.na(iRow))
  })
  data <- data[which(!notNA), ]
  chrSize <- table(data[, chr])
  if (length(chrSize) != 24 | any(chrSize == 0)) {
    CHR <- c(seq(22), "X", "Y")
    equalZero <- names(which(chrSize == 0))
    notIn <- CHR[which(!CHR %in% names(chrSize))]
    chr2Add <- unique(c(equalZero, notIn))
    newLines <- data.frame(do.call("rbind", lapply(chr2Add, function(iChr) {
      newLine <- matrix(as.numeric(rep(NA, ncol(data))), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
      newLine <- data.frame(newLine)
      newLine[1, chr] <- iChr
      return(newLine)
    })))
    colnames(newLines) <- colnames(data)
    data <- rbind(data, newLines)
    data <- data[order(data[, chr], data[, position]), ]
  } else {}
  chrSize <- chrSize[chrSize != 0]
  data <- data[!is.na(data[, y]), ]
  chrSizeNew <- table(data[, chr])
  chrSizeNew <- chrSizeNew[chrSizeNew != 0]
  chrStep <- floor(sum(chrSizeNew) * sep)
  data[, "xPos"] <- unlist(sapply(seq_along(chrSizeNew), function(iSize) {
    if (chrSizeNew[iSize] != 0) {
      xPos <- seq_len(chrSizeNew[iSize])
      range(xPos)
      if (iSize > 1) {
        xPos <- xPos + sum(chrSizeNew[seq(iSize - 1)]) + chrStep * (iSize - 1)
        range(xPos)
      } else {}
      return(xPos)
    } else {}
  }), use.names = FALSE)
  avoidZero <- rep(0, length(chrSize))
  avoidZero[which(chrSize == 0)] <- chrStep
  whichIsCenter <- ceiling(c(cumsum(chrSizeNew) - diff(c(0, cumsum(chrSizeNew))) / 2))
  xBreaks <- data[whichIsCenter, "xPos"]

  pval_trans <- function() {
    neglog10_breaks <- function(n = 5) {
      function(x) {
        rng <- -log(range(x, na.rm = TRUE), base = 10)
        min <- ceiling(rng[2])
        max <- floor(rng[1])
        if (max == min) {
          return(10^-min)
        } else {}
        by <- floor((max - min) / n) + 1
        10^-seq(min, max, by = by)
      }
    }
    trans_new(name = "pval", transform = function(x) {
      -log(x, 10)
    }, inverse = function(x) {
      10^-x
    }, breaks = neglog10_breaks(), domain = c(1e-100, Inf))
  }

  p <- ggplot(data = data, aes_string(x = "xPos", y = y, colour = chr)) +
    geom_point(size = 1.5, shape = 1, na.rm = TRUE) +
    scale_x_continuous(
      breaks = xBreaks,
      labels = names(chrSize),
      limits = c(min(data[, "xPos"]), max(data[, "xPos"]) + sum(avoidZero)),
      expand = c(0.01, 0.01)
    ) +
    labs(title = title, y = ylab, x = xlab) +
    scale_colour_viridis(discrete = TRUE)
  if (ytrans) {
    p <- p +
      expand_limits(y = 10^-(1.05 * -log10(data[, y]))) +
      scale_y_continuous(trans = pval_trans(), expand = c(0, 0))
  } else {
    p <- p +
      scale_y_continuous()
  }

  return(p)
}
