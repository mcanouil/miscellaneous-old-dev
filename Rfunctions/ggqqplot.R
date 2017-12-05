# @examples
# ggqqplot()
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pvalue PARAM_DESCRIPTION
#' @param lambdaNames PARAM_DESCRIPTION, Default: NULL
#' @param pt.size PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggqqplot
#' @export 
ggqqplot <- function (pvalue, lambdaNames = NULL, pt.size = 1) {
    if (!(is.matrix(pvalue) | is.data.frame(pvalue))) {
        pvalue <- matrix(pvalue)
    } else {}
    if (is.null(lambdaNames)) {
        if ((any(colnames(pvalue) == "") | is.null(colnames(pvalue)))) {
            lambdaNames <- paste0("lambda", seq_len(ncol(pvalue)))
        } else {
            lambdaNames <- colnames(pvalue)
        }
    } else {}
    res <- do.call("rbind", lapply(seq_len(ncol(pvalue)), function(i) {
        pv <- pvalue[, i]
        X2 <- qnorm(pv/2)^2
        gc <- median(X2, na.rm = TRUE)/qchisq(0.5, df = 1)
        obspval <- sort(pv)
        logobspval <- -(log10(obspval))
        exppval <- seq_along(obspval)
        logexppval <- -(log10((exppval - 0.5)/length(exppval)))
        labnames <- paste0(lambdaNames[i], "=", round(gc, 4))
        tmp <- data.frame(logexppval, logobspval, i, labnames)
        whichInfinite <- apply(tmp, 1, function (iRow) { return(any(is.infinite(as.numeric(iRow[-c(3, 4)])))) })
        return(tmp[!whichInfinite, ])
    }))
    res[, "i"] <- factor(res[, "i"], levels = unique(res[, "i"]))
    res[, "labnames"] <- factor(res[, "labnames"], levels = unique(res[, "labnames"]))
    p <- ggplot(data = res) +
        geom_abline(intercept = 0, slope = 1) +
        geom_point(aes_string(x = "logexppval", y = "logobspval", colour = "labnames", shape = "labnames"), size = pt.size) +
        scale_colour_viridis(name = NULL) +
        scale_shape_discrete(name = NULL, solid = FALSE) +
        labs(x = bquote(Expected -log[10](P[value])), y = bquote(Observed - log[10](P[value])), title = "Q-Q plot")
    axisLim <- range(pretty_breaks()(range(unlist(lapply(seq_len(ncol(pvalue)),
        function (i) {
            range(res[res[, "i"] == i, c(1, 2)])
        }), use.names = FALSE))))
    p <- p + xlim(axisLim) + ylim(axisLim)
    return(p)
}
