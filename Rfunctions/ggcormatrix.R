#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param method PARAM_DESCRIPTION, Default: c("pearson", "spearman")
#' @param digits PARAM_DESCRIPTION, Default: 2
#' @param limits PARAM_DESCRIPTION, Default: c(-1, 1)
#' @param size PARAM_DESCRIPTION, Default: 4
#' @param theme_dark PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[Hmisc]{rcorr}}
#'  \code{\link[reshape2]{melt}}
#' @rdname ggcormatrix
#' @export 
#' @importFrom Hmisc rcorr
#' @importFrom reshape2 melt
ggcormatrix <- function (data, method = c("pearson", "spearman"), digits = 2, limits = c(-1, 1), size = 4, theme_dark = FALSE) {
    res <- Hmisc::rcorr(as.matrix(data), type = method)
    cormat <- round(res$r, digits = digits)
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <- cormat[hc$order, hc$order]
    cormat[lower.tri(cormat)] <- NA
    melted_cormat <- reshape2::melt(cormat)
    melted_cormat <- na.omit(melted_cormat)
    
    res$P <- res$P[hc$order, hc$order]
    res$P[lower.tri( res$P)] <- NA
    melted_P <- reshape2::melt( res$P)
    melted_P <- na.omit(melted_P)
    
    melted_cormat <- merge(melted_cormat, melted_P, by = c("Var1", "Var2"), all.x = TRUE, suffixes =  c("", ".p"))
    melted_cormat[, "value.p"] <- ifelse(!is.na(melted_cormat[, "value.p"]), format(ifelse(melted_cormat[, "value.p"]==0, 2.2e-16, melted_cormat[, "value.p"]), scientific = -1, digits = 3), NA)
    melted_cormat[, "value.p"] <- ifelse(!is.na(melted_cormat[, "value.p"]), paste0("\np=", melted_cormat[, "value.p"]), "")
    if (method=="spearman") {
        melted_cormat[, "label"] <- paste0("rho=", melted_cormat[, "value"]*100, "%", melted_cormat[, "value.p"])
    } else {
        melted_cormat[, "label"] <- paste0("r=", melted_cormat[, "value"]*100, "%", melted_cormat[, "value.p"])
    }
    
    ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
        geom_tile(colour = "white")+
        scale_fill_viridis(
            limits = limits,
            name = paste0(toupper(substr(method, 1, 1)), substr(method, 2, nchar(method)), "\nCorrelation")
        ) +
        coord_fixed() +
        labs(x = NULL, y = NULL) +
        geom_text(aes(Var2, Var1, label = label), color = ifelse(theme_dark, "white", "white"), size = size) +
        theme(plot.background = element_rect(colour = ifelse(theme_dark, "grey20", "white"), fill = ifelse(theme_dark, "grey20", "white")))
    
     return(ggheatmap)
}
