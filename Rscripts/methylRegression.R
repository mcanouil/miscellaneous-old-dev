methylRegression <- function (
  betamatrix, phenotype, gcase = "G1", gcontrol = "G3", concov = "OFF", padj = "bonferroni", nCPU = 70
) {
  grouplev <- phenotype[colnames(betamatrix), "Sample_Group"]
  cat("Performing limma regression....\n")
  lev1 <- which(grouplev %in% gcase)
  lev2 <- which(grouplev %in% gcontrol)
  eset <- betamatrix[, c(lev1, lev2)]
  TS <- as.factor(c(rep("T", length(lev1)), rep("C", length(lev2))))
  SS <- rep(seq_along(lev1), 2)
  design <- model.matrix(~0 + TS)
  rownames(design) <- colnames(eset)
  colnames(design) <- c("C", "T")
  fit <- lmFit(eset, design)
  cont.matrix <- makeContrasts(comp = T - C, levels = design)
  fit2 <- contrasts.fit(fit, cont.matrix)
  fit2 <- eBayes(fit2)
  result1 <- topTable(fit2, coef = 1, adjust.method = padj, number = nrow(fit2))
  testout <- result1[match(rownames(eset), rownames(result1)), "P.Value"]
  adjustP <- p.adjust(testout, method = padj)
  difb <- apply(eset, 1, function (x) {
    mean(x[seq_along(lev1)]) - mean(x[(length(lev1) + 1):ncol(eset)])
  })
  out <- cbind(testout, adjustP, difb, rowMeans(eset[, seq_along(lev1)]), rowMeans(eset[, (length(lev1) + 1):ncol(eset)]))
  rownames(out) <- rownames(eset)
  colnames(out) <- c(
    "P-Value", "Adjust Pval", "Beta-Difference",
    paste("Mean", paste(gcase, collapse = "_"), sep = "_"),
    paste("Mean", paste(gcontrol, collapse = "_"), sep = "_")
  )
  return(out)
}
