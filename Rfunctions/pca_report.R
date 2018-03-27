pca_report <- function (
    data, 
    design, 
    techvars, 
    n.comp = 5, 
    max.comp = ncol(data)-1-n.comp,
    outliers.comp = c(1, 2), 
    outliers.thresh = 3, 
    title.level = 2,
    theme_dark = FALSE
) {
	require(flashpcaR)
	require(scales)
	require(tidyverse)

	pca.res <-flashpca(
	    X = as.matrix(data),
	    method = "eigen",
	    transpose = TRUE,
	    stand = "sd",
	    ndim = n.comp,
	    nextra = max.comp,
	    maxiter = 100
	) 
	pca.dfxy <- pca.res %>% 
	    `[[`("projection") %>%
	    as.data.frame() %>% 
	    `rownames<-`(colnames(data)) %>%
	    `colnames<-`(paste0("PC", seq_len(ncol(.)))) %>% 
	    merge(x = design, y = ., by = "row.names", all.y = TRUE) %>% 
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


	cat(paste0("\n", paste(rep("#", title.level), collapse = ""), " PCA inertia contribution {-}\n"))
	p <- data.frame(y = (pca.res$x$values/sum(pca.res$x$values)), x = seq_along(pca.res$x$values)) %>%
		mutate(cumsum = cumsum(y)) %>%
		ggplot(aes(x = x, y = y)) +
			geom_bar(stat = "identity", width = 1, colour = "white", fill = viridis_pal(option = "viridis", begin = 0.25, end = 0.25)(1)) +
			scale_y_continuous(labels = percent) +
			scale_x_continuous(breaks = seq_len(10), limits = c(0, 11), expand = c(0, 0)) +
			labs(y = "Inertia", x = "PCA Components")
	print(p)
	cat("\n")
	
	cat(paste0("\n", paste(rep("#", title.level), collapse = ""), " PCA  factorial planes {-}\n"))
	for (ivar in techvars) {
		cat(paste0("\n", paste(rep("#", title.level+1), collapse = ""), " ", ivar, " {-}\n"))
		p <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(n.comp)), 2)), 1, function (icoord) {
			tmp <- pca.dfxy[, c(ivar, icoord)]
			tmp[, ivar] <- as.factor(tmp[, ivar])
			colnames(tmp)[-seq_len(ncol(tmp)-2)] <- c("X", "Y")
			tmp[, "X.PC"] <- icoord[1]
			tmp[, "Y.PC"] <- icoord[2]
			return(tmp)
		})) %>%
			ggplot(aes_string(x = "X", y = "Y", colour = ivar)) +
			geom_hline(aes(yintercept = 0), colour = ifelse(theme_dark, "white", "black")) +
			geom_vline(aes(xintercept = 0), colour = ifelse(theme_dark, "white", "black")) +
			geom_point(shape = 4, size = 2) +
			stat_ellipse(type = "norm") +
			scale_colour_viridis(discrete = TRUE) +
			labs(x = NULL, y = NULL) +
			facet_grid(Y.PC~X.PC, scales = "free")
		print(p)
		cat("\n")
	}
	
	cat(paste0("\n", paste(rep("#", title.level), collapse = ""), " PCA association {-}\n"))
	p <- pca.dfxy %>%
		(function (.data) {
			lapply(seq_len(n.comp), function (i) {
				form <- as.formula(paste0("PC", i, " ~ ", paste(paste0("factor(", techvars, ")"), collapse = " + ")))
				lm(form, data = .data) %>% anova %>% rownames_to_column(var = "term") %>% mutate(PC = i)
			}) %>%
			do.call("rbind", .)
		}) %>%
		filter(term!="Residuals") %>%
		mutate(term = gsub("factor\\((.*)\\)", "\\1", term)) %>% 
		ggplot(aes(x = factor(PC), y = term, fill = `Pr(>F)`)) +
			geom_tile(colour = "white") +
			geom_text(aes(label = scientific(`Pr(>F)`, digits = 2)), colour = ifelse(!theme_dark, "white", "black"), size = 3) +
			scale_fill_viridis(name = "P-Value", na.value = ifelse(theme_dark, "grey30", "grey85"), limits = c(0, 0.1)) +
			theme(panel.grid = element_blank()) +
			scale_x_discrete(expand = c(0, 0)) +
			scale_y_discrete(expand = c(0, 0)) +
			labs(x = "PCA Components", y = NULL) 
	print(p)
	cat("\n")
	
	
	cat(paste0("\n", paste(rep("#", title.level), collapse = ""), " PCA Outliers {-}\n"))
	ivar <- "BadSamples"
	p <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(n.comp)), 2)), 1, function (icoord) {
			tmp <- pca.dfxy[, c(ivar, icoord)]
			tmp[, ivar] <- as.factor(tmp[, ivar])
			colnames(tmp)[-seq_len(ncol(tmp)-2)] <- c("X", "Y")
			tmp[, "X.PC"] <- icoord[1]
			tmp[, "Y.PC"] <- icoord[2]
			return(tmp)
		})) %>%
			ggplot(aes_string(x = "X", y = "Y", colour = ivar)) +
        		geom_hline(aes(yintercept = 0), colour = ifelse(theme_dark, "white", "black")) +
        		geom_vline(aes(xintercept = 0), colour = ifelse(theme_dark, "white", "black")) +
        		geom_point(shape = 4, size = 2) +
        		stat_ellipse(type = "norm") +
        		scale_colour_viridis(discrete = TRUE) +
        		labs(x = NULL, y = NULL) +
        		facet_grid(Y.PC~X.PC, scales = "free")
	print(p)
	cat("\n")
	
	pca.dfxy %>% 
		select(-starts_with("PC")) %>% 
		filter(BadSamples=="BAD") %>% 
		return()
}
