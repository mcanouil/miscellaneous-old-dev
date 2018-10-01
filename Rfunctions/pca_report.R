pca_report <- function(
  data,
  design,
  id_var = "Sample_ID",
  technical_vars,
  n_comp = 5,
  fig_n_comp = n_comp,
  outliers_component = NULL,
  outliers_threshold = 3,
  title_level = 2,
  theme_dark = FALSE
) {
  require(flashpcaR)
  require(scales)
  require(tidyverse)
  
  if (!is(design, "data.frame")) {
    design <- as.data.frame(design)
  }
  if (!is(design[, id_var], "character")) {
    design[[id_var]] <- as.character(design[[id_var]])
  }
  
  keep_technical <- sapply(
    X = design[, technical_vars, drop = FALSE], 
    FUN = function(icol) {
      length(unique(icol))>1 & length(unique(icol))!=length(design[, id_var])
    }
  ) %>% 
    which() %>% 
    names()
  
  if (length(setdiff(technical_vars, keep_technical))!=0) {
    variables_excluded <- setdiff(technical_vars, keep_technical)
    message(
      "The following variables have been excluded (null variances or confounding with samples): ", 
        paste(variables_excluded[-length(variables_excluded)], collapse = ", "), 
        " and ", 
        variables_excluded[length(variables_excluded)]
    )
  }

  pca_res <- flashpca(
    X = t(as.matrix(data)),
    stand = "sd",
    ndim = n_comp
  )
  
  pca_dfxy <- pca_res %>%
    `[[`("projection") %>%
    as.data.frame() %>%
    `colnames<-`(paste0("PC", seq_len(ncol(.)))) %>%
    mutate(Sample_ID = as.character(colnames(data))) %>% 
    left_join(x = design, y = ., by = id_var) %>% 
    as.data.frame()


  cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA inertia contribution {-}\n"))
  p <- data_frame(
    y = (pca_res$values / sum(pca_res$values)), 
    x = sprintf("PC%02d", seq_along(pca_res$values))
  ) %>%
    mutate(cumsum = cumsum(y)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_bar(stat = "identity", width = 1, colour = "white", fill = "#3B528BFF") +
    scale_y_continuous(labels = percent) +
    labs(y = "Inertia", x = "PCA Components")
  print(p)
  cat("\n")

  if (length(keep_technical)>0) {
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA factorial planes {- .tabset}\n"))
    for (ivar in keep_technical) {
      cat(paste0("\n", paste(rep("#", title_level + 1), collapse = ""), " ", ivar, " {-}\n"))
      p <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(fig_n_comp)), 2)), 1, function(icoord) {
        tmp <- pca_dfxy[, c(ivar, icoord)]
        tmp[, ivar] <- as.factor(tmp[, ivar])
        colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
        tmp[, "X.PC"] <- icoord[1]
        tmp[, "Y.PC"] <- icoord[2]
        return(tmp)
      })) %>%
        ggplot(aes_string(x = "X", y = "Y", colour = ivar)) +
        geom_hline(aes(yintercept = 0), colour = ifelse(theme_dark, "white", "black")) +
        geom_vline(aes(xintercept = 0), colour = ifelse(theme_dark, "white", "black")) +
        geom_point(shape = 4, size = 2) +
        stat_ellipse(type = "norm") +
        scale_colour_viridis_d() +
        labs(x = NULL, y = NULL) +
        facet_grid(Y.PC ~ X.PC, scales = "free") +
        guides(colour = ifelse(length(unique(pca_dfxy[, ivar])) <= 12, "legend", "none"))
      print(p)
      cat("\n")
    }

    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA association {-}\n"))
    p <- pca_dfxy %>%
      (function(.data) {
        lapply(seq_len(fig_n_comp), function(i) {
          form <- as.formula(paste0("PC", i, " ~ ", paste(keep_technical, collapse = " + ")))
          lm(form, data = .data) %>% 
            anova() %>% 
            rownames_to_column(var = "term") %>% 
            mutate(PC = i)
        }) %>%
          bind_rows()
      }) %>%
      filter(term != "Residuals") %>%
      mutate(term = gsub("factor\\((.*)\\)", "\\1", term)) %>%
      ggplot(aes(x = factor(PC), y = term, fill = `Pr(>F)`)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = scientific(`Pr(>F)`, digits = 2)), colour = "white", size = 3) +
      scale_fill_viridis_c(name = "P-Value", na.value = "grey85", limits = c(0, 0.1)) +
      theme(panel.grid = element_blank()) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(x = "PCA Components", y = NULL)
    print(p)
    cat("\n")
  }


  if (!is.null(outliers_component)) {
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA Outliers {-}\n"))
    
    pca_outliers <- pca_dfxy[, paste0("PC", outliers_component), drop = FALSE] %>%
      `^`(2) %>%
      rowSums() %>%
      sqrt() %>%
      data_frame(EuclideanDistance = .) %>%
      rownames_to_column(var = id_var) %>%
      mutate(
        BadSamplesLogical = 
          EuclideanDistance <= 
            (median(EuclideanDistance) - outliers_threshold * IQR(EuclideanDistance)) |
          EuclideanDistance >= 
            (median(EuclideanDistance) + outliers_threshold * IQR(EuclideanDistance)),
        BadSamples = factor(ifelse(BadSamplesLogical, "BAD", "GOOD"), levels = c("BAD", "GOOD"))
      ) %>%
      column_to_rownames(var = id_var)
  
    pca_dfxy <- merge(
      x = pca_dfxy,
      y = pca_outliers,
      by = "row.names"
    ) %>%
      column_to_rownames(var = "Row.names")
    
    ivar <- "BadSamples"
    
    p <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(n_comp)), 2)), 1, function(icoord) {
      tmp <- pca_dfxy[, c(ivar, icoord)]
      tmp[, ivar] <- as.factor(tmp[, ivar])
      colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
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
      facet_grid(Y.PC ~ X.PC, scales = "free")
    print(p)
    cat("\n")
    
    pca.dfxy %>%
      select(-starts_with("PC")) %>%
      filter(BadSamples == "BAD") %>%
      return()
  }
  return(invisible())
}
