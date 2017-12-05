library(ellipse)
library(flashpcaR)
library(ggplot2)
library(cowplot)
library(viridis) # color blind proof palette


data(deug, package = "ade4") # Fake data
data <- t(deug$tab) # Samples in columns
Group <- seq_len(ncol(data))<ncol(data)/2 # grouping factor, same length as the number of columns in data
pcs <- c(1, 3) # Principal components to plot


pca.res <- flashpca(
    X = data,
    method = "eigen",
    transpose = TRUE,
    stand = "sd",
    ndim = ncol(data)-1,
    maxiter = 100
)
pca.inertia <- data.frame(y = (pca.res$values/sum(pca.res$values))*100, x = seq_along(pca.res$values))
pca.inertia[, "cumsum"] <- cumsum(pca.inertia[, 1])
pca.dfxy <- cbind.data.frame(as.data.frame(pca.res$projection), Group = Group)
rownames(pca.dfxy) <- colnames(data)
pcs.format <- paste0("V", pcs)
pca.ellipse <- do.call("rbind", lapply(unique(pca.dfxy[, "Group"]), function (g) {
    cbind(as.data.frame(with(
            pca.dfxy[pca.dfxy[, "Group"]==g, ],
            ellipse(
                cor(get(pcs.format[1]), get(pcs.format[2])),
                scale = c(sd(get(pcs.format[1])), sd(get(pcs.format[2])))/3,
                centre = c(mean(get(pcs.format[1])), mean(get(pcs.format[2])))
            )
        )),
        class = g
    )
}))
pca.ellipse[, "Group"] <- pca.ellipse[, "class"]

## Plot PCA
p1 <- ggplot(data = pca.dfxy) +
    theme_light(base_size = 13) +
    geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) +
    geom_point(aes_string(x = pcs.format[1], y = pcs.format[2], colour = "Group"), shape = 4, size = 2) +
    scale_fill_viridis(name = "Group", discrete = TRUE, option = "viridis", begin = 0.05, end = 0.90) +
    geom_path(data = pca.ellipse, aes(x = x, y = y, colour = Group)) +
    scale_colour_manual(name = "Group", values = viridis_pal(option = "viridis", begin = 0.05, end = 0.90)(length(unique(pca.ellipse[, "Group"])))) +
    labs(x = paste0("PC-", pcs[1]), y = paste0("PC-", pcs[2]), title = "PCA")

ggsave(file = "Picture1.png", plot = p1, width = 7.5, height = 6, units = "in", dpi = 300)

## Plot PC contribution
p2 <- ggplot(data = pca.inertia, aes(x = x, y = y)) +
    theme_light(base_size = 13) +
    geom_bar(stat = "identity", width = 0.90, colour = viridis_pal(option = "viridis", begin = 0.25, end = 0.25)(1), fill = viridis_pal(option = "viridis", begin = 0.25, end = 0.25)(1)) +
    labs(y = "Inertia (%)", x = "PCA Components", title = "PC Contribution") +
    scale_x_continuous(expand = c(0, 0.10)) +
    scale_y_continuous(expand = c(0, 0.25))
ggsave(file = "Picture2.png", plot = p2, width = 7.5, height = 6, units = "in", dpi = 300)

##
ggsave(file = "Picture1-2.png", plot = plot_grid(p1, p2, ncol = 2), width = 12, height = 6, units = "in", dpi = 300)

