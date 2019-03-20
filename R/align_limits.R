#' align_limits
#'
#' @param ... [ggplot] list of plots to be arranged into the grid. 
#'   The plots can be either ggplot2 plot objects or arbitrary gtables.
#' @param plotlist [list] (optional) list of plots to display.
#'
#' @return return a a ggplot or a list of ggplot
#' @example 
#' p1 <- ggplot2::ggplot(
#'   data = dplyr::filter(datasets::iris, Species=="setosa"),
#'   mapping = ggplot2::aes(x = Sepal.Length, y = Sepal.Width)
#' ) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "setosa")
#' p2 <- ggplot2::ggplot(
#'   data = dplyr::filter(datasets::iris, Species=="versicolor"),
#'   mapping = ggplot2::aes(x = Sepal.Length, y = Sepal.Width)
#' ) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "versicolor")
#' p3 <- ggplot2::ggplot(
#'   data = dplyr::filter(datasets::iris, Species=="virginica"),
#'   mapping = ggplot2::aes(x = Sepal.Length, y = Sepal.Width)
#' ) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "virginica")
#' 
#' ggpubr::ggarrange(
#'   plotlist = list(p1, p2, p3), 
#'   ncol = 3
#' )
#' 
#' ggpubr::ggarrange(
#'   plotlist = align_limits(p1, p2, p3), 
#'   ncol = 3
#' )
align_limits <- function(..., plotlist = NULL) {
  plots <- c(list(...), plotlist)
  
  limits_x <- range(sapply(plots, function(x) {
    ggplot2::ggplot_build(x)$layout$panel_scales_x[[1]]$range$range
  }))
  
  limits_y <- range(sapply(plots, function(x) {
    ggplot2::ggplot_build(x)$layout$panel_scales_y[[1]]$range$range
  }))
  
  if (length(plots)>1) {
    lapply(X = plots, xlim = limits_x, ylim = limits_y, FUN = function(x, xlim, ylim) {
      x + 
        ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
    })
  } else {
    plots + 
      ggplot2::coord_cartesian(xlim = limits_x, ylim = limits_y)
  }
}
