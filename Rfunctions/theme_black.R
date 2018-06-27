#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param plot PARAM_DESCRIPTION, Default: NULL
#' @param xlim PARAM_DESCRIPTION, Default: c(0, 1)
#' @param ylim PARAM_DESCRIPTION, Default: c(0, 1)
#' @param theme_dark PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggdraw
#' @export
ggdraw <- function(plot = NULL, xlim = c(0, 1), ylim = c(0, 1), theme_dark = FALSE) {
  d <- data.frame(x = 0:1, y = 0:1)
  p <- ggplot(d, aes_string(x = "x", y = "y")) +
    scale_x_continuous(limits = xlim, expand = c(0, 0)) +
    scale_y_continuous(limits = ylim, expand = c(0, 0)) +
    theme_nothing() +
    theme(plot.background = element_rect(colour = ifelse(theme_dark, "grey20", "white"), fill = ifelse(theme_dark, "grey20", "white"))) +
    labs(x = NULL, y = NULL)
  if (!is.null(plot)) {
    p <- p + draw_plot(plot)
  }
  p
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param plotlist PARAM_DESCRIPTION, Default: NULL
#' @param align PARAM_DESCRIPTION, Default: c("none", "h", "v", "hv")
#' @param nrow PARAM_DESCRIPTION, Default: NULL
#' @param ncol PARAM_DESCRIPTION, Default: NULL
#' @param scale PARAM_DESCRIPTION, Default: 1
#' @param rel_widths PARAM_DESCRIPTION, Default: 1
#' @param rel_heights PARAM_DESCRIPTION, Default: 1
#' @param labels PARAM_DESCRIPTION, Default: NULL
#' @param label_size PARAM_DESCRIPTION, Default: 14
#' @param hjust PARAM_DESCRIPTION, Default: -0.5
#' @param vjust PARAM_DESCRIPTION, Default: 1.5
#' @param cols PARAM_DESCRIPTION, Default: NULL
#' @param rows PARAM_DESCRIPTION, Default: NULL
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
#'  \code{\link[grid]{grobTree}}
#' @rdname plot_grid
#' @export
#' @importFrom grid grobTree
plot_grid <- function(..., plotlist = NULL, align = c("none", "h", "v", "hv"), nrow = NULL, ncol = NULL, scale = 1, rel_widths = 1, rel_heights = 1, labels = NULL, label_size = 14, hjust = -0.5, vjust = 1.5, cols = NULL, rows = NULL, theme_dark = FALSE) {
  plots <- c(list(...), plotlist)
  num_plots <- length(plots)
  if (!is.null(cols)) {
    warning("Argument 'cols' is deprecated. Use 'ncol' instead.")
  }
  if (!is.null(rows)) {
    warning("Argument 'rows' is deprecated. Use 'nrow' instead.")
  }
  if (!is.null(ncol)) {
    cols <- ncol
  }
  if (!is.null(nrow)) {
    rows <- nrow
  }
  grobs <- align_plots(plotlist = plots, align = align)
  if (is.null(cols) && is.null(rows)) {
    cols <- ceiling(sqrt(num_plots))
    rows <- ceiling(num_plots / cols)
  }
  if (is.null(cols)) {
    cols <- ceiling(num_plots / rows)
  }
  if (is.null(rows)) {
    rows <- ceiling(num_plots / cols)
  }
  if (length(scale) == 1) {
    scale <- rep(scale, num_plots)
  }
  if ("AUTO" %in% labels) {
    labels <- LETTERS[1:num_plots]
  } else if ("auto" %in% labels) {
    labels <- letters[1:num_plots]
  }
  if (length(hjust) == 1) {
    hjust <- rep(hjust, length(labels))
  }
  if (length(vjust) == 1) {
    vjust <- rep(vjust, length(labels))
  }
  rel_heights <- rep(rel_heights, length.out = rows)
  rel_widths <- rep(rel_widths, length.out = cols)
  x_deltas <- rel_widths / sum(rel_widths)
  y_deltas <- rel_heights / sum(rel_heights)
  xs <- cumsum(rel_widths) / sum(rel_widths) - x_deltas
  ys <- 1 - cumsum(rel_heights) / sum(rel_heights)
  p <- ggdraw(theme_dark = theme_dark)
  col_count <- 0
  row_count <- 1
  for (i in 1:(rows * cols)) {
    if (i > num_plots) {
      break
    }
    x_delta <- x_deltas[col_count + 1]
    y_delta <- y_deltas[row_count]
    width <- x_delta * scale[i]
    height <- y_delta * scale[i]
    x_off <- (x_delta - width) / 2
    y_off <- (y_delta - height) / 2
    x <- xs[col_count + 1] + x_off
    y <- ys[row_count] + y_off
    p_next <- grobs[[i]]
    if (!is.null(p_next)) {
      p <- p + draw_grob(grid::grobTree(p_next), x, y, width, height)
    }
    if (i <= length(labels)) {
      p <- p + draw_plot_label(labels[i], x - x_off, y + height - y_off, size = label_size, hjust = hjust[i], vjust = vjust[i], colour = ifelse(theme_dark, "white", "black"))
    }
    col_count <- col_count + 1
    if (col_count >= cols) {
      col_count <- 0
      row_count <- row_count + 1
    }
  }
  p
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param base_size PARAM_DESCRIPTION, Default: 11
#' @param base_family PARAM_DESCRIPTION, Default: ''
#' @param base_line_size PARAM_DESCRIPTION, Default: base_size/22
#' @param base_rect_size PARAM_DESCRIPTION, Default: base_size/22
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname theme_black
#' @export
theme_black <- function(base_size = 11, base_family = "", base_line_size = base_size / 22, base_rect_size = base_size / 22) {
  half_line <- base_size / 2
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(colour = "white", size = base_line_size, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "grey20", colour = "white", size = base_rect_size, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "white", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),

    axis.line = element_blank(), # element_line(colour = "white"),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = rel(0.8), colour = "white"),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = element_line(colour = "white"),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.title.x = element_text(margin = margin(t = half_line), vjust = 1),
    axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0),
    axis.title.y = element_text(angle = 90, margin = margin(r = half_line), vjust = 1),
    axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0),

    legend.background = element_rect(fill = "grey20", colour = NA),
    legend.spacing = unit(0.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = element_rect(fill = "grey20", colour = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(0.4, "cm"),

    panel.background = element_rect(fill = "grey20", colour = NA),
    panel.border = element_rect(fill = NA, colour = "white", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(colour = "grey50"),
    panel.grid.minor = element_line(colour = "grey50", size = rel(0.5)),
    panel.spacing = unit(0.1, "cm"), # unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = element_rect(fill = "grey20", colour = "white"),
    strip.text = element_text(colour = "white", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background = element_rect(colour = "grey20"),
    plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5, vjust = 1, margin = margin(b = half_line * 1.2)),
    plot.subtitle = element_text(size = rel(0.9), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.9)),
    plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9)),
    plot.margin = margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
