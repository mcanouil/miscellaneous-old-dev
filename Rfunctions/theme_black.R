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

print.ggplot <- function (x, newpage = is.null(vp), vp = NULL, ...) {
  if (is.null(x$theme$plot.background$colour)) {
    base_colour <- theme_get()$plot.background$colour
  } else {
    base_colour <- x$theme$plot.background$colour
  }
  set_last_plot(x)
  if (newpage) {
      grid.newpage()
  }
  grid:::grid.rect(
    gp = grid::gpar(
      fill = base_colour, 
      col = base_colour
    )
  )
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE), 
    list(), 
    getNamespace("ggplot2")
  )
  data <- ggplot_build(x)
  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
      grid.draw(gtable)
  } else {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
    grid.draw(gtable)
    upViewport()
  }
  invisible(x)
}
plot.ggplot <- print.ggplot


ggsave <- function(
  filename, 
  plot = ggplot2::last_plot(), 
  device = NULL, 
  path = NULL, 
  scale = 1, 
  width = NA, 
  height = NA, 
  units = c("in", "cm", "mm"), 
  dpi = 300, 
  limitsize = TRUE, 
  ...
) {
  if (is.null(plot$theme$plot.background$colour)) {
    base_colour <- theme_get()$plot.background$colour
  } else {
    base_colour <- plot$theme$plot.background$colour
  }
  
  ggsave_args <- c("plot", "path", "scale", "width", "height", "units", "dpi", "limitsize")
  args <- as.list(match.call())
  args[[1]] <- NULL
  args <- args[stats::na.omit(match(ggsave_args, names(args)))]
  args_dotdotdot <- c(list(...), bg = base_colour)
  if (is.null(device)) {
    device <- tolower(tools::file_ext(filename))
  }
  if (identical(device, "pdf") || identical(device, grDevices::pdf)) {
    if (!"useDingbats" %in% names(args_dotdotdot)) 
      args_dotdotdot <- append(args_dotdotdot, list(useDingbats = FALSE))
  }
  args <- c(list(filename = filename), args, device = device, args_dotdotdot)
  cur_dev <- grDevices::dev.cur()
  x <- do.call(ggplot2::ggsave, args, envir = parent.frame())
  grDevices::dev.set(cur_dev)
  invisible(x)
}
  
