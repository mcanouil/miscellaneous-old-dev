require(grid)
require(grDevices)
require(ggplot2)
require(scales)

theme_black <- function(
  base_size = 11, 
  base_family = "", 
  base_line_size = base_size / 22, 
  base_rect_size = base_size / 22
) {
  half_line <- base_size / 2
  base_colours <- c("grey20", "grey50", "white")
  ggplot2::theme(
    line = ggplot2::element_line(colour = base_colours[3], size = base_line_size, linetype = 1, lineend = "butt"),
    rect = ggplot2::element_rect(fill = base_colours[1], colour = base_colours[3], size = base_rect_size, linetype = 1),
    text = ggplot2::element_text(family = base_family, face = "plain", colour = base_colours[3], size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(), debug = FALSE),

    axis.line = ggplot2::element_blank(), 
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = base_colours[3]),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = base_colours[3]),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line), vjust = 1),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line), vjust = 0),
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = half_line), vjust = 1),
    axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line), vjust = 0),

    legend.background = ggplot2::element_rect(fill = base_colours[1], colour = NA),
    legend.spacing = ggplot2::unit(0.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = ggplot2::element_rect(fill = base_colours[1], colour = base_colours[3]),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(0.4, "cm"),

    panel.background = ggplot2::element_rect(fill = base_colours[1], colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = base_colours[3], size = 0.5, linetype = "solid"),
    panel.grid = ggplot2::element_line(colour = base_colours[2]),
    panel.grid.major = ggplot2::element_line(colour = base_colours[2]),
    panel.grid.minor = ggplot2::element_line(colour = base_colours[2], size = ggplot2::rel(0.5)),
    panel.spacing = ggplot2::unit(0.1, "cm"), # ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = ggplot2::element_rect(fill = base_colours[1], colour = base_colours[3]),
    strip.text = ggplot2::element_text(colour = base_colours[3], size = ggplot2::rel(0.8)),
    strip.text.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line, r = half_line)),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(0.1, "cm"),
    strip.switch.pad.wrap = ggplot2::unit(0.1, "cm"),

    plot.background = ggplot2::element_rect(colour = base_colours[1]),
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), face = "bold", hjust = 0.5, vjust = 1, margin = ggplot2::margin(b = half_line * 1.2)),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.9), hjust = 0.5, vjust = 1, margin = ggplot2::margin(b = half_line * 0.9)),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(0.9), hjust = 1, vjust = 1, margin = ggplot2::margin(t = half_line * 0.9)),
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5, vjust = 0.5), 
    plot.tag.position = "topleft",

    complete = TRUE
  )
}

dark_mode <- function(.theme) {
  hijack <- function(FUN, ...) {
    .FUN <- FUN
    args <- list(...)
    invisible(lapply(seq_along(args), function(i) {
      formals(.FUN)[[names(args)[i]]] <<- args[[i]]
    }))
    return(.FUN)
  }
  
  compute_brightness <- function(colour) {
    (
      (sum(range(grDevices::col2rgb(colour)))) * 100 * 0.5
    ) / 255
  }
  
  stopifnot(is.theme(.theme))
  geom_names <- apropos("^Geom", ignore.case = FALSE)
  geoms <- list()
  namespaces <- loadedNamespaces()
  for (namespace in namespaces) {
    geoms_in_namespace <- mget(geom_names, envir = asNamespace(namespace), ifnotfound = list(NULL))
    for (geom_name in geom_names) {
      if (is.ggproto(geoms_in_namespace[[geom_name]])) {
        geoms[[geom_name]] <- geoms_in_namespace[[geom_name]]
      }
    }
  }
  for (geom in geoms) {
    stopifnot(is.ggproto(geom))
    if (!is.null(geom$default_aes$fill)) {
      geom$default_aes$fill <- c("white", "black")[(compute_brightness(.theme$plot.background$colour)>50)+1]
    }
    if (!is.null(geom$default_aes$colour)) {
      geom$default_aes$colour <- c("white", "black")[(compute_brightness(.theme$plot.background$colour)>50)+1]
    }
  }
  scale_parameters <- switch(
    EXPR = as.character(
      findInterval(
        x = compute_brightness(.theme$plot.background$colour), 
        vec = c(25, 75)
      )
    ),
    "0" = {list(begin = 2/5, end = 1, direction = -1)},
    "1" = {list(begin = 0, end = 1, direction = 1)},
    "2" = {list(begin = 0, end = 4/5, direction = 1)}
  )
  viridis_names <- apropos("viridis_", ignore.case = FALSE)
  vscales <- list()
  namespaces <- loadedNamespaces()
  for (namespace in namespaces) {
    viridis_in_namespace <- mget(viridis_names, envir = asNamespace(namespace), ifnotfound = list(NULL))
    for (viridis_name in viridis_names) {
      if (is.function(viridis_in_namespace[[viridis_name]])) {
        vscales[[viridis_name]] <- hijack(
          FUN =  viridis_in_namespace[[viridis_name]], 
          begin = scale_parameters[["begin"]],
          end = scale_parameters[["end"]],
          direction = scale_parameters[["direction"]]
        )
        assign(x = viridis_name, value = vscales[[viridis_name]], envir = .GlobalEnv)
      }
    }
  }

  return(invisible(.theme))
}

plot.ggplot <- print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (is.null(x$theme$plot.background$colour)) {
    base_colour <- ggplot2::theme_get()$plot.background$colour
    .theme <- ggplot2::theme_get()
  } else {
    base_colour <- x$theme$plot.background$colour
    .theme <- x$theme
  }
  dark_mode(.theme = .theme)

  ggplot2::set_last_plot(x)
  if (newpage) {
    grid::grid.newpage()
  }
  grid::grid.rect(gp = grid::gpar(fill = base_colour, col = base_colour))
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )
  data <- ggplot2::ggplot_build(x)
  gtable <- ggplot2::ggplot_gtable(data)
  if (is.null(vp)) {
    grid::grid.draw(gtable)
  } else {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
    }
    grid::grid.draw(gtable)
    grid::upViewport()
  }
  return(invisible(x))
}

ggsave <- function(
  filename, 
  plot = last_plot(), 
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
    base_colour <- ggplot2::theme_get()$plot.background$colour
    .theme <- ggplot2::theme_get()
  } else {
    base_colour <- plot$theme$plot.background$colour
    .theme <- plot$theme
  }
  dark_mode(.theme = .theme)
  ggplot2::ggsave(
    filename = filename,  
    plot = plot, 
    device = device, 
    path = path, 
    scale = scale, 
    width = width, 
    height = height, 
    units = units, 
    dpi = dpi, 
    limitsize = limitsize,
    bg = base_colour,
    ...
  )
}

theme_set <- function (new) {
  return(ggplot2::theme_set(dark_mode(.theme = new)))
}
