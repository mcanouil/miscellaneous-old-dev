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
    text = ggplot2::element_text(family = base_family, face = "plain", colour = base_colours[3], size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),

    axis.line = ggplot2::element_blank(), 
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(size = rel(0.8), colour = base_colours[3]),
    axis.text.x = ggplot2::element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y = ggplot2::element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = base_colours[3]),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.title.x = ggplot2::element_text(margin = margin(t = half_line), vjust = 1),
    axis.title.x.top = ggplot2::element_text(margin = margin(b = half_line), vjust = 0),
    axis.title.y = ggplot2::element_text(angle = 90, margin = margin(r = half_line), vjust = 1),
    axis.title.y.right = ggplot2::element_text(angle = -90, margin = margin(l = half_line), vjust = 0),

    legend.background = ggplot2::element_rect(fill = base_colours[1], colour = NA),
    legend.spacing = ggplot2::unit(0.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = ggplot2::element_rect(fill = base_colours[1], colour = base_colours[3]),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(0.4, "cm"),

    panel.background = ggplot2::element_rect(fill = base_colours[1], colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = base_colours[3], size = 0.5, linetype = "solid"),
    panel.grid = ggplot2::element_line(colour = base_colours[2]),
    panel.grid.major = ggplot2::element_line(colour = base_colours[2]),
    panel.grid.minor = ggplot2::element_line(colour = base_colours[2], size = rel(0.5)),
    panel.spacing = ggplot2::unit(0.1, "cm"), # ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = ggplot2::element_rect(fill = base_colours[1], colour = base_colours[3]),
    strip.text = ggplot2::element_text(colour = base_colours[3], size = rel(0.8)),
    strip.text.x = ggplot2::element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = ggplot2::element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(0.1, "cm"),
    strip.switch.pad.wrap = ggplot2::unit(0.1, "cm"),

    plot.background = ggplot2::element_rect(colour = base_colours[1]),
    plot.title = ggplot2::element_text(size = rel(1.2), face = "bold", hjust = 0.5, vjust = 1, margin = margin(b = half_line * 1.2)),
    plot.subtitle = ggplot2::element_text(size = rel(0.9), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.9)),
    plot.caption = ggplot2::element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9)),
    plot.margin = margin(half_line, half_line, half_line, half_line),
    plot.tag = ggplot2::element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5), 
    plot.tag.position = "topleft",

    complete = TRUE
  )
}

print.ggplot <- function (x, newpage = is.null(vp), vp = NULL, ...) {
  if (is.null(x$theme$plot.background$colour)) {
    base_colour <- ggplot2::theme_get()$plot.background$colour
  } else {
    base_colour <- x$theme$plot.background$colour
  }
  ggplot2::ggplot_build(x)
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
    base_colour <- ggplot2::theme_get()$plot.background$colour
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
    if (!"useDingbats" %in% names(args_dotdotdot)) {
      args_dotdotdot <- append(args_dotdotdot, list(useDingbats = FALSE))
    }
  }
  args <- c(list(filename = filename), args, device = device, args_dotdotdot)
  cur_dev <- grDevices::dev.cur()
  x <- do.call(ggplot2::ggsave, args, envir = parent.frame())
  grDevices::dev.set(cur_dev)
  return(invisible(x))
}

theme_set <- function (new) {
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
  
  scale_parameters <- switch(
    EXPR = as.character(
      findInterval(
        x = compute_brightness(new$plot.background$colour), 
        vec = c(25, 75)
      )
    ),
    "0" = {list(option = "viridis", begin = 2/5, end = 1, direction = -1)},
    "1" = {list(option = "viridis", begin = 0, end = 1, direction = 1)},
    "2" = {list(option = "viridis", begin = 0, end = 4/5, direction = 1)}
  )
  
  # tibble(
  #   f_names = c(
  #     "scale_colour_viridis_c", "scale_colour_viridis_d", "scale_color_viridis_c", 
  #     "scale_color_viridis_d", "scale_fill_viridis_c", "scale_fill_viridis_d"
  #   ),
  #   f_internal = map(.x = f_names, .f = ~eval(parse(text = paste0("ggplot2::", .x)))),
  #   f_modified = map(
  #     .x = f_internal, 
  #     .f = hijack, 
  #     option = scale_parameters[["option"]],
  #     begin = scale_parameters[["begin"]],
  #     end = scale_parameters[["end"]],
  #     direction = scale_parameters[["direction"]]
  #   )
  # ) %>% 
  #   mutate(
  #     f_assign = map2(.x = f_names, .y = f_modified, .f = function(x, y) {
  #       assign(x = x, value = y, envir = .GlobalEnv)
  #    })
  #   ) %>% 
  #   invisible()
  
  functions_to_hijack <- c(
    "scale_colour_viridis_c", "scale_colour_viridis_d", "scale_color_viridis_c", 
    "scale_color_viridis_d", "scale_fill_viridis_c", "scale_fill_viridis_d"
  )
  
  invisble(lapply(X = functions_to_hijack, FUN = function(x) {
    y <- hijack(
      FUN = eval(parse(text = paste0("ggplot2::", x))), 
      option = scale_parameters[["option"]],
      begin = scale_parameters[["begin"]],
      end = scale_parameters[["end"]],
      direction = scale_parameters[["direction"]]
    )
    assign(x = x, value = y, envir = .GlobalEnv)
  }))
  
  return(ggplot2::theme_set(new))
}
