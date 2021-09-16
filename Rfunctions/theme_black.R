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
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(colour = base_colours[3], size = base_line_size, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = base_colours[1], colour = base_colours[3], size = base_rect_size, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = base_colours[3], size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),

    axis.line = element_blank(), # element_line(colour = base_colours[3]),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = rel(0.8), colour = base_colours[3]),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = element_line(colour = base_colours[3]),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.title.x = element_text(margin = margin(t = half_line), vjust = 1),
    axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0),
    axis.title.y = element_text(angle = 90, margin = margin(r = half_line), vjust = 1),
    axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0),

    legend.background = element_rect(fill = base_colours[1], colour = NA),
    legend.spacing = unit(0.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = element_rect(fill = base_colours[1], colour = base_colours[3]),
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

    panel.background = element_rect(fill = base_colours[1], colour = NA),
    panel.border = element_rect(fill = NA, colour = base_colours[3], size = 0.5, linetype = "solid"),
    panel.grid = element_line(colour = base_colours[2]),
    panel.grid.major = element_line(colour = base_colours[2]),
    panel.grid.minor = element_line(colour = base_colours[2], size = rel(0.5)),
    panel.spacing = unit(0.1, "cm"), # unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = element_rect(fill = base_colours[1], colour = base_colours[3]),
    strip.text = element_text(colour = base_colours[3], size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background = element_rect(colour = base_colours[1]),
    plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5, vjust = 1, margin = margin(b = half_line * 1.2)),
    plot.subtitle = element_text(size = rel(0.9), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.9)),
    plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9)),
    plot.margin = margin(half_line, half_line, half_line, half_line),
    plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
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
<<<<<<< HEAD
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
=======
  ggplot2::ggplot_build(x)
  gtable <- ggplot2::ggplot_gtable(data)
>>>>>>> 4111ac3 (add prefix package; small changes)
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

gganimate <- function(
  p = last_plot(),
  filename = NULL,
  saver = NULL,
  title_frame = TRUE,
  ...
) {
  if (is.null(p)) {
    stop("no plot to animate")
  }
  gganimate_save <- function(g, filename = NULL, saver = NULL, fps = 1, loop = 0, ...) {
    plot_ggplot_build <- function (b, newpage = is.null(vp), vp = NULL) {
      if (is.null(b$theme$plot.background$colour)) {
        base_colour <- ggplot2::theme_get()$plot.background$colour
      } else {
        base_colour <- b$theme$plot.background$colour
      }
      ggplot2::set_last_plot(b)
      if (newpage) {
        grid::grid.newpage()
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
      gtable <- ggplot2::ggplot_gtable(b)
      if (is.null(vp)) {
        grid::grid.draw(gtable)
      } else {
        if (is.character(vp)) {
          grid::seekViewport(vp)
        } else grid::pushViewport(vp)
        grid::grid.draw(gtable)
        grid::upViewport()
      }
    }
    if (is.null(filename)) {
      if (is.null(saver)) {
        filename <- gganimate:::gganimate_tempfile(fileext = ".gif")
      } else {
        filename <- gganimate:::gganimate_tempfile(fileext = paste0(
          ".",
          saver
        ))
      }
    }
    s <- gganimate:::animation_saver(saver, filename)
    withr::with_dir(dirname(filename), {
      if (s$saver == "gif" && FALSE) {
        gganimate:::save_gganimate_custom(g, filename = filename, ...)
      } else {
        s$func(for (pl in g$plots) {
          plot_ggplot_build(pl)
        }, basename(filename), autobrowse = FALSE, ...)
      }
    })
    g$filename <- filename
    if (!is.null(s$mime_type)) {
      g$src <- base64enc::dataURI(file = filename, mime = s$mime_type)
      g$mime_type <- s$mime_type
    }
    g$saved <- TRUE
    return(g)
  }

  built <- ggplot2::ggplot_build(p)
  frames <- plyr::compact(lapply(built$data, `[[`, "frame"))
  if (length(frames) == 0) {
    stop("No frame aesthetic found; cannot create animation")
  }
  if (is.factor(frames[[1]])) {
    frames <- sort(unique(unlist(frames)))
  } else {
    frames <- sort(unique(do.call(c, frames)))
  }
  frames <- sort(unique(frames))
  plots <- lapply(frames, function(f) {
    b <- built
    for (i in seq_along(b$data)) {
      frame_vec <- b$data[[i]]$frame
      if (!is.null(frame_vec)) {
        sub <- (frame_vec == f | is.na(frame_vec))
        if (!is.null(b$data[[i]]$cumulative)) {
          sub <- sub | (b$data[[i]]$cumulative & (frame_vec <= f))
        }
        b$data[[i]] <- b$data[[i]][sub, ]
      }
    }
    if (title_frame) {
      if (!is.null(b$plot$labels$title)) {
        b$plot$labels$title <- paste(
          b$plot$labels$title,
          f
        )
      } else {
        b$plot$labels$title <- f
      }
    }
    b
  })
  ret <- list(plots = plots, frames = frames)
  class(ret) <- "gganimate"
  if (!is.null(filename)) {
    ret <- gganimate_save(ret, filename, saver, ...)
  } else {
    ret$ani_opts <- list(...)
    ret$saved <- FALSE
  }
  return(ret)
}
