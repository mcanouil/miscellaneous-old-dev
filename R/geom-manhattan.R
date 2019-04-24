#' stat_manhattan
#'
#' @param mapping []
#' @param data []
#' @param geom []
#' @param position []
#' @param na.rm []
#' @param show.legend []
#' @param inherit.aes []
#' @param shape []
#' @param fill []
#' @param ... []
#'
#' @return theme
#' @export
stat_manhattan <- function(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  shape = 21,
  fill = NA,
  ...
) {
  ggplot2::layer(
    stat = StatManhattan,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      shape = shape,
      fill = fill,
      ...
    )
  )
}


#' @rdname stat_manhattan
#' @export
geom_manhattan <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  shape = 21,
  fill = NA,
  ...
) {
  list(
    ggplot2::layer(
      stat = StatManhattan,
      data = data,
      mapping = mapping,
      geom = 'point',
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        shape = shape,
        fill = fill,
        ...
      )
    ),
    ggplot2::scale_x_continuous(
      breaks = 1:24,
      labels = c(1:22, "X", "Y"),
      expand = ggplot2::expand_scale(add = 0.25)
    ),
    ggplot2::scale_y_continuous(
      trans = "pval",
      expand = ggplot2::expand_scale(mult = c(0, 0.10)),
      limits = c(1, NA)
    ),
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank()
    ),
    ggplot2::labs(colour = "Chromosome", x = "Chromosome", y = "P-Value")
  )
}

#' @rdname stat_manhattan
#'
#' @format NULL
#' @usage NULL
#'
#' @export
StatManhattan <- ggplot2::ggproto("StatManhattan", ggplot2::Stat,
  required_aes = c("x", "y", "colour"),
  default_aes = ggplot2::aes(group = ggplot2::stat(colour)),
  setup_data = function(data, params) {
    data %>%
      dplyr::mutate(
        x_chr = factor(
          x = c(seq(22), "X", "Y")[colour],
          levels = c(seq(22), "X", "Y")
        ),
        colour = x_chr,
        x_pos = as.integer(x),
        y_pval = as.numeric(y)
      ) %>%
      dplyr::arrange(x_chr, x_pos) %>%
      dplyr::group_by(x_chr) %>%
      dplyr::mutate(x_pos = scales::rescale(x = x_pos, to = c(-0.4, 0.4))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(x_pos = x_pos + as.integer(x_chr)) %>%
      dplyr::select(-x, -y, -group) %>%
      dplyr::rename(x = x_pos, y = y_pval, group = x_chr)
  },
  compute_panel = function(data, scales, params) {
    data
  }
)


# #' @rdname stat_manhattan
# #'
# #' @format NULL
# #' @usage NULL
# #'
# #' @export
# GeomManhattan <- ggplot2::ggproto("GeomManhattan", ggplot2::Geom,
#   required_aes = c("x", "y", "colour"),
#   default_aes = ggplot2::aes(group = ggplot2::stat(colour), shape = 21),
#   draw_key = draw_key_point,
#   draw_panel = function(data, panel_params, coord) {
#     coords <- coord$transform(data, panel_params)
#     grid::pointsGrob(
#       x = coords$x,
#       y = coords$y,
#       pch = coords$shape,
#       gp = grid::gpar(col = coords$colour)
#     )
#   }
# )


