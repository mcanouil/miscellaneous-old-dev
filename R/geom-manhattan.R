#' pval_trans
#'
#' @return trans
#' @export
pval_trans <- function() {
  neglog10_breaks <- function(n = 5) {
    function(x) {
      rng <- -log(range(x, na.rm = TRUE), base = 10)
      min <- 0
      max <- floor(rng[1])
      if (max == min) {
        10^-min
      } else {
        by <- floor((max - min) / n) + 1
        10^-seq(min, max, by = by)
      }
    }
  }
  scales::trans_new(
    name = "pval",
    transform = function(x) {-log(x, 10)},
    inverse = function(x) {10^-x},
    breaks = neglog10_breaks(),
    format = function(x) {
      parse(
        text = gsub("1 %*% ", "", gsub("e", " %*% 10^", gsub("1e+00", "1", scales::scientific_format()(x), fixed = TRUE)))
      )
    },
    domain = c(0, 1)
  )
}

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
  shape = 16,
  size = 0.5,
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
      size = size,
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
  shape = 16,
  fill = NA,
  size = 0.5,
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
        size = size,
        ...
      )
    ),
    ggplot2::scale_x_continuous(
      breaks = 1:24,
      labels = c(1:22, "X", "Y"),
      expand = ggplot2::expansion(add = 0.25)
    ),
    ggplot2::scale_y_continuous(
      trans = "pval",
      expand = ggplot2::expansion(mult = c(0, 0.10)),
      limits = c(1, NA)
    ),
    ggplot2::scale_colour_manual(values = rep(scales::viridis_pal(begin = 1/4, end = 3/4)(2), 12)),
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
    map_chro <- c(seq(22), "X", "Y", "X", "Y") 
    names(map_chro) <- c(seq(24), "X", "Y")
      
    data %>%
      dplyr::mutate(
        x_chr = map_chro[colour],
        x_chr = factor(x_chr, levels = unique(map_chro)),
        colour = x_chr,
        x_pos = as.integer(x),
        y_pval = as.numeric(y)
      ) %>%
      dplyr::group_by(x_chr) %>%
      dplyr::arrange(x_pos) %>%
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

fortify.manhattan <- function(data, x, y, group) {
  map_chro <- c(seq(22), "X", "Y", "X", "Y") 
  names(map_chro) <- c(seq(24), "X", "Y")
    
  data %>%
    dplyr::mutate(
      x_chr = map_chro[.data[[group]]],
      x_chr = factor(x_chr, levels = unique(map_chro)),
      colour = x_chr,
      x_pos = as.integer(.data[[x]]),
      y_pval = as.numeric(.data[[y]])
    ) %>%
    dplyr::group_by(x_chr) %>%
    dplyr::arrange(x_pos) %>%
    dplyr::mutate(x_pos = scales::rescale(x = x_pos, to = c(-0.4, 0.4))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x_pos = x_pos + as.integer(x_chr)) %>%
    dplyr::select(-x, -y, -group) %>%
    dplyr::rename(x = x_pos, y = y_pval, group = x_chr)
}

# #' @rdname stat_manhattan
# #'
# #' @format NULL
# #' @usage NULL
# #'
# #' @export
# GeomManhattan <- ggplot2::ggproto("GeomManhattan", ggplot2::Geom,
#   required_aes = c("x", "y", "colour"),
#   default_aes = ggplot2::aes(group = ggplot2::stat(colour), shape = 16),
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



# library(tidyverse)
# dta <- tibble(
#   chr = factor(x = c(seq(22), "X", "Y"), levels = c(seq(22), "X", "Y")),
#   pos = replicate(
#     n = length(chr), 
#     sort(sample(x = seq(1e4), size = runif(n = 1, min = 100, max = 1000)))
#   ),
#   pvalue = map(.x = pos, .f = ~minfi::ilogit2(rnorm(length(.x), mean = -10, sd = 10)))
# ) %>%
#   unnest(c(pos, pvalue)) %>%
#   mutate(
#     label = ifelse(pvalue<1e-12, "sign", NA),
#     position = seq(n())
#   )

# ggplot(data = dta, mapping = aes(x = pos, y = pvalue, colour = chr)) +
#   geom_manhattan() + # ou stat_manhattan()
#   geom_label(mapping = aes(label = label), stat = "manhattan", fill = NA, na.rm = TRUE, show.legend = FALSE)
