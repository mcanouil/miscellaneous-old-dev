#' ggqqplot
#'
#' @param data [data.frame]
#' @param col_names [character]
#' @param point_size [numeric]
#'
#' @return ggplot2
#' @export
#'
#' @noRd
ggqqplot <- function (data, col_names = colnames(data), point_size = 1) {
  if (is.null(ncol(data))) {
    data <- data.frame(X1 = data)
  }

  if (is.null(col_names)) {
    colnames(data) <- col_names
  }
  
  set_colour <- function() {
    ggplot2::theme_get()$plot.background$colour %>% 
      grDevices::col2rgb() %>% 
      range() %>% 
      sum() %>% 
      (function(.x) {
        if ((.x* 100 * 0.5/255)>=50) "black" else "white"
      })()
  }
  
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
          text = gsub("e", " %*% 10^", gsub("1e+00", "1", scales::scientific_format()(x), fixed = TRUE))
        )
      },
      domain = c(0, 1)
    )
  }
  
  data %>% 
    as.data.frame() %>% 
    tidyr::gather(key = "group", value = "obspval") %>% 
    dplyr::group_by(group) %>% 
    dplyr::arrange(obspval) %>% 
    dplyr::mutate(
      exppval = (seq_len(n()) - 0.5)/length(seq_len(n())),
      gc = median(qnorm(obspval/2)^2, na.rm = TRUE)/qchisq(0.5, df = 1),
      logobspval = -log10(obspval),
      logexppval = -log10(exppval)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      labels = paste0("lambda[", group, "]==", round(gc, digits = 3)),
      labels = factor(labels, levels = unique(labels))
    ) %>% 
    ggplot2::ggplot() +
      ggplot2::geom_abline(intercept = 0, slope = 1, colour = set_colour()) +
      ggplot2::geom_point(
        mapping = ggplot2::aes_string(
          x = "exppval", 
          y = "obspval", 
          colour = "labels", 
          shape = "labels"
        ), 
        size = point_size
      ) +
      ggplot2::scale_x_continuous(trans = pval_trans()) +
      ggplot2::scale_y_continuous(trans = pval_trans()) +
      ggplot2::scale_colour_viridis_d(labels = scales::parse_format()) +
      ggplot2::scale_shape_discrete(solid = FALSE, labels = scales::parse_format()) +
      ggplot2::labs(
        x = "Expected P-value", 
        y = "Observed P-value", 
        title = "Q-Q plot",
        colour = NULL,
        shape = NULL
      )
}

