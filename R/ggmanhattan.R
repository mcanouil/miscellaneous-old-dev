# @examples
# ggmanhattan()
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param x_chr PARAM_DESCRIPTION
#' @param x_pos PARAM_DESCRIPTION
#' @param y_pval PARAM_DESCRIPTION
#' @param y_trans PARAM_DESCRIPTION, Default: TRUE
#' @param x_space PARAM_DESCRIPTION, Default: 5e7
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggmanhattan
#' @export
ggmanhattan <- function(data, x_chr, x_pos, y_pval, y_trans = TRUE, x_space = 5e7, ...) {
  require(tidyverse)
  require(scales)
  args_values <- list(...)
  all_args <- names(args_values)
  old_args <- intersect(
    all_args,
    c("chr", "position", "y", "sep", "ytrans")
  )
  setdiff(
    all_args, 
    c("data", "x_chr", "x_pos", "y_pval", "y_trans", "x_space", "chr", "position", "y", "sep", "ytrans")
  ) %>% 
    purrr::map(.x = ., .f = ~message(paste0("Argument '", .x, "' is deprecated!")))
  if (length(old_args)!=0) {
    for (iarg in old_args) {
      message(paste0(
        "Argument '", 
        iarg, 
        "' is deprecated, use '", 
        switch(
          EXPR = iarg,
          "chr" = {"x_chr"},
          "position" = {"x_pos"},
          "y" = {"y_pval"},
          "sep" = {"x_space"},
          "ytrans" = {"y_trans"}
        ), 
        "' instead!"
      ))
    }
    
    for (iexpr in intersect(old_args, c("chr", "position", "y", "sep", "ytrans"))) {
      switch(
        EXPR = iexpr,
        "chr" = {x_chr <- args_values[["chr"]]},
        "position" = {x_pos <- args_values[["position"]]},
        "y" = {y_pval <- args_values[["y"]]},
        "sep" = {x_space <- args_values[["sep"]]},
        "ytrans" = {y_trans <- args_values[["ytrans"]]}
      )
    }
  }
    
  pval_trans <- function() {
    neglog10_breaks <- function(n = 5) {
      function(x) {
        rng <- -log(range(x, na.rm = TRUE), base = 10)
        min <- 0
        max <- floor(rng[1])
        if (max == min) {
          return(10^-min)
        } else {
          by <- floor((max - min) / n) + 1
          return(10^-seq(min, max, by = by))
        }
      }
    }
    scales::trans_new(
      name = "pval",
      transform = function(x) {
        -log(x, 10)
      },
      inverse = function(x) {
        10^-x
      },
      breaks = neglog10_breaks(),
      domain = c(1e-300, 1),
      format = function(x) {
        parse(
          text = scales::scientific_format()(x) %>%
            gsub("1e+00", "1", ., fixed = TRUE) %>%
            gsub("e", " %*% 10^", .)
        )
      }
    )
  }

  data <- data %>% 
    dplyr::rename(
      x_chr = !!x_chr,
      x_pos = !!x_pos,
      y_pval = !!y_pval
    ) %>% 
    dplyr::mutate(
      x_chr = x_chr %>% 
        toupper() %>% 
        gsub("CHR", "", .) %>% 
        factor(., levels = c(seq(22), "X", "Y")),
      x_pos = as.integer(x_pos),
      y_pval = as.numeric(y_pval)
    ) %>% 
    dplyr::filter(!is.na(x_chr) & !is.na(x_pos)) %>% 
    dplyr::arrange(x_chr, x_pos) %>% 
    dplyr::group_by(x_chr) %>% 
    dplyr::mutate(x_pos = x_pos - min(x_pos) + 1) %>% 
    dplyr::ungroup()

  data <- dplyr::full_join(
    x = data,
    y = data %>% 
      dplyr::group_by(x_chr) %>% 
      dplyr::summarise(x_pos = max(x_pos)) %>% 
      dplyr::mutate(x_start = c(0, cumsum(x_pos[-length(x_pos)]+x_space))) %>% 
      dplyr::select(x_chr, x_start),
    by = "x_chr"
  ) %>% 
    dplyr::mutate(
      x_pos = x_pos + x_start
    )

  x_breaks <- data %>% 
    dplyr::group_by(x_chr) %>% 
    dplyr::summarise(x_med = min(x_pos)+diff(range(x_pos))/2) %>% 
    dplyr::select(x_chr, x_med)

  p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x_pos, y = y_pval, colour = x_chr)) +
    ggplot2::geom_point(size = 1.5, shape = 21, na.rm = TRUE, show.legend = FALSE) +
    ggplot2::scale_colour_manual(
      values = rep(scales::viridis_pal(begin = 1/4, end = 3/4)(2), 12)
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks[["x_med"]],
      labels = x_breaks[["x_chr"]],
      limits = range(data[["x_pos"]]),
      expand = c(0.01, 0)
    ) +
    ggplot2::labs(y = y_pval, x = x_chr)
  if (y_trans) {
    p <- p +
      ggplot2::scale_y_continuous(trans = pval_trans(), expand = ggplot2::expand_scale(mult = c(0, 0.10)), limits = c(1, NA))
  } else {
    p <- p +
      ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0, 0.10)))
  }
  return(p)
}
