pretty_kable <- function (
  data, 
  font_size = 12, 
  format_args = list(scientific = -1, digits = 3, big.mark = ","), 
  col.names = NA,
  pval_cols = NULL,
  full_width,
  echo = TRUE,
  ...
) {
  require(Hmisc)
  require(knitr)
  require(kableExtra)
	format_pval <- function (x, thresh = 10^-2, digits = 3, eps = 1e-50) {
		ifelse(
			x>=thresh, 
			Hmisc::format.pval(x, digits = digits, eps = eps, nsmall = digits), 
			base::format.pval(x, digits = digits, eps = eps, scientific = TRUE, nsmall = digits)
		)
	}
  if (!is.null(pval_cols)) {
    data[, pval_cols] <- format_pval(
      x = data[, pval_cols], 
      digits = format_args$digits
    )
  }
  colnames(data) <- Hmisc::capitalize(colnames(data))
  out <- if (knitr:::is_latex_output()) {
    options(knitr.table.format = "latex")
    output <- knitr::kable(x = data, booktabs = TRUE, format.args = format_args, col.names = col.names, ...)
    kableExtra::kable_styling(
      kable_input = output,
      latex_options = c("striped", "hold_position"),
      full_width = ifelse(missing(full_width), FALSE, full_width),
      position = "center",
      font_size = font_size
    )
  } else {
    options(knitr.table.format = "html")
    output <- knitr::kable(x = data, format.args = format_args, col.names = col.names, ...)
    kableExtra::kable_styling(
      kable_input = output,
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = ifelse(missing(full_width), TRUE, full_width),
      position = "center",
      font_size = font_size
    )
  }
  
  if (echo) print(out)
  
  invisible(out)
}
