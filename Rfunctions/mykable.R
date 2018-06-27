mykable <- function(
  data,
  font_size = 12,
  format.args = list(scientific = -1, digits = 3, big.mark = ","),
  col.names = NA,
  pval_cols = NULL,
  ...
) {
  require(knitr)
  require(kableExtra)
  require(Hmisc)
  format_pval <- function(x, thresh = 10^-2, digits = 3, eps = 1e-50) {
    ifelse(
      x >= thresh,
      Hmisc::format.pval(x, digits = digits, eps = eps, nsmall = digits),
      base::format.pval(x, digits = digits, eps = eps, scientific = TRUE, nsmall = digits)
    )
  }
  if (!is.null(pval_cols)) {
    data[, pval_cols] <- format_pval(
      x = data[, pval_cols],
      digits = format.args$digits
    )
  } else {}
  colnames(data) <- capitalize(colnames(data))
  if (knitr:::is_latex_output()) {
    options(knitr.table.format = "latex")
    kable(x = data, booktabs = TRUE, format.args = format.args, col.names = col.names, ...) %>%
      kable_styling(
        latex_options = c("striped", "hold_position"),
        full_width = FALSE,
        position = "center",
        font_size = font_size
      )
  } else {
    options(knitr.table.format = "html")
    kable(x = data, format.args = format.args, col.names = col.names, ...) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = TRUE,
        position = "center",
        font_size = font_size
      )
  }
}
