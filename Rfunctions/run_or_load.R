 run_or_load <- function(rds_path, expr) {
  sub_expr <- paste(deparse(substitute(expr)), collapse = "")

  if (!file.exists(rds_path)) {
    message('Evaluating "expr"...')
    tmp <- expr
    saveRDS(object = tmp, file = rds_path, compress = FALSE)
    if (any(grepl("<-", sub_expr))) {
      tmp
    } else {
      out <- tmp
    }
  } else {
    message(paste0('Loading "', rds_path, '" ...'))
    # if (any(grepl("<-", sub_expr))) {
    #   readRDS(file = rds_path)
    # } else {
      out <- readRDS(file = rds_path)
    # }
  }
  
  on.exit()
  # if (any(grepl("<-", sub_expr))) {
  #   return(invisible())
  # } else {
    return(invisible(out))
  # }
}

# run_or_load(rds_path = "./test.rds", expr = {x <- 1+1})
# x <- run_or_load(rds_path = "./test.rds", expr = {1+1})
