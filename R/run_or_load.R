 run_or_load <- function(expr, rds_path = tempfile(pattern = 'cache_rol', fileext = '.rds')) {
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

# run_or_load(expr = {x <- 1+1})
# x <- run_or_load( expr = {1+1})
