set_ncores <- function(n_cores = parallel::detectCores(), hooks = NULL) {
  if (is.null(hooks)) {
    stop('"hooks" is missing and must be provided!')
  }
  cmd <- paste(
    'curl -X POST -H "Content-Type: application/json"',
    '--data \'{"text":"', 
    paste0(n_cores, ' cores are currently being used by ', Sys.getenv("LOGNAME"), '!'),
    '"}\'',
    hooks,
    '--silent'
  )
  res_cmd <- system(cmd, intern = TRUE)
  
  if (grepl("Bad Request", res_cmd) | !grepl("{\"success\":true}", res_cmd, fixed = TRUE)) {
    stop("Request to send a message to chat failed!")
  }

  return(n_cores)
}
