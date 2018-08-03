set_ncores <- function(n_cores = parallel::detectCores(), hooks = NULL) {
  if (is.null(hooks)) {
    stop('"hooks" is missing and must be provided!')
  }
  message <- paste0(
    '*', n_cores, ' cores*',
    ' (', paste0((n_cores / parallel::detectCores()) * 100, "%"), ')',
    ' are currently being used by _*', Sys.getenv("LOGNAME"), '*_', 
    ' (on _', Sys.info()[["nodename"]],  '_)',
    '!'
  )

  cmd <- paste(
    'curl',
    '-X POST',
    '-H "Content-Type: application/json"',
    '--data \'{', 
      '"text":"', message, '"',
    '}\'',
    hooks,
    '--silent'
  )
  res_cmd <- system(cmd, intern = TRUE)
  
  if (grepl("Bad Request", res_cmd) | !grepl("{\"success\":true}", res_cmd, fixed = TRUE)) {
    stop("Request to send a message to chat failed!")
  }

  return(n_cores)
}
