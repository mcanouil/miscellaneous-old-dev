send_message <- function(
  message, 
  tag = paste0('_', Sys.getenv("LOGNAME"), '@',  Sys.info()[["nodename"]], '_', ':'), 
  hooks = NULL
) {
  cmd <- paste(
    'curl',
    '-X POST',
    '-H "Content-Type: application/json"',
    '--data \'{', 
      '"text":"', tag, message, '"',
    '}\'',
    hooks,
    '--silent'
  )
  res_cmd <- system(cmd, intern = TRUE)
  
  if (grepl("Bad Request", res_cmd) | !grepl("{\"success\":true}", res_cmd, fixed = TRUE)) {
    stop("Request to send a message to chat failed!")
  }
  invisible()
}

