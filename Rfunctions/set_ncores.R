set_ncores <- function(
  n_cores = parallel::detectCores(), 
  hooks = NULL
) {
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

# set_ncores(
#   n_cores = 42
# )

bot_ncores <- function (
  expr, 
  hooks = "http://chat.egid.local/hooks/sxsHEbEwyNMWx2CPo/4G6gEd8os43L2NniYR8GDiaP86vJtZBuePEved5h35oYevce"
) {
  if (is.null(hooks)) {
    stop('"hooks" is missing and must be provided!')
  }
  
  send_message <- function(message, hooks) {
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
    return(invisible())
  }
  
  random_id <- paste0('[ID:', sprintf(fmt = "%03d", sample(1:100, 1)), ']')
  
  sub_expr <- deparse(substitute(expr))
  if (any(grepl("mc.cores", sub_expr))) {
    n_cores <- gsub(
      pattern = ".*mc.cores *= *([^, ]+),.*", 
      replacement = "\\1", 
      x = grep("mc.cores", sub_expr, value = TRUE)
    )
    n_cores <- eval(parse(text = n_cores))
  } else {
    n_cores <- getOption("mc.cores", 3L)
  }

  message_in <- paste(
    random_id, 
    '_', Sys.getenv("LOGNAME"), '_', 
    '*started* using', paste0('*', n_cores, ' cores*'), 
    paste0('(', (n_cores / parallel::detectCores()) * 100, '%)'),
    'on', '_', Sys.info()[["nodename"]],  '_'
  )
  message_out <- paste(
    random_id, 
    '_', Sys.getenv("LOGNAME"), '_', 
    '*stopped* using', paste0('*', n_cores, ' cores*'), 
    paste0('(', (n_cores / parallel::detectCores()) * 100, '%)'),
    'on', '_', Sys.info()[["nodename"]],  '_'
  )
    
  send_message(message = message_in, hooks = hooks)
  
  if (any(grepl("<-", sub_expr))) {
    expr
  } else {
    out <- expr
  }
  
  send_message(message = message_out, hooks = hooks)

  on.exit()
  if (any(grepl("<-", sub_expr))) {
    return(invisible())
  } else {
    return(out)
  }
}

# bot_ncores(
#   res <- parallel::mclapply(rep(4, 25), mc.cores = 25, Sys.sleep)
# )
