set_ncores <- function(
  n_cores = parallel::detectCores(), 
  hooks = NULL, 
  message = "are currently being used"
) {
  if (is.null(hooks)) {
    stop('"hooks" is missing and must be provided!')
  }
  n_cores_percent <- paste0(
    '(', 
    round((n_cores / parallel::detectCores()) * 100, digits = 2), 
    '%)'
  )
  
  full_message <- paste0(
    '*', n_cores, ' cores* ',
    n_cores_percent,
    ' ', message,' by _*', Sys.getenv("LOGNAME"), '*_', 
    ' (on _', Sys.info()[["nodename"]],  '_)',
    '!'
  )

  cmd <- paste(
    'curl',
    '-X POST',
    '-H "Content-Type: application/json"',
    '--data \'{', 
      '"text":"', full_message, '"',
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
  hooks = NULL
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
  
  random_id <- paste0('[ID:', sprintf(fmt = "%04d", sample(1:1000, 1)), ']')
  
  sub_expr <- paste(deparse(substitute(expr)), collapse = "")
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
  n_cores_percent <- paste0(
    '(', 
    round((n_cores / parallel::detectCores()) * 100, digits = 2), 
    '%)'
  )

  message_in <- paste(
    random_id, 
    paste0('_', Sys.getenv("LOGNAME"), '_'), 
    '*started* using', paste0('*', n_cores, ' cores*'), 
    n_cores_percent,
    'on', paste0('_', Sys.info()[["nodename"]],  '_')
  )
  message_out <- paste(
    random_id, 
    paste0('_', Sys.getenv("LOGNAME"), '_'), 
    '*stopped* using', paste0('*', n_cores, ' cores*'), 
    n_cores_percent,
    'on', paste0('_', Sys.info()[["nodename"]],  '_')
  )
    
  send_message(message = message_in, hooks = hooks)
  
  if (any(grepl("<-[^(]*mclapply", sub_expr))) {
    expr
  } else {
    out <- expr
  }
  
  send_message(message = message_out, hooks = hooks)

  on.exit()
  if (any(grepl("<-[^(]*mclapply", sub_expr))) {
    return(invisible())
  } else {
    return(out)
  }
}

# bot_ncores(
#   res <- parallel::mclapply(rep(4, 25), mc.cores = 25, Sys.sleep)
# )
