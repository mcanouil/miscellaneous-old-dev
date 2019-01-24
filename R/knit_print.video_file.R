knit_print.video_file <- function(x, options, ...) {
  if (grepl('\\.(mp4)|(webm)|(ogg)$', x, ignore.case = TRUE)) {
    knitr::knit_print(
      htmltools::browsable(
        as_html_video(x, width = get_chunk_width(options), autoplay = get_chunk_autoplay(options))
      ), 
      options, 
      ...
    )
  } else {
    warning('The video format doesn\'t support HTML', call. = FALSE)
    invisible(NULL)
  }
}

as_html_video <- function(x, width = NULL, autoplay = TRUE) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop('The base64enc package is required for showing video')
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop('The htmltools package is required for showing video')
  }
  format <- tolower(sub('^.*\\.(.+)$', '\\1', x))
  htmltools::HTML(paste0(
    '<video controls', if (autoplay) ' autoplay' else '',
    if (is.null(width)) '' else paste0(' width="', width, '"'),
    '><source src="data:video/',
    format,
    ';base64,',
    base64enc::base64encode(x),
    '" type="video/mp4"></video>'
  ))
}

get_chunk_autoplay <- function (options) {
  options$autoplay %||% TRUE
}
