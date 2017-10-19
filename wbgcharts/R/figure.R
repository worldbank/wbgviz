#' @export
figure <- function(p, theme=NULL, aspect_ratio=2/1, ...) {
  out <- list(plot = p, theme = theme, aspect_ratio = aspect_ratio, meta = list(...))
  class(out) <- "wbgfigure"
  out
}

#' @export
#' @method print wbgfigure
print.wbgfigure <- function(x) {
  f <- add_captions(
    x$plot,
    if(is.null(x$theme)) x$plot$theme else x$theme,
    title = x$meta$title,
    subtitle = x$meta$subtitle,
    source = x$meta$source,
    note = x$meta$note,
    source_url = x$meta$source_url,
    show.logo = TRUE
  )
  #grid.newpage()
  grid.draw(f)
}

#' @export
figure_save_twitter <- function(fig, filename) {
  png(filename, width = 1012, height = 506, res = 96*2)
  f <- add_captions(
    fig$plot,
    if(is.null(fig$theme)) fig$plot$theme else fig$theme,
#    title = fig$meta$title,
#    subtitle = fig$meta$subtitle,
    note = NULL,
    source_url = paste0(fig$meta$source_url, if (is.null(fig$meta$note)) " - See link for source details." else "- See link for source details and notes."),
    show.logo = TRUE
  )
  grid.draw(f)
  dev.off()
}

#' @export
figure_save_print_pdf <- function(fig, filename, width = 5, height = NULL, ...) {
  if (is.null(height)) {
    height <- width / fig$aspect_ratio
  }

  pdf(filename, width = width, height = height, ...)
  f <- add_captions(
    fig$plot,
    if(is.null(fig$theme)) fig$plot$theme else fig$theme,
    title = fig$meta$title,
    subtitle = fig$meta$subtitle,
    source = fig$meta$source,
    note = fig$meta$note,
    show.logo = FALSE
  )
  grid.draw(f)
  dev.off()
}

#' @importFrom rvg dsvg
#' @export
figure_save_web_svg <- function(fig, filename, width = 5, height = NULL, metadata = TRUE, ...) {
  if (is.null(height)) {
    height <- width / fig$aspect_ratio
  }

  # Save plot
  rvg::dsvg(filename, width = width, height = height, ...)
  f <- add_captions(
    fig$plot,
    if(is.null(fig$theme)) fig$plot$theme else fig$theme,
    show.logo = FALSE
  )
  grid.draw(f)
  dev.off()

  # Save metadata
  meta <- jsonlite::toJSON(fig$meta, pretty=TRUE, auto_unbox = TRUE)
  readr::write_file(meta, paste0(filename, ".meta.json"))
}

#' @export
figure_save_web_png <- function(fig, filename, width = 1500/96/2, height = NULL, metadata = TRUE, ...) {
  if (is.null(height)) {
    height <- width / fig$aspect_ratio
  }

  # Save plot
  png(filename, width = width, height = height, units = "in", res = 96*2, ...)
  f <- add_captions(
    fig$plot,
    if(is.null(fig$theme)) fig$plot$theme else fig$theme,
    show.logo = FALSE
  )
  grid.draw(f)
  dev.off()

  # Save metadata
  meta <- jsonlite::toJSON(fig$meta, pretty=TRUE, auto_unbox = TRUE)
  readr::write_file(meta, paste0(filename, ".meta.json"))
}

#' @export
figure_save_allformats <- function(fig, basename, style=style_atlas) {
  figure_save_twitter(fig(style()), paste0(basename, "_twitter.png"))
  figure_save_print_pdf(fig(style()), paste0(basename, ".pdf"))
  figure_save_web_svg(fig(style(10)), paste0(basename, ".svg"))
  figure_save_web_png(fig(style(10)), paste0(basename, ".png"))
}
