wbgfigure_parent <- ggplot2::ggproto("wbgfigure_parent",
  data = function(self, refresh = FALSE) {
    if (is.null(self$.cached_data) | refresh) {
      if (is.function(self$.data)) {
        self$.cached_data <- self$.data()
      } else {
        self$.cached_data <- self$.data
      }
    }
    return(self$.cached_data)
  },
  plot = function(self, ...) {
    self$.plot(self$data(), ...)
  },
  .data = function() {},
  .plot = function(data, style) {}
)

#' Create a new figure
#'
#' A figure is a reuseable, parameterisable, retargetable chunk of analysis and
#' graphical output.
#'
#' Usually \code{figure} is called within a function, where content parameters
#' can be specified to change what is shown in the figure (see Examples).
#'
#' Purely graphical parameters should be restricted to the plot function.
#'
#' In general most data transformations should occur in \code{data}, however,
#' transformations that are purely for presentation purposes (e.g. reordering
#' a factor) may occur in \code{plot}. Use your judgment - the output of
#' \code{data} may be made available to readers/viewers as the "Download the
#' data for this figure", so it should be obvious how to get from that to the
#' chart.
#'
#' @param data a function that takes no arguments and returns the data that will
#'   be plotted (either a dataframe or a list of named dataframes).
#' @param plot a function that takes the data to plot as its first argument, and
#'   graphical parameters as subsequent arguments, and returns either a ggplot
#'   object or a grob.
#' @param theme
#' @param aspect_ratio
#' @param ... other metadata, for example \code{title}, \code{subtitle}, etc.
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Minimal example
#' f <- figure(
#'   data = function() { mtcars },
#'   plot = function(df) { ggplot(df, aes(cyl, mpg)) + geom_point() }
#' )
#' f
#'
#' # More typical example, enclosed in function with parameter, and metadata
#' fig_cars <- function(auto_manual = c(0, 1)) {
#'   figure(
#'     data = function() { mtcars %>% filter(am %in% auto_manual) },
#'     plot = function(df) { ggplot(df, aes(cyl, mpg)) + geom_point() },
#'     title = "Cars with more cylinders are less fuel efficient",
#'     subtitle = "Miles per gallon vs engine cylinders",
#'     source = "Henderson and Velleman (1981)"
#'   )
#' }
#' fig_cars()
#' fig_cars(auto_manual = 0)
#'
#' @export
figure <- function(data, plot, theme = NULL, aspect_ratio = 1,...) {
  params = as.list(parent.frame())
  ggproto("wbgfigure", wbgfigure_parent,
          .data = data, .plot = plot,
          params = params, theme = theme, aspect_ratio = aspect_ratio,
          meta = list(...))
}

##### PRINT FUNCTIONS

#' @export
#' @method print wbgfigure
print.wbgfigure <- function(x, padding = margin(0,0,0,0,"pt"), ...) {
  grid.newpage()
  p <- x$plot(...)
  f <- add_captions(
    p,
    if(is.null(p$theme)) x$theme else p$theme,
    title = x$meta$title,
    subtitle = x$meta$subtitle,
    source = x$meta$source,
    note = x$meta$note,
    source_url = x$meta$source_url,
    show.logo = FALSE,
    padding = padding
  )
  grid.draw(f)
}

#' @export
figure_rmarkdown_pre <- function(fig) {
  paste0(
    "<figure>\n",
    "<figcaption class='figure title'>",htmltools::htmlEscape(fig$meta$title),"</figcaption>\n",
    "<div class='figure subtitle'>",htmltools::htmlEscape(fig$meta$subtitle),"</div>"
  )
}

#' @export
figure_rmarkdown_fig <- function(fig, style) {
  grid.newpage()
  plot <- fig$plot(style)
  if ("ggplot" %in% class(plot)) {
    g = ggplotGrob(plot)
  } else if ("grob" %in% class(plot)) {
    g = plot
  } else {
    stop("Don't know how to deal with object of class", class(plot))
  }
  grid.draw(g)
}

#' @export
figure_rmarkdown_post <- function(fig) {
  paste0(
    "<div class='figure note'>",htmltools::htmlEscape(fig$meta$note),"</div>\n",
    "<div class='figure source'>",htmltools::htmlEscape(fig$meta$source),"</div>\n",
    "</figure>"
  )
}

#' @export
figure_rmarkdown <- function(fig, style) {
  cat(figure_rmarkdown_pre(fig))
  figure_rmarkdown_fig(fig, style)
  cat(figure_rmarkdown_post(fig))
}

#' @export
figure_rmarkdown_ggiraph <- function(ggi) {
  ggi <- htmlwidgets::prependContent(ggi,htmltools::HTML(figure_rmarkdown_pre(fig)))
  ggi <- htmlwidgets::appendContent(ggi,htmltools::HTML(figure_rmarkdown_post(fig)))
  ggi
}

#' @export
figure_save_draft_png <- function(fig, style, filename, width = 1500/96/2, height = NULL, res = 96*2, metadata = TRUE, padding = margin(0,0,0,0,"pt"), ...) {
  if (is.null(height)) {
    height <- width / fig$aspect_ratio
  }

  # Save plot
  png(filename, width = width, height = height, units = "in", res = res, ...)
  p <- fig$plot(style())
  f <- add_captions(
    p,
    if(is.null(fig$theme)) p$theme else fig$theme,
    title = fig$meta$title,
    subtitle = fig$meta$subtitle,
    note = fig$meta$note,
    source = fig$meta$source,
    show.logo = FALSE,
    padding = padding
  )
  grid.draw(f)
  dev.off()

  # Mogrify plot to record resolution, for better Office imports, etc.
  retval <- system2("mogrify", c("-density 192", "-units pixelsperinch",filename))
  if (retval != 0) {
    warning("Failed to successfully run mogrify (perhaps you don't have imagemagick installed?). Saved PNG will not have resolution metadata.")
  }
}

#' @export
figure_save_web_png <- function(fig, style, filename, width = 1500/96/2, height = NULL, metadata = TRUE, ...) {
  if (is.null(height)) {
    height <- width / fig$aspect_ratio
  }

  # Save plot
  png(filename, width = width, height = height, units = "in", res = 96*2, ...)
  p <- fig$plot(style(12))
  f <- add_captions(
    p,
    if(is.null(fig$theme)) p$theme else fig$theme,
    show.logo = FALSE
  )
  grid.draw(f)
  dev.off()

  # Save metadata
  meta <- jsonlite::toJSON(fig$meta, pretty=TRUE, auto_unbox = TRUE)
  readr::write_file(meta, paste0(filename, ".meta.json"))

  # Save data
  data <- fig$data()
  if (!is.data.frame(data)) {
    { mapply(function(d, n) { write.csv(d, paste0(filename, "_",n,".csv")) }, data, names(data)) }
  } else {
    write.csv(data, paste0(filename, ".csv"))
  }
}

#' @export
figure_save_final_pdf <- function (fig, style, filename, width = 1500/96/2, height = NULL, colormodel = "cmyk", padding = margin(0,0,0,0,"pt"), ...){
  # Temporary fix
  filename <- tools::file_path_sans_ext(filename)

  if (is.null(height)) {
    height <- width/fig$aspect_ratio
  }

  # We use the pdf device and not quartz or cairo as neither support cmyk color model. However
  # pdf has bad unicode support and forces everything to single byte encoding. The standard latin
  # encoding does not support curly quotes, but this MacRoman one seems to.
  #
  # Also, see ?pdf for useDingbats - unfortunately for stroked shape = 21, this is rendered inconsistently,
  # so we fall back on actual circles and hope that will work.
  pdf(paste0(filename, ".pdf"), width = width, height = height, colormodel = colormodel, encoding = "MacRoman", useDingbats = FALSE, ...)

  p <- fig$plot(style())
  f <- add_captions(
    p,
    if (is.null(fig$theme)) p$theme else fig$theme,
    title = fig$meta$title,
    subtitle = fig$meta$subtitle,
    note = fig$meta$note,
    source = fig$meta$source,
    show.logo = FALSE,
    padding = padding)
  grid::grid.draw(f)
  dev.off()
}

#' @export
figure_save_final_pdf_cairo <- function (fig, style, filename, width = 1500/96/2, height = NULL, padding = margin(0,0,0,0,"pt"), ...){
  # Temporary fix
  filename <- tools::file_path_sans_ext(filename)

  if (is.null(height)) {
    height <- width/fig$aspect_ratio
  }

  # We use the Cairo device because it has much better character set support
  # Then post process to CMYK using ghostscript
  cairo_pdf(paste0(filename, ".pdf"), width = width, height = height, ...)

  p <- fig$plot(style())
  f <- add_captions(
    p,
    if (is.null(fig$theme)) p$theme else fig$theme,
    title = fig$meta$title,
    subtitle = fig$meta$subtitle,
    note = fig$meta$note,
    source = fig$meta$source,
    show.logo = FALSE,
    padding = padding)
  grid::grid.draw(f)
  dev.off()

  # Ghostscript plot to convert to CMYK
  retval <- system2("gs", c(paste0("-o ", filename, ".cmyk.pdf"), "-sDEVICE=pdfwrite","-sColorConversionStrategy=CMYK", "-dEmbedAllFonts=false", "-dProvideUnicode", paste0(filename, ".pdf")), stdout = FALSE)
  if (retval != 0) {
    warning("Failed to successfully run ghostscript. Saved PDF will be in RGB not CMYK.")
  } else {
    #file.remove(paste0(filename, ".pdf"))
    #file.rename(paste0(filename, ".pdf.temp"), paste0(filename, ".pdf"))
  }
}
