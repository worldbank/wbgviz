# = Titling functions ==========================================================

text_element_to_gp <- function(elem) {
  gpar(
    fontfamily = elem$family,
    fontface   = elem$face,
    col        = elem$colour,
    fontsize   = elem$size,
    lineheight = elem$lineheight
  )
}

make_title <- function(theme, title = NULL) {
  if (is.null(title))
    return(zeroGrob())

  wraptextGrob(
    label = title,
    x = unit(0, "npc"),
    y = unit(1, "npc"),
    hjust = 0, vjust = 1,
    gp = text_element_to_gp(calc_element('plot.title', theme))
  )
}

make_subtitle <- function(theme, subtitle = NULL) {
  if (is.null(subtitle))
    return(zeroGrob())

  wraptextGrob(
    label = subtitle,
    x = unit(0, "npc"),
    y = unit(1, "npc"),
    hjust = 0, vjust = 1,
    gp = text_element_to_gp(calc_element('plot.subtitle', theme))
  )
}

make_note <- function(theme, note = NULL) {
  if (is.null(note))
    return(zeroGrob())

  wraptextGrob(
    label = note,
    x = unit(0, "npc"),
    y = unit(1, "npc"),
    hjust = 0, vjust = 1,
    gp = text_element_to_gp(calc_element('plot.caption', theme))
  )
}

make_source <- function(theme, .source = NULL) {
  if (is.null(.source))
    return(zeroGrob())

  wraptextGrob(
    label = .source,
    x = unit(0, "npc"),
    y = unit(1, "npc"),
    hjust = 0, vjust = 1,
    gp = text_element_to_gp(calc_element('plot.caption', theme))
  )
}

make_source_url <- function(theme, source_url = NULL) {
  if (is.null(source_url))
    return(zeroGrob())

  gp <- text_element_to_gp(calc_element('plot.caption', theme))
  gp$cex <- 0.8
  gp$col <- "gray60"

  textGrob(
    label = paste("© 2017 CC BY 3.0 IGO", source_url),
    x = unit(0, "npc"),
    y = unit(0.1, "npc"),
    hjust = 0, vjust = 0,
    gp = gp
  )
}

make_logo <- function(theme, show.logo = TRUE) {
  if (show.logo) {
    if (requireNamespace("png", quietly = TRUE)) {
      logo <- png::readPNG(system.file("logo-wb-header-en.png", package="wbgcharts"))
      logoGrob <- rasterGrob(
        logo,
        x = unit(1, "npc"),
        y = unit(0, "npc"),
        hjust = 1,
        vjust = 0,
        height = unit(1.8, "line"),
        name = "logo",
        gp = text_element_to_gp(ggplot2::calc_element('plot.caption', theme))
      )
    } else {
      stop("Require package 'png' installed to use logos")
    }
  } else {
    logoGrob = zeroGrob()
  }

  gTree(
    children = gList(logoGrob),
    gp = text_element_to_gp(ggplot2::calc_element('plot.caption', theme))
  )
}

#' Add captions (title, notes, etc) to a plot or grid graphics object (grob)
#'
#' \code{add_captions} adds captions to a plot outside of \code{ggplot}. This is
#' useful since the default \code{ggplot} title, subtitle and caption align with
#' the plot area, rather than the entire figure (including axes),
#' unlike many publishing standards.
#'
#' The \code{title} and \code{subtitle} arguments correspond directly to the
#' same in \code{title}, and appear above the plot. The \code{note} and
#' \code{source} arguments share the work of \code{ggplot}'s \code{caption} and
#' appear below the plot. The \code{source_url} and logo appear at the very
#' bottom of the plot, in slightly smaller text, and would usually only be
#' shown in standalone, branded graphics (for e.g. distribution on social media)
#' and not, for instance, within a report.
#'
#' The relevant theme elements within \code{theme} are the same as would be used
#' if the the \code{ggplot} title, subtitle and caption were used.
#'
#' @param plot the \code{ggplot} plot object or a {grid} grob
#' @param theme the \code{ggplot} theme object to use for styling captions
#' @param title the plot title (displayed at top)
#' @param subtitle the plot subtitle
#' @param note the plot note
#' @param source the plot source
#' @param source_url the plot source URL or other location info
#' @param show.logo if TRUE, include a logo
#' @return a new \code{gtable} object which can be \code{print}ed to a graphic device
#'
#' @examples
#' library(ggplot2)
#' library(grid)
#'
#' # Simple example for adapting a ggplot labs() call...
#' p <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
#'
#' p + labs(
#'   title = "This is a title",
#'   subtitle = "This is a subtitle",
#'   caption = "This is a caption"
#' )
#'
#' # ...to use add_captions() instead
#' g <- add_captions(
#'   plot = p,
#'   theme = p$theme,
#'   title = "This is a title",
#'   subtitle = "This is a subtitle",
#'   note = "Note: This is a note",
#'   source = "Note: This is a source",
#'   source_url = "http://where.this.is.found.com",
#'   show.logo = TRUE
#' )
#' grid.newpage()
#' grid.draw(g)
#'
#' # Some relevant text attributes of ggplot themes are obeyed
#' p <- ggplot(mtcars, aes(cyl, mpg)) + geom_point() +
#'   theme_classic() + theme(plot.title = element_text(color = "blue"))
#'
#' p + labs(
#'   title = "This is a title",
#'   subtitle = "This is a subtitle",
#'   caption = "This is a caption"
#' )
#'
#' g <- add_captions(
#'   plot = p,
#'   theme = p$theme,
#'   title = "This is a title",
#'   subtitle = "This is a subtitle",
#'   note = "Note: This is a note",
#'   source = "Note: This is a source",
#'   show.logo = FALSE
#' )
#' grid.newpage()
#' grid.draw(g)
#' @export
add_captions <- function(plot, theme, title = NULL, subtitle = NULL, note = NULL, source = NULL, source_url = NULL, show.logo=TRUE) {
  # TODO maybe wrap everything in a gTree and apply the gpar from the top level ggplot2
  # in order to catch the theme(text = ...) inherited stuff
  grob_title <- make_title(theme, title)
  grob_subtitle <- make_subtitle(theme, subtitle)
  grob_note <- make_note(theme, note)
  grob_source <- make_source(theme, source)
  grob_logo <- make_logo(theme, show.logo)
  grob_source_url <- make_source_url(theme, source_url)

  if ("ggplot" %in% class(plot)) {
    grob_plot = ggplotGrob(plot)
  } else if ("grob" %in% class(plot)) {
    grob_plot = plot
  } else {
    stop("Don't know how to deal with object of class", class(plot))
  }

  # If we add the padding in the main gtable it can break the wrapping
  # as it will not be accounted for (this is complex). So we add it via
  # the viewpoint.
  vp <- viewport(width = unit(1 - 0.025*2, "npc"), height = unit(1 - 0.025*2, "npc"))

  # FIXME: incorporate extra space into elements not here
  figure <- gtable(
    unit.c(unit(1, "null"), grobWidth(grob_logo$children[[1]])),
    unit.c(
      grobHeight(grob_title)+unit(0.1*!is.null(title), "in"),
      grobHeight(grob_subtitle)+unit(0.1*!is.null(subtitle), "in"),
      unit(1, "null"),
      grobHeight(grid.force(grob_note))+unit(0.1*!is.null(note), "in"),
      grobHeight(grob_source)+unit(0.1*!is.null(source), "in"),
      max(grobHeight(grob_source_url), grobHeight(grob_logo$children[[1]]))
    ),
    vp = vp
  ) %>%
    gtable_add_grob(grob_title, 1, 1, 1, 2) %>%
    gtable_add_grob(grob_subtitle, 2, 1, 2, 2) %>%
    gtable_add_grob(grob_plot, 3, 1, 3, 2) %>%
    gtable_add_grob(grob_note, 4, 1, 4, 2) %>%
    gtable_add_grob(grob_source, 5, 1, 5, 2) %>%
    gtable_add_grob(grob_source_url, 6, 1, clip="on") %>%
    gtable_add_grob(grob_logo, 6, 2, clip="on")

  #gtable_show_layout(figure)
  #stop()

  figure
}

#' @export
# = Compound charts ============================================================

interleave_panels_horizontal <- function(
  ...,
  width.ratio = 1, major.space = unit(1, "line"), minor.space = unit(0.5, "line"))
{
  pgs <- lapply(lapply(list(...), ggplotGrob), grid.force)
  panel_names <- pgs[[1]]$layout$name[(grepl("panel", pgs[[1]]$layout$name))]

  plot_count <- length(pgs)
  panel_count <- length(panel_names)

  ylabels <- getGrob(pgs[[1]], gPath("axis-l"), grep=T)

  # TODO could redo this bit with getGrob(global=T)
  panels <- lapply(panel_names, function(panel_name) {
    lapply(pgs, function(pg) {
      getGrob(pg, gPath(panel_name), grep=T)
    })
  })

  #strip_gp <- getGrob(pgs[[1]], gPath("GRID.stripGrob", "GRID.text"), grep=T)$gp

  # TODO styles don't carry over still
  plot_titles <- lapply(pgs, function(pg) {
    getGrob(pg, gPath("title\\.", "GRID.text"), grep=T)
  })
  panel_titles <- as.list(getGrob(pgs[[1]], gPath("GRID.titleGrob"), grep=T, global=T))

  chart <- gtable(
    unit.c(ylabels$width, unit(rep_len(width.ratio,panel_count*plot_count), "null")),
    unit.c(
      if (length(panel_titles) > 0) grobHeight(panel_titles[[1]])*1.2 else unit(0, 'null'),
      grobHeight(plot_titles[[1]])*1.2,
      unit(1, 'null')
    )
  )

  chart <- gtable_add_grob(chart, ylabels, 3, 1)
  for (panel_id in 1:panel_count) {
    if (length(panel_titles) > 0)
      chart <- gtable_add_grob(chart, panel_titles[[panel_id]], 1, 1+(panel_id-1)*plot_count+1)
    for (plot_id in 1:plot_count) {
      column <- 1+(panel_id-1)*plot_count+plot_id
      chart <- gtable_add_grob(chart, plot_titles[[plot_id]], 2, column)
      chart <- gtable_add_grob(chart, panels[[panel_id]][[plot_id]], 3, column)
    }
  }

  col_space <- rep(c(rep(list(minor.space), plot_count-1), list(major.space)), panel_count)
  col_space <- col_space[1:length(col_space)-1]
  col_space <- do.call(unit.c, c(list(unit(0, "line")), col_space))
  chart <- gtable_add_col_space(chart, col_space)
  chart <- gtable_add_padding(chart, padding = unit(c(0,0,0.5,0), "line"))

  chart <- gTree(children = gList(chart), gp = gpar())
}

#' @export
current_aspect_ratio <- function() {
  return(
    convertWidth(unit(1, "npc"), "in", valueOnly = TRUE) /
    convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
  )
}

#' Convert CMYK percentages to RGB in the simplest way
#'
#' @export
cmyk <- function(c, m, y, k, maxColorValue = 1.0) {
  r = (1-c/maxColorValue) * (1-k/maxColorValue)
  g = (1-m/maxColorValue) * (1-k/maxColorValue)
  b = (1-y/maxColorValue) * (1-k/maxColorValue)
  rgb(r, g, b, alpha = 1)
}

#' Convert an RGBA color to RGB by combining with a background color
#'
#' @param color a color or vector of colors
#'
#' @param background a background color
#' @param alpha the alpha to use, which if not NULL will override any alphas in \code{color}
#' @param preserve.names preserve the names of \code{color} if any
#'
#' @export
rgba2rgb <- function(color, background = "white", alpha = NULL, preserve.names = TRUE)
{
  color.names <- names(color)
  color <- col2rgb(color, alpha = TRUE)
  background <- col2rgb(background)
  if (is.null(alpha))
    alpha <- color['alpha',] / 255

  out <- rgb(
    (1 - alpha) * background['red',1] + alpha * color['red',],
    (1 - alpha) * background['green',1] + alpha * color['green',],
    (1 - alpha) * background['blue',1] + alpha * color['blue',],
    maxColorValue = 255
  )

  if (preserve.names) {
    names(out) <- color.names
  }
  out
}

#' @export
lighten <- function(color, amount=0.5) {
  rgba2rgb(color, "white", 1-amount)
}

#' @export
darken <- function(color, amount=0.5) {
  rgba2rgb(color, "black", 1-amount)
}


#' Combine factors for a single \code{ggplot} aesthetic
#'
#' @examples
#' # Suppose we want a fill that uses two variables
#' p <- ggplot(
#'   diamonds[1:10,],
#'   aes(depth, price, color = comb_aes(cut, color))
#' ) +
#'   geom_point()
#' p
#'
#' # If we have labels set up for each of the factors, we can combine them
#' # (although this doesn't offer much advantage over using a function for
#' # the label argument).
#'
#' cut.labels <- setNames(as.character(unique(diamonds$cut)), unique(diamonds$cut))
#' color.labels <- c(
#'   D = "D (best)", E = "E", F = "F", G = "G", H = "H", I = "I", J = "J (best)"
#' )
#' labels <- comb_aes_values(diamonds$cut, diamonds$color, FUN =
#'   function(cut, color) {
#'     paste0(cut.labels[[cut]], " / ",color.labels[[color]])
#'   }
#' )
#' p + scale_color_discrete(labels = labels)
#'
#' # More usefully, we can do the same with other aesthetics (e.g. color) to
#' # create multi-dimensional color scales.
#'
#' @export
comb_aes <- function(...) {
  codes <- paste(..., sep = ".")
  combs <- expand.grid(lapply(rev(list(...)), levels), stringsAsFactors = FALSE)
  levels <- apply(combs, 1, function(r) {paste(rev(r), collapse = ".")})
  codes <- factor(codes, levels = levels)
  codes
}

#' @rdname comb_aes
#' @export
comb_aes_values <- function(..., FUN, not.found = NA) {
  combs <- expand.grid(lapply(rev(list(...)), levels), stringsAsFactors = FALSE)
  levels <- apply(combs, 1, function(r) {paste(rev(r), collapse = ".")})
  values <- apply(combs, 1, function(r) {
    tryCatch(
      do.call(FUN, unname(as.list(rev(as.character(r))))),
      error = function(e) { not.found }
    )
  })
  setNames(values, levels)
}

#' Create a list with self-referential elements
#'
#' \code{listy()} is like \code{list()} except that successive elements can
#' refer back to previously defined elements
#'
#' @inheritParams base::list
#'
#' @examples
#' # This doesn't work
#' list(a = 2, b = 2 * a)
#'
#' # But this does
#' listy(a = 2, b = 2 * a)
#'
#' @export
listy <- function(...) {
  dots <- eval(substitute(alist(...)))
  l <- list()
  for (i in 1:length(dots)) {
    if (is.null(names(dots)[i]))
      l[[i]] <- eval(dots[[i]], envir = l)
    else
      l[[names(dots)[i]]] <- eval(dots[[i]], envir = l)
  }
  l
}

count_lines <- function(strings) {
    sapply(strsplit(strings, "\n"), length)
}

#' Wrap a vector of strings to a specified number of lines
#'
#' Like \code{stringr::str_wrap()} except that instead of specifying a width you
#' specify a "height" (ie. a number of lines that each string can occupy). The
#' longest string and "height" will jointly determine the width, which will then
#' be applied to the other strings - so that strings are not unnecessarily wrapped.
#'
#' @export
str_wrap_lines <- function(strings, lines = 2, indent = 0, exdent = 0) {
  # Calculate the necessary width for a two line wrap - it has to be at least as
  # large as 1/lines the longest string
  width <- max(round(nchar(wbgref$regions$labels)/lines))
  while (TRUE) {
    wrapped <- stringr::str_wrap(strings, width, indent, exdent)
    wrapped.lines <- count_lines(wrapped)
    if (max(wrapped.lines) <= lines) break
    width <- width + 1
  }
  wrapped
}

relative_luminance <- function(c) {
  cg <- ifelse(c <= 10, c/3294, (c/269 + 0.0513)^2.4)
  L <- 0.2126 * cg["red",] + 0.7152 * cg["green",] + 0.0722 * cg["blue",]
  L
}

contrast_ratio <- function(a, b) {
  # http://ux.stackexchange.com/a/82068
  rgb_a <- col2rgb(a)
  rgb_b <- col2rgb(b)
  L_a <- relative_luminance(rgb_a)
  L_b <- relative_luminance(rgb_b)
  L_big <- ifelse(L_a > L_b, L_a, L_b)
  L_small <- ifelse(L_a > L_b, L_b, L_a)
  (L_big + 0.05) / (L_small + 0.05)
}

#' Select contrasting text colors, from a list, for a set of fill colors
#'
#' Given a (possibly named) vector of fill (background) colors, this function
#' returns a (possibly named) vector of highest-contrast text (foreground)
#' colors, from a list of potential text colors (usually two is sufficient).
#'
#' @param biases added to the "true" contrast ratios to bias the selection
#'
#' @export
contrasting_colors <- function(fillcolors, textcolors=c("black", "white"), biases=c(0, 2)) {
  setNames(
    sapply(fillcolors, function(bg) {
      contrast_ratios <- sapply(textcolors, function(fg) {contrast_ratio(bg, fg)})
      textcolors[which.max(contrast_ratios + biases)]
    }),
    names(fillcolors)
  )
}
