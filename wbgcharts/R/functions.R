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
    gp = text_element_to_gp(ggplot2::calc_element('plot.title', theme))
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
    gp = text_element_to_gp(ggplot2::calc_element('plot.subtitle', theme))
  )
}

#' @importFrom RGraphics splitTextGrob
make_note <- function(theme, note = NULL) {
  if (is.null(note))
    return(zeroGrob())

  wraptextGrob(
    label = note,
    x = unit(0, "npc"),
    y = unit(1, "npc"),
    hjust = 0, vjust = 1,
    gp = text_element_to_gp(ggplot2::calc_element('plot.caption', theme))
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
    gp = text_element_to_gp(ggplot2::calc_element('plot.caption', theme))
  )
}

make_source_url <- function(theme, source_url = NULL) {
  if (is.null(source_url))
    return(zeroGrob())

  gp <- text_element_to_gp(ggplot2::calc_element('plot.caption', theme))
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

#' @importFrom png readPNG
make_logo <- function(theme, show.logo = TRUE) {
  logo <- readPNG(system.file("logo-wb-header-en.png", package="wbgcharts"))

  if (show.logo) {
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
    logoGrob = zeroGrob()
  }

  gTree(
    children = gList(logoGrob),
    gp = text_element_to_gp(ggplot2::calc_element('plot.caption', theme))
  )
}

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
    )
  ) %>%
    gtable_add_grob(grob_title, 1, 1, 1, 2) %>%
    gtable_add_grob(grob_subtitle, 2, 1, 2, 2) %>%
    gtable_add_grob(grob_plot, 3, 1, 3, 2) %>%
    gtable_add_grob(grob_note, 4, 1, 4, 2) %>%
    gtable_add_grob(grob_source, 5, 1, 5, 2) %>%
    gtable_add_grob(grob_source_url, 6, 1, clip="on") %>%
    gtable_add_grob(grob_logo, 6, 2, clip="on") %>%
    gtable_add_padding(unit(0.025, "npc"))

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
  pgs = lapply(lapply(list(...), ggplotGrob), grid.force)
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
    getGrob(pg, gPath("title", "GRID.text"), grep=T)
  })
  panel_titles <- as.list(getGrob(pgs[[1]], gPath("GRID.stripGrob"), grep=T, global=T))

  chart <- gtable(
    unit.c(ylabels$width, unit(rep_len(width.ratio,panel_count*plot_count), "null")),
    unit.c(grobHeight(panel_titles[[1]])*1.2, grobHeight(plot_titles[[1]])*1.2, unit(1, 'null'))
  )

  chart <- gtable_add_grob(chart, ylabels, 3, 1)
  for (panel_id in 1:panel_count) {
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
#' @import grid
current_aspect_ratio <- function() {
  return(
    grid::convertWidth(unit(1, "npc"), "in", valueOnly = TRUE) /
    grid::convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
  )
}
