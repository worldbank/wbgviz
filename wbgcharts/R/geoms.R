# = Handy labelling functions ==================================================

dp <- Vectorize(function(x) {
  if (is.na(x)) return(0)
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
})

label_divide <- function(scale) {
  function(precision = NULL, big.mark = ",", always.signed = FALSE) {
    function(label) {
      scaled_values <- label / (10^scale)
      digits <- if (is.null(precision)) max(dp(scaled_values), na.rm = TRUE) else precision
      strs <- format(round(scaled_values, digits = digits), nsmall = digits, scientific = FALSE, big.mark = big.mark)
      strs <- trimws(strs)
      if (always.signed) {
        strs <- paste0(ifelse(scaled_values > 0, "+", ""), strs)
      }
      strs
    }
  }
}

#' @export
ones = label_divide(0)
#' @export
thousands = label_divide(3)
#' @export
millions = label_divide(6)
#' @export
billions = label_divide(9)
#' @export
trillions = label_divide(12)

#' A breaks function for ggplot2 scales that shows the data limits
#'
#' This builds on the standard ggplot2 continuous breaks (extended breaks)
#' function to make sure the data limits, given as a parameter, are included
#' in the breaks.
#'
#' By default it will try to use the limits ggplot provides, but by default those
#' are extended by the expand argument of scales, so it will use the integral
#' ceiling and floor respectively of these. Usually this will work. If not you
#' can provide limits to override.
#'
#' @param limits A pair of limits, overrides the default
#' @param at To limit the breaks to be on 5s, 10s, etc (more of a hint)
#' @param closeness_threshold If one of the data limit breaks and one of the
#'        default breaks will be closer than closeness_threshold * the ggplot2
#'        calculated limits, the the nearby default break will be dropped.
#'
#' @example
#' library(ggplot2)
#' library(dplyr)
#'
#' housing <- txhousing %>%
#'   filter(month == 1, year > 2001, year < 2015)
#'
#' # Standard extended_breaks don't look great on this example
#' p <- ggplot(housing, aes(year, volume, color = city)) +
#'   geom_line() +
#'   theme(legend.position="none")
#' p
#'
#' # Bracketed breaks work nicely
#' p + scale_x_continuous(breaks = bracketed_breaks())
#' @export
bracketed_breaks <- function(limits, at = 1, closeness_threshold = 0.1, breaker = scales::extended_breaks()) {
  if (missing(limits)) {
    limits <- NULL
  } else {
    limits <- range(limits, na.rm=TRUE)
  }
  if (missing(at)) {
    at <- NULL
  }

  function(x) {
    if (is.null(limits)) {
      limits <- c(ceiling(x[1]), floor(x[2]))
    }

    mingap <- diff(x) * closeness_threshold

    if (!is.null(at)) {
      x <- range(x)/at
      x[1] <- floor(x[1])*at
      x[2] <- ceiling(x[2])*at
    }

    inner <- breaker(x)
    inner <- inner[inner > limits[1] & inner < limits[2]]

    if (inner[1] - limits[1] < mingap) {
      inner <- inner[-1]
    }

    if (limits[2] - inner[length(inner)] < mingap) {
      inner <- inner[-length(inner)]
    }

    sort(c(limits, inner))
  }
}

# = Pretty simple "compound" geoms =============================================

#' TODO: option to only line adjacent areas with same fill
#' @export
geom_lined_area <- function(color = "white", size = 0.35, ...) {
  c(
    geom_area(...),
    geom_line(color = color, size = size, ...)
  )
}

#' @export
geom_percent_col <- function(mapping = aes(), fill = NULL, fill.bg = "gray80", ...) {
  c(
    geom_col(modifyList(mapping, aes(y = 100)), fill = fill.bg, ...),
    geom_col(mapping, fill = fill, ...)
  )
}

#' A geom for the other kind of dotplot, with a connecting line
#'
#' This is still a fairly hacky way to make dotplots, so you have to observe
#' and obey some weirdness, which is outlined below. Also, the "direction"
#' (usually time) of the data is dictated by the row ordering in the data frame,
#' so make sure it is sorted correctly.
#'
#' @param mapping Complicatedly, you must specify x (the value axis), y (the
#'   categorical axis) and group in the geom's own mapping argument, even if
#'   you specify them in the main ggplot() call. If the chart is faceted, you
#'   need to also include the facetting variable as part of the group (use paste0).
#' @param linecolor The color of the grouping line
#' @param linesize The size of the grouping line
#' @param arrow NA or FALSE for no arrow, TRUE for a standard arrow, or pass
#'   a grob for something weird and wonderful. For reverse direction groups, the
#' @param flip.legend By default the legend will show the arrow facing forward,
#'   but if most the groups run backwards it may be helpful to show the arrow
#'   backwards too, so you can set this to TRUE to do that.
#' @param size The size of the points & arrows if used.
#'
#' @export
geom_other_dotplot <- function(mapping = aes(), linecolor = "black", linesize = 0.25, arrow = NA, flip.legend = FALSE, size = 1,...) {
  # TODO break apart both mapping and ... so that anything with .line suffix is sent
  # to line geom, and the rest to point geom.
  if (is.null(mapping$y)) {
    stop("Must provide x aesthetic directly in geom_other_dotplot even if its in ggplot call.")
  }

  flipper <- function(df) {
    df %>% group_by(!! mapping$group) %>% mutate(hflip = last(!! mapping$x) < first(!! mapping$x))
  }

  layers <- c(
    geom_path(
      modifyList(mapping, setNames(mapping["y"], "group")),
      color = linecolor, size = linesize, ...)
  )

  if (!is.na(arrow) && arrow != FALSE) {
    if (arrow == TRUE) {
      arrow <- grid::polygonGrob(x = c(0.2,1,0.2,0.2), y = c(0, 0.5, 1, 0))
    }
    layers <- c(
      layers,
      geom_custom_point(
        aes(hflip = hflip),
        data = flipper,
        custom.shapes = list(`99` = arrow),
        size = size,
        flip.legend = flip.legend
      )
    )
  } else {
    layers <- c(layers, geom_point(mapping, size = size, ...))
  }
  layers
}

#' @export
geom_other_dotplot_label <- function(mapping = aes(), data = NULL, side = "left", ...) {
  if (!is.null(data)) {
    stop("This geom does not yet support its own data (wouldn't be hard to add")
  }
  FUN <- list(left = min, right = max)
  HJUST <- list(left = 1, right = 0)

  if (is.null(mapping$y)) {
    stop("Must provide y aesthetic directly in geom_other_dotplot even if its in ggplot call.")
  }
  mapping <- modifyList(mapping, setNames(mapping["y"], "group"))
  if (is.null(mapping$label)) {
    mapping <- modifyList(mapping, setNames(mapping["y"], "label"))
  }
  data_fun <- function(d) {
    d %>% group_by_(mapping$y) %>% filter_(paste0(mapping$x," == ",FUN[side],"(",mapping$x,")")) %>% filter(row_number() == 1) %>% ungroup
  }
  geom_text(mapping, data = data_fun, hjust = HJUST[side], ...)
}

# = Geoms built "from scratch" =================================================

# - customPoints ---------------------------------------------------------------

#' @export
customPointsGrob <- function (x, y, pgrobs, hflip = FALSE, vflip = FALSE, hjust = 0.5,
                              size = unit(1, "char"), default.units = "native", name = NULL,
                              gp = gpar(), vp = NULL)
{
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x = x, y = y, pch = 1,
       pgrobs = pgrobs, hflip = hflip, vflip = vflip, hjust = hjust,
       size = size, name = name, gp = gp,
       vp = vp, cl = "custompoints")
}

#' @method drawDetails custompoints
#' @export
drawDetails.custompoints <- function(x, recording = TRUE) {
  if (length(x$size) == 1) x$size <- rep(x$size, length(x$pgrobs))
  if (length(x$hflip) == 1) x$hflip <- rep(x$hflip, length(x$pgrobs))
  if (length(x$vflip) == 1) x$vflip <- rep(x$vflip, length(x$pgrobs))
  if (length(x$hjust) == 1) x$hjust <- rep(x$hjust, length(x$pgrobs))
  if (length(x$gp) == 1) x$gp <- rep(x$gp, length(x$pgrobs))
  for (i in 1:length(x$pgrobs)) {
    pushViewport(viewport(
      x = x$x[i], y = x$y[i],
      width=(0.5 - x$hflip[i])*2 * x$size[i],
      height=(0.5 - x$vflip[i])*2 * x$size[i],
      just = x$hjust[i],
      gp = x$gp[i]
    ))
    grid::grid.draw(grid::forceGrob(x$pgrobs[[i]]))
    popViewport()
  }
}

#' @export
geom_custom_point <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              custom.shapes = list(),
                              flip.legend = FALSE,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCustomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      custom.shapes = custom.shapes,
      flip.legend = flip.legend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCustomPoint <- ggproto("GeomPoint", GeomPoint,
                           required_aes = c("x", "y"),
                           non_missing_aes = c("size", "shape", "colour"),
                           default_aes = aes(
                             shape = 19, colour = "black", size = 1.5, fill = NA,
                             alpha = NA, stroke = 0.5, hflip = FALSE
                           ),

                           draw_panel = function(data, panel_params, coord, custom.shapes = list(), flip.legend = FALSE, na.rm = FALSE) {
                             custom.data <- data %>% filter(shape %in% names(custom.shapes))
                             default.data <- data %>% filter(!(shape %in% names(custom.shapes)))

                             custom.coords <- coord$transform(custom.data, panel_params)
                             default.coords <- coord$transform(default.data, panel_params)

                             ggplot2:::ggname("geom_custom_point",
                                              grobTree(
                                                pointsGrob(
                                                  default.coords$x, default.coords$y,
                                                  pch = default.coords$shape,
                                                  gp = gpar(
                                                    col = alpha(default.coords$colour, default.coords$alpha),
                                                    fill = alpha(default.coords$fill, default.coords$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = default.coords$size * .pt + default.coords$stroke * .stroke / 2,
                                                    lwd = default.coords$stroke * .stroke / 2
                                                  )
                                                ),
                                                customPointsGrob(
                                                  custom.coords$x, custom.coords$y,
                                                  pgrobs = custom.shapes[match(custom.coords$shape, names(custom.shapes))],
                                                  hflip = custom.coords$hflip,
                                                  hjust = 1,
                                                  size = unit(custom.coords$size, "mm"),
                                                  gp = gpar(
                                                    col = alpha(custom.coords$colour, custom.coords$alpha),
                                                    fill = alpha(custom.coords$fill, custom.coords$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = custom.coords$size * .pt + custom.coords$stroke * .stroke / 2,
                                                    lwd = custom.coords$stroke * .stroke / 2
                                                  )
                                                )
                                              )
                             )
                           },

                           draw_key = function (data, params, size)
                           {
                             custom.shapes <- params$custom.shapes
                             if (data$shape[1] %in% names(custom.shapes)) {
                               customPointsGrob(
                                 0.5, 0.5,
                                 pgrobs = custom.shapes[match(data$shape, names(custom.shapes))],
                                 hflip = params$flip.legend,
                                 gp = gpar(
                                   col = alpha(data$colour, data$alpha),
                                   fill = alpha(data$fill, data$alpha),
                                   fontsize = data$size * .pt + data$stroke * .stroke/2,
                                   lwd = data$stroke * .stroke/2
                                 )
                               )
                             } else {
                               ggplot2::draw_key_point(data, params, size)
                             }
                           }
)

# - wrapText -------------------------------------------------------------------

#' @export
wraptextGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                         just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
                         default.units = "npc", name = NULL, gp = gpar(), vp = NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(label = label, x = x, y = y, just = just, hjust = hjust,
       vjust = vjust, rot = rot, check.overlap = check.overlap,
       name = name, gp = gp, vp = vp, cl = "wraptext")
}

#' @export
#' @method drawDetails wraptext
drawDetails.wraptext <- function(x, recording) {
  x$label <- RGraphics::splitString(x$label)
  class(x) <- "text"
  drawDetails(x, recording)
}

#' @export
#' @method heightDetails wraptext
heightDetails.wraptext <- function(x) {
  tg <- do.call(textGrob, modifyList(x, list(label = RGraphics::splitString(x$label))))
  heightDetails(tg)
}

#' @export
#' @method widthDetails wraptext
widthDetails.wraptext <- function(x) {
  tg <- do.call(textGrob, modifyList(x, list(label = RGraphics::splitString(x$label))))
  widthDetails(tg)
}

# - geom_bartext ---------------------------------------------------------------

#' @export
geom_bartext <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      padding = unit(0.3, "line"),
                      color.inside = "white",
                      color.outside = "black",
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      padding = padding,
      color.inside = color.inside,
      color.outside = color.outside,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}


#' @export
GeomBarText <- ggplot2::ggproto("GeomBarText", ggplot2::Geom,
                    required_aes = c("x", "y"),

                    default_aes = ggplot2::aes(
                      colour = "black", size = 3.88, angle = 0, #hjust = 0.5,
                      vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                      label = NA
                    ),

                    draw_panel = function(data, panel_params, coord, parse = FALSE, padding = unit(0.3, "line"),
                                          color.inside = "white", color.outside = "black",
                                          na.rm = FALSE, check_overlap = FALSE) {
                      lab <- ifelse(is.na(data$label), data$y, data$label)
                      if (parse) {
                        lab <- parse(text = as.character(lab))
                      }

                      zero <- coord$transform(data.frame(y = 0), panel_params)
                      data <- coord$transform(data, panel_params)
                      if (is.character(data$vjust)) {
                        data$vjust <- compute_just(data$vjust, data$y)
                      }
                      #if (is.character(data$hjust)) {
                      #  data$hjust <- compute_just(data$hjust, data$x)
                      #}

                      bartextGrob(
                        lab,
                        data$x, data$y,
                        maxwidth = data$x - zero$x,
                        padding = padding,
                        color.inside = alpha(color.inside, data$alpha),
                        color.outside = alpha(color.outside, data$alpha),
                        default.units = "native",
                        hjust = data$hjust, vjust = data$vjust,
                        rot = data$angle,
                        gp = gpar(
                          col = alpha(data$colour, data$alpha),
                          fontsize = data$size * .pt,
                          fontfamily = data$family,
                          fontface = data$fontface,
                          lineheight = data$lineheight
                        ),
                        check.overlap = check_overlap
                      )
                    },

                    draw_key = ggplot2::draw_key_text
)

#' @export
bartextGrob <- function (label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                         maxwidth = unit(1, "npc"), padding = unit(0.5, "line"),
                         color.inside = "white", color.outside = "black",
                          just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
                          default.units = "npc", name = NULL, gp = gpar(), vp = NULL)
{
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(maxwidth))
    maxwidth <- unit(maxwidth, default.units)
  if (!is.unit(padding))
    padding <- unit(padding, default.units)
  grob(label = label, x = x, y = y,
       maxwidth = maxwidth, padding = padding, color.inside = color.inside, color.outside = color.outside,
       just = just, hjust = hjust,
       vjust = vjust, rot = rot, check.overlap = check.overlap,
       name = name, gp = gp, vp = vp, cl = "bartext")
}

#' @export
#' @method drawDetails bartext
drawDetails.bartext <- function(x, recording) {
  maxwidth <- convertWidth(x$maxwidth, "native", valueOnly = TRUE)
  padding <- convertWidth(x$padding, "native", valueOnly = TRUE)
  width <- convertWidth(stringWidth(x$label), "native", valueOnly = TRUE) + padding * 2
  xval <- convertX(x$x, "native", valueOnly = TRUE)
  yval <- convertY(x$y, "native", valueOnly = TRUE)

  outside <- ifelse(width > maxwidth, TRUE, FALSE)
  hjust <- ifelse(outside, 0, 1)
  xval <- ifelse(outside, xval + padding, xval - padding)

  all <- data.frame(label = x$label, x = xval, y = yval, hjust = hjust,
                    vjust = x$vjust, rot = x$rot,
                    check.overlap = x$check.overlap)

  # TODO: redo - it turns out gpar takes vectors so this is all unnecessary!
  if (any(outside)) {
    grid.text(all$label[outside], x = unit(all$x[outside], "native"), y = unit(all$y[outside], "native"), hjust = all$hjust[outside],
              vjust = all$vjust[outside], rot = all$rot[outside],
              check.overlap = all$check.overlap[outside],
              gp=gpar(col = x$color.outside))
  }
  if (any(!outside)) {
    grid.text(all$label[!outside], x = unit(all$x[!outside], "native"), y = unit(all$y[!outside], "native"), hjust = all$hjust[!outside],
              vjust = all$vjust[!outside], rot = all$rot[!outside],
              check.overlap = all$check.overlap[!outside],
              gp=gpar(col = x$color.inside))
  }
}

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

# - position_bullet ------------------------------------------------------------

#' @export
position_bullet <- function(width = NULL, preserve = c("total", "single")) {
  ggplot2::ggproto(NULL, PositionBullet,
          width = width,
          preserve = match.arg(preserve)
  )
}

#' @export
PositionBullet <- ggplot2::ggproto("PositionBullet", ggplot2::Position,
  width = NULL,
  preserve = "total",
  setup_params = function(self, data) {
   if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
     warning("Width not defined. Set with `position_dodge(width = ?)`",
             call. = FALSE)
   }

   if (identical(self$preserve, "total")) {
     n <- NULL
   } else {
     n <- max(table(data$xmin))
   }

   list(
     width = self$width,
     n = n
   )
  },

  setup_data = function(self, data, params) {
   if (!"x" %in% names(data) & all(c("xmin", "xmax") %in% names(data))) {
     data$x <- (data$xmin + data$xmax) / 2
   }
   data
  },

  compute_panel = function(data, params, scales) {
   ggplot2:::collide(
     data,
     params$width,
     name = "position_bullet",
     strategy = pos_bullet,
     n = params$n,
     check.width = FALSE
   )
  }
)

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_bullet <- function(df, width, n = NULL) {
  if (is.null(n)) {
    n <- length(unique(df$group))
  }

  if (n == 1)
    return(df)

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  d_width <- max(df$xmax - df$xmin)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x #+ width * ((groupidx - 0.5) / n - .5)
  df$xmin <- df$x - (d_width / 2) / n * groupidx
  df$xmax <- df$x + (d_width / 2) / n * groupidx

  df
}
