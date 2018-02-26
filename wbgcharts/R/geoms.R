# = Handy labelling functions ==================================================

dp <- Vectorize(function(x) {
  if (is.na(x)) return(0)
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
})

label_divide <- function(scale, big.mark = ",") {
  function(precision = NULL) {
    function(label) {
      scaled_values <- label / (10^scale)
      digits <- if (is.null(precision)) max(dp(scaled_values), na.rm = TRUE) else precision
      format(round(scaled_values, digits = digits), nsmall = digits, scientific = FALSE, big.mark = big.mark)
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

#' @export
geom_other_dotplot <- function(mapping = aes(), color.line = "black", size.line = 0.25, arrow = NULL, ...) {
  # TODO break apart both mapping and ... so that anything with .line suffix is sent
  # to line geom, and the rest to point geom.
  if (is.null(mapping$y)) {
    stop("Must provide y aesthetic directly in geom_other_dotplot even if its in ggplot call.")
  }
  c(
    geom_line(
      modifyList(mapping, setNames(mapping["y"], "group")),
      color = color.line, size = size.line, arrow = arrow, ...),
    geom_point(mapping, ...)
  )
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
