#' @export
style_base <- function(textsize=7) {
  list(
    labels = list(
      regions = c(
        EAS = "East Asia & Pacific",
        ECS = "Europe & Central Asia",
        LCN = "Latin America & the Caribbean",
        MEA = "Middle East & North Africa",
        NAC = "North America",
        SAS = "South Asia",
        SSF = "Sub-Saharan Africa"
      ),
      regions_wrapped = c(
        EAS = "East Asia\n& Pacific",
        ECS = "Europe\n& Central Asia",
        LCN = "Latin America\n& the Caribbean",
        MEA = "Middle East\n& North Africa",
        NAC = "North America",
        SAS = "South Asia",
        SSF = "Sub-Saharan\nAfrica"
      ),
      regions_vwrapped = c(
        EAS = "East\nAsia\n&\nPacific",
        ECS = "Europe\n&\nCentral\nAsia",
        LCN = "Latin\nAmerica\n&\nthe\nCaribbean",
        MEA = "Middle\nEast\n&\nNorth\nAfrica",
        NAC = "North\nAmerica",
        SAS = "South\nAsia",
        SSF = "Sub-\nSaharan\nAfrica"
      )
    ),
    gg_text_size = grid::convertX(grid::unit(textsize, "points"), "mm", valueOnly = TRUE),
    gg_max_point_size = grid::convertX(grid::unit(0.1, "npc"), "mm", valueOnly = TRUE),
    theme_map = function(aspect_ratio = 1) {
      t <- theme(
        panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        axis.text = element_blank()
      )

      if (aspect_ratio > 1) {
        t + theme(
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = c(1, 1),
          legend.key.width = unit(1, "lines")
        )
      } else {
        t + theme(
          legend.position = "top",
          legend.direction = "horizontal",
          legend.justification = c(0.5, 1),
          legend.key.width = unit(1.5, "lines")
        )
      }
    },
    theme_bubble_map = function() {
      theme(
        panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        axis.text = element_blank(),
        legend.position = c(0,0),
        legend.direction = "vertical",
        legend.justification = c(0, 0)
      )
    },
    theme_barchart = function() {
      theme(
        axis.text.y = element_text(face="plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = theme_minimal()$panel.grid.major.x
      )
    },
    theme_scatter = function() {
      theme(
        axis.title.x = element_text(margin = margin(1,0,0,0,"lines")),
        panel.grid.major.x = NULL
      )
    },
    theme_legend = function(position = "top") {
      listy(
        top = theme(
          legend.position = "top",
          legend.margin = margin(0,0,0.3,0, "lines")
        ),
        topleft = top + theme(legend.justification = c(0, 0.5)),
        right = theme(
          legend.position = "right",
          legend.margin = margin(0,0,0,0.5, "lines")
        ),
        righttop = right + theme(legend.justification = c(0.5, 1)),
        bottom = theme(
          legend.position = "bottom",
          legend.margin = margin(0.3,0,0,0, "lines")
        ),
        left = theme(
          legend.position = "left",
          legend.margin = margin(0,0.5,0,0, "lines")
        ),
        lefttop = left + theme(legend.justification = c(0.5, 1))
      )[position]
    }
  )
}

#' @export
style_atlas <- function(textsize=7, family="Avenir Book", family.bold = "Avenir Heavy") {
  modifyList(style_base(textsize), list(
    theme = function() {
      theme_minimal() +
        theme(text = element_text(family = family, size=textsize, color="grey10"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              plot.caption=element_text(hjust=0, size=rel(0.9), margin=margin(1.5,0,0,0, unit="line"), lineheight = 1, color = "grey70"),
              plot.title=element_text(hjust=0, size=rel(1.15), family=family.bold, face="bold", lineheight = 1),
              plot.subtitle = element_text(hjust=0, size=rel(1.0), lineheight = 1),
              axis.text=element_text(size=rel(1.0)),
              axis.text.y=element_text(family = family.bold, face="bold"),
              axis.text.x=element_text(face="plain"),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.margin = margin(1,3,5,0, unit = "mm"), #trbl
              legend.box.spacing = unit(0.2, "lines"),
              legend.margin = margin(0,0,0.3,0, "lines"),
              legend.title = element_blank(),
              legend.key.size = unit(1.5*textsize, "points"),
              legend.text = element_text(size = rel(1.0), lineheight = 0.8),
              legend.position = "none",
              strip.text = element_text(size = rel(1.0))
        )
    },
    colors = listy(
      text = "grey10",
      text.inverse = "white",
      spot.primary = "#cc0641",
      spot.secondary = "gray30",
      spot.primary.light = lighten(spot.primary),
      spot.primary.dark = darken(spot.primary),
      spot.secondary.light = lighten(spot.secondary),
      spot.secondary.dark = darken(spot.secondary),
      regions = c(
        EAS = rgb(223, 127, 46, maxColorValue = 255),
        ECS = rgb(206,18,73, maxColorValue = 255),
        LCN = rgb(58,148,60, maxColorValue = 255),
        MEA = rgb(127, 62, 131, maxColorValue = 255),
        NAC = rgb(77, 77, 76, maxColorValue = 255),
        SAS = rgb(32, 120, 182, maxColorValue = 255),
        SSF = rgb(255, 203, 6, maxColorValue = 255)
      ),
      world = c(WLD = "black"),
      regions.light = rgba2rgb(regions, alpha = 0.7, background = "white"),
      regions.dark = rgba2rgb(regions, alpha = 0.7, background = "black"),
      incomes = c(
        HIC = spot.primary,
        UMC = spot.primary.light,
        LMC = spot.secondary.light,
        LIC = spot.secondary
      ),
      categorical = c(
        spot.primary,
        spot.primary.light,
        "#4d4d4c",
        "#9e9f9e",
        "#686868"
      ),
      continuous.primary = function(n) { scales::gradient_n_pal(c("white", spot.primary.light, spot.primary, spot.primary.dark))((1:n)/n) },
      continuous.secondary = function(n) { scales::gradient_n_pal(c("white", spot.secondary.light, spot.secondary, spot.secondary.dark))((1:n)/n) },
      continuous = continuous.primary
    ),
    shapes = list(
      categorical = c(
        19,
        19,
        1,
        1,
        1
      )
    ),
    linetypes = list(
      regions = c(
        EAS = "solid",
        ECS = "solid",
        LCN = "solid",
        MEA = "solid",
        NAC = "solid",
        SAS = "solid",
        SSF = "solid"
      ),
      world = c(WLD = "12")
    ),
    arrow = function(ends = "last") { grid::arrow(length = unit(1.5, "mm"), type = "closed", ends = ends) }
  ))
}

#' @export
style_atlas_cmyk <- function(textsize=7, family="Avenir Book", family.bold = "Avenir Heavy") {
  modifyList(style_atlas(textsize, family, family.bold), list(
    colors = listy(
      spot.primary = cmyk(2.7, 100, 58.6, 12.2, maxColorValue = 100), #"#https://data.worldbank.org/indicator/SH.STA.STNT.MA.ZS?locations=BD",
      spot.secondary = cmyk(0, 0, 0, 80, maxColorValue = 100),
      spot.primary.light = cmyk(1.3, 50, 29.3, 6.1, maxColorValue = 100),
      spot.primary.dark = darken(spot.primary),
      spot.secondary.light = cmyk(0, 0, 0, 50, maxColorValue = 100),
      spot.secondary.dark = darken(spot.secondary),
      regions = c(
        EAS = cmyk(0, 55, 90, 10, maxColorValue = 100), #rgb(223, 127, 46, maxColorValue = 255),
        ECS = cmyk(2.7, 100, 58.6, 12.2, maxColorValue = 100), #rgb(206,18,73, maxColorValue = 255),
        LCN = cmyk(72, 5, 100, 20, maxColorValue = 100), #rgb(58,148,60, maxColorValue = 255),
        MEA = cmyk(45, 83, 0, 20, maxColorValue = 100), #rgb(127, 62, 131, maxColorValue = 255),
        NAC = cmyk(0, 0, 0, 80, maxColorValue = 100), #rgb(77, 77, 76, maxColorValue = 255),
        SAS = cmyk(80, 40, 0, 10, maxColorValue = 100), #rgb(32, 120, 182, maxColorValue = 255),
        SSF = cmyk(0, 20, 100, 0, maxColorValue = 100) #rgb(255, 203, 6, maxColorValue = 255)
      ),
      world = c(WLD = "black"),
      regions.light = rgba2rgb(regions, alpha = 0.7, background = "white"),
      regions.dark = rgba2rgb(regions, alpha = 0.7, background = "black"),
      categorical = c(
        spot.primary,
        spot.primary.light,
        "#4d4d4c",
        "#9e9f9e",
        "#686868"
      ),
      continuous = function(n) { scales::gradient_n_pal(c("white", spot.primary.light, spot.primary, spot.primary.dark))((1:n)/n) }
    )))
}


#' @export
style_atlas_open <- function(textsize=7) {
  style_atlas(textsize=textsize, family="Nunito Sans", family.bold = "Nunito Sans")
}


#' @export
style_worldbank.org <- function(textsize=7) {
  modifyList(style_base(textsize), list(
    theme = function() {
      theme_minimal() +
        theme(text = element_text(family = "Open Sans", size = textsize, color="#333333"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              #legend.position = "none",
              plot.caption=element_text(hjust=0, size=rel(0.9), margin=margin(1.5,0,0,0, unit="line")),
              plot.title=element_text(hjust=0, size=rel(1.15), face="bold"),
              plot.subtitle = element_text(hjust=0,size=rel(1.0)),
              axis.text=element_text(size=rel(1.0)),
              axis.text.y=element_text(face="bold"),
              axis.title=element_blank(),
              plot.margin=unit(c(5,5,5,0),"mm"),
              legend.title = element_blank())
    },
    colors = list(
      spot.primary = "#0071bc",
      spot.secondary = "#009fda",
      spot.secondary.light = "#a5e8ff",
      regions =
        c(EAS = "#0071bc",
          ECS = "#009fda",
          LCN = "#a5e8ff",
          MEA = "#0071bc",
          NAC = "#009fda",
          SAS = "#a5e8ff",
          SSF = "#0071bc"),
      categorical = c(
        "#0071bc",
        "#009fda",
        "#a5e8ff",
        "#9e9f9e",
        "#686868"
      ),
      world = "black",
      continuous = function(n) { scales::seq_gradient_pal(low = "white", high = "#0071bc")((1:n)/n) }
    ),
    shapes = list(
      categorical = c(
        19,
        19,
        1,
        1,
        1
      )
    )
  ))
}
