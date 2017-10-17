#' @import ggplot2 gtable grid magrittr extrafont
#' @export
theme_wbg <- function() {
  theme_minimal()
}

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
        axis.text = element_blank()
      ) +
        theme(
          legend.position = c(0,0),
          legend.direction = "vertical",
          legend.justification = c(0, 0),
        )
    }
  )
}

#' @export
style_atlas <- function(textsize=7) {
  modifyList(style_base(textsize), list(
    theme = function() {
      theme_wbg() +
        theme(text = element_text(family = "Avenir Book", size=textsize, color="grey10"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              #legend.position = "none",
              plot.caption=element_text(hjust=0, size=rel(0.9), margin=margin(1.5,0,0,0, unit="line"), lineheight = 1),
              plot.title=element_text(hjust=0, size=rel(1.15), family="Avenir Heavy", face="bold", lineheight = 1),
              plot.subtitle = element_text(hjust=0, size=rel(1.0), lineheight = 1),
              axis.text=element_text(size=rel(1.0)),
              axis.text.y=element_text(family = "Avenir Heavy", face="bold"),
              axis.text.x=element_text(face="plain"),
              axis.title=element_blank(),
              plot.margin=unit(c(5,5,5,0),"mm"),
              legend.title = element_blank(),
              legend.key.size = unit(1.5*textsize, "points"))
    },
    theme_barchart = function() {
      theme <- style_atlas(textsize)$theme()

      theme + theme(
        axis.text.y = element_text(face="plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = theme_minimal()$panel.grid.major.x
      )
    },
    colors = list(
      spot.primary = "#cc0641",
      spot.secondary = "gray30",
      spot.secondary.light = "gray90",
      regions = c(
        EAS = rgb(223, 127, 46, maxColorValue = 255),
        ECS = rgb(206,18,73, maxColorValue = 255),
        LCN = rgb(58,148,60, maxColorValue = 255),
        MEA = rgb(127, 62, 131, maxColorValue = 255),
        NAC = rgb(77, 77, 76, maxColorValue = 255),
        SAS = rgb(32, 120, 182, maxColorValue = 255),
        SSF = rgb(255, 203, 6, maxColorValue = 255)
      ),
      categorical = c(
        "#cc0641",
        "#db8887",
        "#4d4d4c",
        "#9e9f9e",
        "#686868"
      ),
      world = "black",
      continuous = function(n) { scales::seq_gradient_pal(low = "white", high = "#cc0641")((1:n)/n) }
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

#' @export
style_worldbank.org <- function(textsize=7) {
  modifyList(style_base(textsize), list(
    theme = function() {
      theme_wbg() +
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
    theme_barchart = function() {
      theme <- style_worldbank.org(textsize)$theme()

      theme + theme(
        axis.text.y = element_text(family = "Avenir", face="plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = theme_minimal()$panel.grid.major.x
      )
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
