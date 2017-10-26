library(ggplot2)

theme_atlas <- function() {
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.caption=element_text(hjust=0, size=11, color="#575756", margin=margin(1.5,0,0,0, unit="line")),
    plot.title=element_text(hjust=0, size=11, face="bold", color="#575756"),
    plot.subtitle = element_text(hjust=0, size=11, color="#575756"),
    axis.text.y=element_text(size=10, face="bold", color="#575756"),
    axis.text.x=element_text(size=10, color="#575756"),
    axis.title=element_blank(),
    plot.margin=unit(c(5,5,5,0),"mm"))
}

colors_atlas_regions <- Vectorize(function(region) {
  if (region == "Sub-Saharan Africa, rural") {
    "#FFCC00"
  } else if (region == "Sub-Saharan Africa, urban") {
    "#C39F00"
  } else if (region == "South Asia, rural") {
    "#2478B6"
  } else if (region == "South Asia, urban") {
    "#1D5B89"
  } else if (region == "East Asia & Pacific, rural") {
    "#DF7F22"
  } else if (region == "East Asia & Pacific, urban") {
    "#A66117"
  } else {
    "#E3E3E3"
  } 
})

billions <- function(ROUND) { function(x) format(round(x/1e9,ROUND), nsmall=ROUND) }
millions <- function(ROUND) { function(x) format(round(x/1e6,ROUND), nsmall=ROUND) }

print_or_ggiraph <- function(p) {
  output.format <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (length(output.format) > 0 && grepl("html", output.format)) {
    ggiraph(code = {print(p)},width=1,
            width_svg = opts_current$get("fig.width"), height_svg = opts_current$get("fig.height"),
            tooltip_opacity=1.0, tooltip_extra_css="background-color: white; border: 1px solid grey; padding: 7px; border-radius: 5px; box-shadow: rgba(0, 0, 0, 0.3) 0 2px 10px;")
  } else {
    print(p)  
  }
}

# geom_atlas_labels <- Vectorize(function(x, y, x.to, y.to, label, color, hjust, size) {
#   df.labels <- data.frame(x = x, y = y, label = label, color=color)
#   ret <- list(
#     geom_text(data=df.labels,
#               aes(x=x,y=y,label=label),
#               hjust=hjust, vjust=0.5, size=size,color=color)
#    )
#    if (!is.na(x.to) && !is.na(y.to)) {
#      ret <- c(ret, list(
#        geom_segment(x = x+0.25*(hjust*2-1), y = y, xend = x.to, yend=y, colour=color, size=0.1),
#        geom_segment(x = x.to, y = y, xend = x.to, yend=y.to, colour=color, size=0.1)
#      ))
#    }
#    return(ret)
# })
