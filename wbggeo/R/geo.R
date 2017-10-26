make_basic_layers <-function() {
  world <- ggplot2::map_data("world")
  world.names <- data.frame(region = unique(world$region), iso3c = iso.alpha(unique(world$region), 3), stringsAsFactors = F)
  world.names$iso3c[world.names$region == "Kosovo"] <- "XKX"
  world$id <- world.names$iso3c[match(world$region, world.names$region)]
  # This produces pretty bad centroids, really, so it's last ditch...
  world_centroids <- aggregate(cbind(long,lat) ~ id, world[is.na(world$subregion),], mean)
  world_centroids$x <- world_centroids$long
  world_centroids$y <- world_centroids$lat
  world$region <- NULL
  world$subregion <- NULL
  list(
    countries = world,
    country_centroids = world_centroids,
    disputed = data.frame(long = NA, lat = NA, group = NA),
    boundaries = data.frame(long = NA, lat = NA, group = NA, lineend = NA, linetype = NA),
    lakes = data.frame(long = NA, lat = NA, group = NA),
    coastline = data.frame(long = NA, lat = NA, group = NA)
  )
}

#' @export
geom_bubble_map <- function(mapping = NULL, data = NULL, ..., centroids, sort_size = TRUE) {
  mapping <- modifyList(mapping, aes(x = long, y = lat))
  if(is.function(data)) stop("Don't currently support data as function")
  datafun <- function(d) {
    d <- d %>% left_join(centroids, by = setNames("id", as.character(mapping$map_id)))
    if (sort_size && !is.null(mapping$size)) {
      d <- d %>% arrange_(paste0("-",as.character(mapping$size)))
    }
  }
  #geom_point(mapping, data = if (is.null(data)) datafun else datafun(data), ...)
  layer(geom = "point", stat = "identity", data = if (is.null(data)) datafun else datafun(data),
        mapping = mapping, position = "identity", params = list(...), check.aes = FALSE)
}

#standard_crop_wintri <- list(
#  left=-12000000, right=16396891,
#  top=10018754, bottom=-6800000
#)
#' @export
standard_crop_wintri <- function() {
  l <- list(
    left=-12000000, right=16396891,
    top=9400000, bottom=-6500000
  )
  l$xlim <- c(l$left, l$right)
  l$ylim <- c(l$bottom, l$top)
  l
}

# Average two colors (used for some disputed areas)
average_color <- function(c1, c2) {
  rgb1 = col2rgb(c1)
  rgb2 = col2rgb(c2)

  # If both colors are the same avoid color mismatch caused by rounding error
  if (all(rgb1 == rgb2)) {
    return(c1)
  }

  rgb(t(rgb1+rgb2)/2/256)
}

disputed_fill <- function(pb, id, newfill = NULL) {
  for (i in 1:length(pb$data)) {
    if (!("map_id" %in% colnames(pb$data[[i]])))
      next
    if (id %in% pb$data[[i]]$map_id) {
      if (!is.null(newfill)) {
        pb$data[[i]]$fill[pb$data[[i]]$map_id == id] <- newfill
        return(pb)
      } else {
        return(pb$data[[i]]$fill[pb$data[[i]]$map_id == id])
      }
    }
  }
  warning(paste("Did not find ID",id,"in ggplot layers"))
  if (!is.null(newfill)) {
    return(pb)
  } else {
    return(NA)
  }
}

#' @export
wbg_color_disputed <- function(p) {
  pb <- ggplot_build(p)

  chn_ind <- average_color(disputed_fill(pb, "IND"), disputed_fill(pb, "CHN"))
  ind_pak <- average_color(disputed_fill(pb, "IND"), disputed_fill(pb, "PAK"))
  sdn_ssd <- average_color(disputed_fill(pb, "SDN"), disputed_fill(pb, "SSD"))

  pb <- disputed_fill(pb, "Arunachal Pradesh", chn_ind)
  pb <- disputed_fill(pb, "Aksai Chin", chn_ind)
  pb <- disputed_fill(pb, "Demchok", chn_ind)

  pb <- disputed_fill(pb, "Azad Kashmir", ind_pak)
  pb <- disputed_fill(pb, "Jammu and Kashmir", ind_pak)
  pb <- disputed_fill(pb, "Northern Areas", ind_pak)
  pb <- disputed_fill(pb, "Siachen Glacier", ind_pak)

  pb <- disputed_fill(pb, "Abyei", sdn_ssd)

  return(ggplot_gtable(pb))
}

#' @export
rename_na <- function(na.label) { function(l) {ifelse(is.na(l), na.label, l)}}
