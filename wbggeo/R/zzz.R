#' @export
.onLoad <- function(libname, pkgname) {
  maps_loaded <- library("wbgmaps", logical.return = TRUE)
  if (!maps_loaded) {
    warning(paste(
      "The wbgmaps package is not available. Maps produced will not conform to",
      "World Bank Group conventions. To install wbgmaps, run:\n\n",
      "    devtools::install_github('worldbank/wbgviz' subdir='wbgmaps')"
    ))
    layers <- make_basic_layers()
    wbgmaps <- list(high = layers, low = layers)
  }
}
