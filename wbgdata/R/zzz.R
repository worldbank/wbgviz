.onLoad <- function(libname, pkgname) {
  create_wbgref()

  # wbstats is not yet updated for the (officially unannounced) API v2, but
  # there are bugs in v1 that are resolved in v2, so we override things to
  # point there
  assignInNamespace("wburls",wburls.v2,"wbstats")
}
