################################################################################
# Utilities
################################################################################

ol <-   function(l)    list('(', l)
cl <-   function(l)    list('[', l)
or <-   function(r)    list(')', r)
cr <-   function(r)    list(']', r)
olor <- function(l, r) list(ol(l), or(r))
olcr <- function(l, r) list(ol(l), cr(r))
clor <- function(l, r) list(cl(l), or(r))
olcr <- function(l, r) list(ol(l), cr(r))
atom <- function(x)    list(x)

#' (Better) Convert Numeric to Factor
#'
#' Like \code{cut}, \code{supercut} divides the range of x into intervals and
#' codes the values in x according to the interval in which they fall. Unlike
#' \code{cut}, it understands mathematical interval notation so you can be very
#' precise with how the intervals are constructed.
#'
#' The intervals are strings being either a single number (e.g. "10"), which
#' will be assigned to only that value, or an interval (e.g. "[10,20]"), which
#' will be assigned to any value in that interval. Open ("(1,5)"), closed
#' ("[1,5]") and mixed ("[1,5)") intervals are supported. To create an interval
#' which is open below, use e.g. "(-Inf, 5]" and for an interval which is open
#' above use e.g. "(20, Inf)".
#'
#' If a value in \code{x} doesn't match any interval, a warning will be issued.
#'
#' If intervals overlap behaviour is unspecified (don't do this!)
#'
#' @param x a numeric vector which is to be converted to a factor by supercutting
#' @param intervals a vector of string intervals, see Details and Examples
#' @param labels labels (if not given names(intervals) will be used if set)
#' @param ordered_result logical: should the result be an ordered factor?
#'
#' @seealso \code{\link{cut}}
#'
#' @examples
#' supercut(mtcars$hp, c(
#'   "less than 110 hp" = "(-Inf,110)",
#'   "exactly 110 hp"   = "110",
#'   "110 - 200 hp"     = "(110,200)",
#'   "200 hp and over"  = "[200, Inf)"
#' ))
#'
#' @export
supercut <- function(x, intervals, labels = NULL, ordered_result = T) {
  # If we got the string form of intervals, process to list version
  if (!is.list(intervals)) {
    if (is.null(labels)) {
      if (is.null(names(intervals))) {
        labels <- intervals
      } else {
        labels <- names(intervals)
      }
    }

    intervals <- lapply(intervals, function(i) {
      if (substr(i,1,1) %in% c("(", "[")) {
        l <- if(substr(i,1,1) == "(") ol else cl
        r <- if(substr(i, nchar(i), nchar(i)) == ")") or else cr
        bounds <- as.numeric(unlist(strsplit(substr(i, 2, nchar(i)-1), ",")))
        return(list(l(bounds[1]), r(bounds[2])))
      } else {
        return(list(as.numeric(i)))
      }
    })
  } else {
    if (is.null(labels)) {
      if (is.null(names(intervals))) {
        stop("In list() mode labels must be provided as list names or in the labels argument")
      } else {
        labels <- names(intervals)
      }
    }
  }


  binned_x <- sapply(x, function(item) {
    if (is.na(item)) return(NA)
    for (i in 1:length(intervals)) {
      ival <- intervals[[i]]
      if (is.numeric(ival[[1]])) {
        if (item == ival) {
          return(labels[i])
        }
      } else {
        test_l <- if (ival[[1]][[1]] == "(") ival[[1]][[2]] < item else ival[[1]][[2]] <= item
        test_r <- if (ival[[2]][[1]] == ")") item < ival[[2]][[2]] else item <= ival[[2]][[2]]
        if (test_l & test_r) return(labels[i])
      }
    }
    warning(paste("Value", item, "did not match any interval"))
    return (NA)
  })

  binned_x <- factor(binned_x, labels, ordered=ordered_result)

  unused <- setdiff(levels(binned_x), unique(binned_x))
  if (length(unused) > 0) {
    warning(paste("Level(s)",paste(unused,collapse=","),"unused: may cause bin-color misalignment"))
  }
  return (binned_x)
}
