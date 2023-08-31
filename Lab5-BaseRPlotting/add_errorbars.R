#' Add error bars around x,y points 
#' 
#' @param x Position along x-axis
#' @param y Position along y-axis
#' @param w Width of the error bar (e.g., standard error or standard deviation)
#' @param horiz If horizontal "end caps" are desired, set to a value greater
#'   than zero. Larger values mean larger end caps.
#' @param ... Other arguments accepted by \code{arrows} (e.g., \code{lwd=} and \code{col=})
#' @return Nothing. This function has the side effect of modifying the current figure.
#' 
#' Note that \code{lwd=} controls the thickness of error bar
add_errorbars <- function(x, y, w, horiz = 0, ...) {
  arrows(x0 = x, y0 = y - w,
         x1 = x, y1 = y + w,
         code = 3, angle = 90,
         length = horiz)
}
