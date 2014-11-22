library(roxygen2)
library(devtools)

#' mapBetween
#' 
#' Map a vector (e.g., vertex sizes) proportionally or via log transform
#' to a given real interval.
#' 
#' @param x  an n-length vector of numeric values
#' @param minmax  a vector of two positive numeric values defining the interval
#' onto which x is mapped, e.g. c(1,20)
#' @param log  logical to do log transform; default FALSE
#' @return a vector same length as x argument
#' @details 
#' This function (f: Rd -> Rd) maps a vector proportionally or via log transform
#' to a given real interval. Output values of -Inf are coded as the minimum of
#' the minmax argument. Nonpositive (x_i < 0) elements of input vector are okay
#' since log transform is done after mapping to minmax interval, which must be
#' positive. Use log transform to even out the intervals between elements in x,
#' for example to make more clear sizes of graph vertices.
#' @author Stephen Downing
#' @examples
#' \dontrun{
#' ## simple example
#' x1 <- 90:100 
#' mapBetween(x = x1, minmax = c(1,20), log = F)
#' ## log transform with negative input values
#' x2 <- -5:5
#' mapBetween(x = x2, minmax = c(0,1), log = T)
#' ## log transform with negative decimal values
#' x3 <- seq(from = -1.5, to = 1.5, by = .1)
#' mapBetween(x = x3, minmax = c(1,2), log = T)
#' }
#' @export
mapBetween <- function(x,   #vector of degrees
                       minmax=c(1,20),  # vector of length 2: min and max
                       log=F           # logicial for log transform
) { 
  if (any(minmax < 0)) stop ("Negative output range is not allowed.\nPlease assign minmax argument as vector of 2 non-negative values (x>=0) and rerun function.")
  n <- length(x)
  dist <- max(x) - min(x)  #scalar
  #output
  M <- max(minmax)
  m <- min(minmax)
  range <- M - m   # interval to be mapped to
  scale <- (x-min(x)) / dist 
  
  if(log) {
    if(all(x>=1 | x==0)) {
      lx <- log(x)
      maxlx <- max(lx[lx<Inf & lx>-Inf])
      scale <- lx / maxlx
      y <- m + range*scale
      y[is.na(y)|is.nan(y)|y==-Inf|y==Inf] <- m
    } else {
      #augment x proportions to > 1 yielding values suitable for log transform
      scalepos <- (scale+1)/min(scale+1)
      #account for log transform while still proportional to elements of x 
      # by normalizing with the log of the maximum
      scale <- log(scalepos) / log(max(scalepos))
      y <- m + range*scale
    }
  } else {
    y <- m + range*scale
  }
  return(y)
}
