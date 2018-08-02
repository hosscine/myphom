
#' Calculate persistent diagram of point cloud \code{X}
#'
#' This function overwrites global variant \code{diagram} to prevent to lose calculateion result of persistent diagram.
#'
#' @param X target data.
#' @param maxdimension max dimension.
#' @param maxscale max scale.
#' @param plot if \code{T}, plot diagram.
#' @param ret if \code{T}, return diagram.
#'
#' @return persistent diagram.
#' @export
#'
#' @examples
#'
#' library(TDA)
#'
#' circle <- circleUnif(100)
#'
#' circle.diag <- calcPhom(circle, maxdimension = 1, maxscale = 1)
#'
calcPhom <- function(X, maxdimension, maxscale, plot = T, ret = T){
  diagram <<- TDA::ripsDiag(X, maxdimension = maxdimension, maxscale = maxscale)
  if(plot) showPersistentDiagram(diagram)
  if(ret) return(diagram)
}

#' Calculate persistent diagram of subsample of point cloud \code{X}
#'
#' @param X target data.
#' @param maxdimension max dimension.
#' @param maxscale max scale.
#' @param rate sabsampling rate. larger rate takes large subsample.
#' @param plot if \code{T}, plot diagram.
#' @param ret if \code{T}, return diagram.
#'
#' @importFrom assertthat assert_that
#'
#' @return persistent diagram.
#' @export
#'
calcSubsamplePhom <- function(X, maxdimension, maxscale, rate = 0.5, plot = T, ret = F){
  assert_that(assertthat::is.number(rate) && rate <= 1 && rate >= 0)
  subX <- X[sample(nrow(X), nrow(X) * rate),]
  calcPhom(subX, maxdimension, maxscale, plot, ret)
}
