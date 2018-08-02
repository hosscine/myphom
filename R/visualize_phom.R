
#' Formats persistent diagram
#'
#' Solve problem caused by difference of persistent diagram format between \code{myphom} and \code{TDA} package.
#'
#' @param diag persistent diagram.
#'
#' @return formatted persistent diagram.
#'
formatDiagram <- function(diag){
  if(class(diag)=="list") return(diag[["diagram"]])
  else return(diag)
}


#' Shows persistent diagram
#'
#' @param diag persistent diagram.
#' @param maxdimention max dimension.
#' @param maxscale max scale.
#' @param cex graphics parameters.
#'
#' @export
#'
showPersistentDiagram <- function(diag = diagram, maxdimention = NULL, maxscale = NULL, cex = 1.5){
  diag <- formatDiagram(diag)
  if(missing(maxdimention)) maxdimention <- attr(diag, "maxdimension")
  if(missing(maxscale)) maxscale <- attr(diag,"scale")
  maxdimention <- maxdimention + 1
  diag[is.infinite(diag)] <- maxscale[2]

  graphics::plot(diag[, 2], diag[, 3], xlim = maxscale, ylim = maxscale,
                 cex = cex, cex.axis = cex, col = diag[, 1] + 1,
                 pch = diag[, 1] + 1, xlab = "Birth", ylab = "Death",
                 cex.lab = cex, cex.main = 2)
  graphics::abline(0, 1)

  legends <- 0
  for (i in (0:(maxdimention - 1))){
    legends <- c(legends, paste("dim", i))
  }
  legends <- legends[-1]

  graphics::legend(maxscale[2] / 2 * 1.2, maxscale[2] / 2, legend = sapply(legends, as.expression),
                   col = 1:maxdimention, pch = 1:maxdimention, cex = cex, pt.cex = cex)
}
