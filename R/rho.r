# rho.r
# written by JuG
# March 24 2018


#' Compute rho - distance from ref point
#' @author JuG
#' @description
#' @param x x-coordinates
#' @param y y-coordinates
#' @param x-ref x ref coordinate
#' @param y-ref y ref coordinate
#' @details
#' @examples
#' @return
#' @export

rho <- function(x,y,xref=0, yref=0){
  return(sqrt((x-xref)**2 + (y-yref)**2))
}
