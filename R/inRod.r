# inRod.r
# written by JuG
# March 24 2018


#' Test if coordinate is in rod bacteria
#' @author JuG
#' @description
#' @param x x-position
#' @param y y-position
#' @param z z-position
#' @param r rod radius
#' @param a rod half-length
#' @details
#' Test (x*x + y*y + 1/4 * (abs(z-a) + abs(z+a) - 2*a)^2) <= (r*r)
#' @examples
#'
#'
#' @return
#' @export


inRod<- function(x,y,z,r,a){
  return((x*x + y*y + 1/4 * (abs(z-a) + abs(z+a) - 2*a)^2) <= (r*r))
}
