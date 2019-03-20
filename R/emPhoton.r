# emPhoton.r
# written by JuG
# March 24 2018


#' Do something
#' @author JuG
#' @description
#' @param illum illumination profile
#' @param CountRate countrate per molecule kHz
#' @param timeScale msec
#' @details
#' @examples
#'
#' @return
#' @export


emPhoton <- function(illum, CountRate = 5, timeScale = 1, sum = FALSE, darkNoise = 2){
  #CountRate photons per ms
  lambda <- round(illum * CountRate * timeScale * 1/.4) + round( darkNoise)
  rep <- sapply(lambda, FUN = rpois, n=1)
  if(sum){
    rep <- sum(rep)
  }
  return(rep)
}
