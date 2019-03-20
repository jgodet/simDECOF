# simPhotons.r
# written by JuG
# April 10 2018


#' Simulate photons trace
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'
#'
#' @return
#' @export


simPhotons<- function(Nfluo=100, radius = 300, len = 1000, boxSize = 1500, D = 1, CR = 100){
  #initial position of Nfluo melecules
  pos0 <- createFluo3d(Nfluo =Nfluo, r=radius, a=len, boxSize = boxSize)

  #brownian diffusion
  diffArray <-createDiffusionArray(initialPos = pos0,nSteps = 500, timeStep = 1e-3,diffusion = D)

  #illumination matrix
  illMat <- createIllumArray(diffArray)

  photons <- apply(illMat,1,emPhoton, CountRate = CR, timeScale = 1,sum=TRUE)

  return(photons)
}
