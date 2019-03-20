# createTrace.r
# written by JuG
# March 24 2018


#' Generate MCS trace
#' @author JuG
#' @description
#' @param diffArray diffusion array
#' @details
#' @examples
#'Nfluo = 100                   #nombre de fluorophores
#'radius = 350                  #bacteria radius in nm
#'len = 750                     #rod body half length in nm
#'boxSize = 1500
#'
#'pos0 <- createFluo3d(Nfluo, radius, len, boxSize)
#'diffArray <-createDiffusionArray(initialPos = pos0,nSteps = 500, timeStep = 1e-3,diffusion = 1)
#'plot(1:dim(diffArray)[1],createTrace(diffArray, CR = 5, tScale = 1), type='l', ylab="Photons",xlab='Time, ms')

#'
#' @return
#' @export


createTrace<- function(diffArray, CR = 5, tScale = 1){
  trace <- numeric()
  for (i in 1:dim(diffArray)[1]){
    illum <- PSF2p(rhoVal = rho(x = diffArray[i,1,], y =  diffArray[i,3,]), z =  diffArray[i,2,])
    trace[i] <- emPhoton(illum, CountRate = CR, timeScale = tScale, sum=TRUE)
  }
  return(trace)
}
