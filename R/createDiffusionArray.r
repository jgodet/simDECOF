# createDiffusionArray.r
# written by JuG
# March 24 2018


#' Do something
#' @author JuG
#' @description
#' @param initialPos
#' @param nSteps number of steps
#' @param timeStep time elapsed between 2 steps in ms
#' @param diffusion Diffusion coefficient in $\mu m^2$/sec
#' @details
#' @examples
#' Nfluo = 100                   #nombre de fluorophores
#' radius = 350                  #bacteria radius in nm
#' len = 750                     #rod body half length in nm
#' boxSize = 1500                #canvas size in nm
#' pos0 <- createFluo3d(Nfluo, radius, len, boxSize)
#' diffArray <-createDiffusionArray(initialPos = pos0,nSteps = 50, timeStep = 16e-3,diffusion = .45)
#'
#'  plot3d(pos0$data$x,pos0$data$z,pos0$data$y,type = 's',
#'  xlab ="x", ylab="y",zlab="z",radius = 10,col='blue' ,
#'  xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize))
#' lines3d(diffArray[1:100,1,50],diffArray[1:100,3,50],diffArray[1:i,2,50],col='red',lty=2)
#' @return coordArray[nSteps, ncoord, Nfluo]
#' @export


createDiffusionArray<- function(initialPos, nSteps, timeStep, diffusion ){
  if(!require('MASS')){install.packages('MASS')}
  require(MASS)

  Nfluo <- dim(initialPos$data)[1]
  diagSigmaMatrix = 2 * diffusion * 1e6 * timeStep #\bar{x} = \sqrt{2.dim.D.t} in nm
  coordArray <- array(data = NA, dim = c(nSteps,3,Nfluo))
  coordArray[1,1,] <- initialPos$data$x
  coordArray[1,2,] <- initialPos$data$y
  coordArray[1,3,] <- initialPos$data$z

  Sigma <- matrix(c(diagSigmaMatrix,1,1,1,diagSigmaMatrix,1,1,1,diagSigmaMatrix),3,3)

  for (i in 2:nSteps){
    for (j in 1:Nfluo){
      cont = TRUE
      while(cont){
        newCoord <- coordArray[i-1,,j] +  mvrnorm(n = 1, mu=c(0,0,0), Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
        if(inRod(newCoord[1],newCoord[2],newCoord[3],radius, len)){
          cont=FALSE
          coordArray[i,,j] <- newCoord
        }
      }
    }
  }
  return(coordArray)
}
