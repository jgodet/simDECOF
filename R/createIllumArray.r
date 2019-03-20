# createIllumArray.r
# written by JuG
# March 26 2018


#' Do something
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'
#' illMat <- createIllumArray(diffArray)
#' @return
#' @export


createIllumArray<- function(diffArray){
  illumMat <- array(data = NA,dim = c(dim(diffArray)[c(1,3)]))
  for (i in 1:dim(diffArray)[1]){
    illumMat[i,] <- PSF2p(rhoVal = rho(x = diffArray[i,1,], y =  diffArray[i,3,]), z =  diffArray[i,2,])
  }
  return(illumMat/PSF2p(0,0))
}
