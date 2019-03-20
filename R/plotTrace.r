# plotTrace.r
# written by JuG
# March 26 2018


#' Plot traces
#' @author JuG
#' @description
#' @param coordArray Array of coordinates [nSteps, ncoord, Nfluo]
#' @param whichMol Molecule to trace (vector)
#' @details
#' @examples
#'
#' plotTrace(diffArray, whichMol = c(10:11))

#' @return
#' @export


plotTrace<- function(coordArray, whichMol = c(10,30), from = NULL, to = NULL,boxSize = 1500 ,coul = "lightgrey"){
  if(missing(coordArray)){cat("coordArray missing")
                          return()}
  if(is.null(from)){from <- 1}
  if(is.null(to)){to <- dim(coordArray)[1]}
  plot3d(coordArray[from:to,1,whichMol[1]],coordArray[from:to,3,whichMol[1]],coordArray[from:to,2,whichMol[1]],type = 's',
         xlab ="x", ylab="y",zlab="z",radius = 10,col = coul,
         xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
  lines3d(coordArray[from:to,1,whichMol[1]],coordArray[from:to,3,whichMol[1]],coordArray[from:to,2,whichMol[1]],col='red',lty=2)
  if(length(whichMol)>1){
    for (i in 2:length(whichMol)){
      plot3d(coordArray[from:to,1,whichMol[i]],coordArray[from:to,3,whichMol[i]],coordArray[from:to,2,whichMol[i]],
             type = 's',radius = 10,col=coul,add=TRUE)
      lines3d(coordArray[from:to,1,whichMol[i]],coordArray[from:to,3,whichMol[i]],coordArray[from:to,2,whichMol[i]],col=(i+3),lty=2)
    }
  }

  #lines3d(coordArray[1:i,1,50],coordArray[1:i,3,50],coordArray[1:i,2,50],col='red',lty=2)
  return()
}
