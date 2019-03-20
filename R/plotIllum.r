# plotIllum.r
# written by JuG
# March 24 2018


#' Do something
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'
#'
#' @return
#' @export


plotIllum<- function(diffArray, posI=1, plot2D = TRUE){
  if(!require('RColorBrewer')){install.packages('RColorBrewer')}
  require(RColorBrewer)
  if(!require('rgl')){install.packages('rgl')}
  require(rgl)
  illum <- PSF2p(rhoVal = rho(x = diffArray[posI,1,], y =  diffArray[posI,3,]), z =  diffArray[posI,2,])
  illumCu <- cut (illum, 100)
  #colML <- matlab.like(30)
  #http://colorbrewer2.org/#type=sequential&scheme=YlOrRd&n=4
  #YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  YlOrRd <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")
  colML <- colorRampPalette(c("white",YlOrRd), space = "Lab")(100)
  if(plot2D){plot(diffArray[posI,3,],diffArray[posI,1,],
       xlab ="x", ylab="y",bg=colML[illumCu],asp=1, pch=21, col="black" ,cex=1)
  }
  if(!plot2D){
    plot3d(diffArray[posI,1,],diffArray[posI,2,],diffArray[posI,3,],type = 's',
           xlab ="x", ylab="y",zlab="z",radius = 10,col=colML[illumCu]  ,
           xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
  }
}
