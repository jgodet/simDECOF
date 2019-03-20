# pch.r
# written by JuG
# March 28 2018


#' Do something
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'
#'pch(photons)
#' @return
#' @export


pch<- function(Photons){
  phCut <- cut(Photons, seq(min(Photons),max(Photons),by=1))
  emptyMin <- min(which(c(table(phCut))!=0))+1
  emptyMax <- max(which(c(table(phCut))!=0))-1
  probs <- table(phCut)[emptyMin:emptyMax]/length(Photons)
  plot(emptyMin:emptyMax,probs, log="y",pch='+', ylab='',xlab="N photons")
  axis(2, las=1)
  }
