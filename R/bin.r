# bin.r
# written by JuG
# March 28 2018


#' Bin a vector
#' @author JuG
#' @description
#' @param data vector of values
#' @param binSize number to bin
#' @details
#' @examples
#' x <- rep(1,100)
#' bin(x,binSize=10)
#' bin(x,binSize=12)
#'
#' @return
#' @export


bin<- function(data, binSize){
  n <- length(data)
  while(n%%binSize != 0){
    data <- c(data,NA)
    n <- length(data)
  }
  cat <- rep(1:(n/binSize), each=binSize)
  return( aggregate(data~cat,FUN = sum, na.rm=T)[,2])
}
