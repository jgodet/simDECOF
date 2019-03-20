# readFifo.r
# written by JuG
# April 11 2018


#' Read FIFO files from SPC830 exported as ascii
#' @author JuG
#' @description
#' @param fichier path to ascii file
#' @details
#' @examples
#'pathFile <- "/Users/jgodet/Seafile/Ma bibliothèque/Code/simDECOF/data/P453_02mWdep_002.asc"
#'fluxPhotons <- readFifo(pathFile)
#'plot(1:length(fluxPhotons), fluxPhotons, type='l')
#' @return photons flux (vector)
#' @export


readFifo <- function(fichier){
  con<-file(fichier) #ouvrir le fichier comme connection
  #trouver les blocs
  b<-readLines(con)
  n1<-which(b=="*BLOCK 1 Mcs")
  fn1<-which(b=="*END")
  nl1<-fn1-n1-1
  close(con)
  #signal <- as.numeric(b[(n1+1):(fn1-1)])
  don<-scan(file=fichier,skip=n1, nline=nl1,multi.line = TRUE,sep=" ")
  return(don)
}
