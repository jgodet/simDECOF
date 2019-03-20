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
  n1<-which(b=="End of info header")
  close(con)
  #signal <- as.numeric(b[(n1+1):(fn1-1)])
  don<-read.table(file=fichier,skip=n1+1, sep=" ")
  don <- don[order(don$V1),]
  nRow <- nrow(don)
  don$V1 <- don$V1 * 50e-9
  don$V3[2:nRow] <- don$V1[2:nRow] - don$V1[1:(nRow-1)]
  don$V3[1] <- don$V1[1]

  colnames(don) <- c("MacroTime", "MicroTime","DiffTime")
  return(don)
}
