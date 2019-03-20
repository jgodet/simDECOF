


readFifo<-function(fichier){

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
    
 pathFichier <- "E:\\Seafile\\Ma bibliothèque\\Code\\simDECOF\\data\\P453_02mWdep_001.asc"
 
 signal = readFifo(pathFichier)
 x11()
 
 plot(1:20000,signal,type='l')
 