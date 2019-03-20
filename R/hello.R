# #simulation bleaching
# #JuG le  Thu Aug 10 15:53:52 2017
# #modif le Fri Aug 11 23:31:07 2017
#
# #simule N molécules fluorescentes qui diffusent au sein d'une bactérie (rod)
# #
# #
#
# #TODO mettre e doc des fonctions
# #TODO mettre en forme 3D
# #TODO ins?rer brillance et bruit signaux fluo
#
# Mac <- TRUE
#
# #%% load packages
#
# library(spatstat)
# library(plotrix)
# library(MASS)
# library(rgl)
# library(RColorBrewer)
#
# #%% user choices
#
# dimension = "2D"              #2D ou 3D
# plotAtN  = NA                 #plot at a given frame (NA if do not plot)
# plotDynamique = TRUE          #plot
# tmax=1000                      #max number of frames to be rendered
# if(Mac){
#   path4plot = "/Users/jgodet/Documents/FLIPdiffusion/180323"    #penser ? "\\" a la fin
# }else{
#   path4plot = "C:\\Users\\julien godet\\Desktop\\test\\"    #penser ? "\\" a la fin
# }
#
# #%% initalisation des variables
#
# #format taille
# sizex = 2500                  #taille cellule X in nm
# sizey = 700                   #taille cellule Y in nm
# radius = 350                  #bacteria radius in nm
# len = 750                     #rod body half length in nm
# boxSize = 1500                #canvas size in nm
#
# #nombre et diffusion
# timePerFrame = 1           #time per frame in ms
# Nfluo = 1000                   #nombre de fluorophores
# nSteps = 500                  #nombre de frames (ms)
# stepDistum2sec =  .5       #step jump per frame in µm^2 per sec
# diagSigmaMatrix = 1500        #sigma_i values for normal 3D random Walk
#
# #bleaching
# bleachingRadius = 350/2         #bleaching radius in nm
# bleachposX = 1250             #position X du centre du bleaching spot
# bleachposY = 350              #position Y du centre du bleaching spot
# bleachingRate = .7          #bleaching Rate  (the higher the faster)
#
# #brillance et lfuo
# bkgSignal = 0.2               # Background Signal in kHz
# bright = 5                    # molecular brightness in kHz
#
#
# #TODO
# synth = FALSE                  #si synthese de prot
# synthRate = 200 / 3600        # nombre par seconde
#
#
# #%% load functions
# if(Mac){
#   source("/Users/jgodet/Seafile/Ma bibliothèque/Code/FLIPdiffusion/simTracesFunctions.r")
# }else{
#   source("E:\\Seafile\\Ma biblioth?que\\Code\\FLIPdiffusion\\simTracesFunctions.r")
# }
#
# #%% Initialisation des parametres.
# stepDist = stepDistum2sec * timePerFrame *1000 #1µm^2/s <=> 1000 nm/ms
# diagSigmaMatrix = 1500  #a changer
# bleachTime <- round(rexp(n = Nfluo, rate = bleachingRate))+1
# bkgSignalperFrame <- bkgSignal *timePerFrame
# brightperFrame <-  bright *timePerFrame
#
#
#
#
#
# #%% Calculate and display impact time.  Increment initial speed each step.
#
# if(dimension=="2D"){
#   pos0 <- createFluo2D(Nfluo, sizex,sizey)
#   plot(pos0,xlim=c(0,sizex),ylim=c(0,sizey),pch=".")
#
#
#   #calc matrice de coordonenes
#   coordArray <- array(data = NA, dim = c(nSteps,2,Nfluo))     #dim(t,N)
#   bleachStateArray <- array(data = 1, dim = c(nSteps,Nfluo))  #dim(t,N)
#   inRadiusArray <- array(data = NA, dim = c(nSteps,Nfluo))    #dim(t,N)
#   for (i in 1:Nfluo){
#     xp <-Reduce( updateposX, c(pos0$x[i],2:nSteps), accumulate=TRUE)
#     yp <-Reduce( updateposY, c(pos0$y[i],2:nSteps), accumulate=TRUE)
#     coordArray[,1,i] <- xp
#     coordArray[,2,i] <- yp
#     inRadiusArray[,i]<- apply(coordArray[,1:2,i],1,inRadius)
#   }
#
#   #determine si molecule est bleachee
#   idbleach <- numeric()
#   for (i in 1:Nfluo){
#     indice <- whichbleach(x=apply(inRadiusArray,2,cumsum)[,i], val=bleachTime[i])
#     idbleach[i] <-ifelse(indice==Inf, nSteps, indice)
#     if(idbleach[i]<nSteps){
#       bleachStateArray[ idbleach[i]:nSteps,i]<-0.1
#     }
#   }
#
#   nbInFocus <- numbInRadius(inRadiusArray, bleachStateArray,ind = 1)
#
#   if(plotDynamique){
#     #plot dynamique
#     par(mfrow=c(1,2))
#     for (i in 1:tmax){
#       plot(coordArray[i,1,],coordArray[i,2,],type="p",xlim=c(0,sizex),ylim=c(0,sizey),asp=1,col=rgb(0,inRadiusArray[i,],!inRadiusArray[i,],bleachStateArray[i,])
#            ,pch=19,xlab="",ylab="",axes=FALSE)
#       box()
#       draw.circle(bleachposX,bleachposY,radius=bleachingRadius, nv=100, lty=2)
#       plot(1:i, nbInFocus[1:i],xlim=c(0,tmax),ylim=c(0,max(nbInFocus)*1.2),type='b',xlab="Time",ylab="Nb Fluorophores")
#       #curve(dexp(x,rate=rateMes),from=0,to=i,add=TRUE,col='red')
#       Sys.sleep(.1)
#     }
#   }
#
#   if(!is.na(plotAtN) & is.numeric(plotAtN)){
#     par(mfrow=c(1,2))
#     plot(coordArray[plotAtN,1,],coordArray[plotAtN,2,],type="p",xlim=c(0,sizex),ylim=c(0,sizey),asp=1,col=rgb(0,0,1,bleachStateArray[plotAtN,])
#          ,pch=19,xlab="",ylab="",axes=FALSE)
#     box()
#     draw.circle(bleachposX,bleachposY,radius=bleachingRadius, nv=100, lty=2)
#     plot(1:plotAtN, nbInFocus[1:plotAtN],xlim=c(0,200),ylim=c(0,max(nbInFocus)*1.2),type='b',xlab="Time",ylab="Nb Fluorophores")
#     #curve(dexp(x,rate=rateMes),from=0,to=i,add=TRUE,col='red')
#     mtext(side = 3, paste("Bleaching rate: ",bleachingRate, "\nSteps Jump: ",stepDist,sep='' ))
#   }
#
#
# }
#
#
#
#
# if(dimension=="3D"){
#
#
#
#   pos0 <- createFluo3D(Nfluo, radius, len, boxSize)
#   plot3d(pos0$data$x,pos0$data$z,pos0$data$y,type = 's',
#          xlab ="x", ylab="y",zlab="z",radius = 10,col='blue' ,
#          xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize))
#
#
#   coordArray <- array(data = NA, dim = c(nSteps,3,Nfluo))
#   coordArray[1,1,] <- pos0$data$x
#   coordArray[1,2,] <- pos0$data$y
#   coordArray[1,3,] <- pos0$data$z
#
#
#   inRadiusArray <- array(data = NA, dim = c(nSteps,Nfluo))
#   bleachStateArray <- array(data = 5, dim = c(nSteps,Nfluo))
#
#   for (j in 1:Nfluo){
#     inRadiusArray[1,j] <- inRadius(coordArray[1,c(1,3),j], r= bleachingRadius, x0 = 0, y0 = 0 )}
#   Sigma <- matrix(c(diagSigmaMatrix,1,1,1,diagSigmaMatrix,1,1,1,diagSigmaMatrix),3,3)
#
#   for (i in 2:nSteps){
#     for (j in 1:Nfluo){
#       cont = TRUE
#       while(cont){
#         newCoord <- coordArray[i-1,,j] +  mvrnorm(n = 1, mu=c(0,0,0), Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
#         if(inRod(newCoord[1],newCoord[2],newCoord[3],radius, len)){
#           cont=FALSE
#           coordArray[i,,j] <- newCoord
#         }
#       }
#       inRadiusArray[i,j] <- inRadius(coordArray[i,c(1,3),j], r= bleachingRadius, x0 = 0, y0 = 0 )
#     }
#   }
#
#
#   #determine si molecule est bleachee
#   idbleach <- numeric()
#   for (i in 1:Nfluo){
#     indice <- whichbleach(x=apply(inRadiusArray,2,cumsum)[,i], val=bleachTime[i])
#     idbleach[i] <-ifelse(indice==Inf, nSteps, indice)
#     if(idbleach[i]<nSteps){
#       bleachStateArray[ idbleach[i]:nSteps,i]<-1
#     }
#   }
#
#   nbInFocus <- numbInRadius(inRadiusArray, bleachStateArray,ind=5)
#
#   darkcols <- brewer.pal(5, "Blues")
#   tmax=100
#
#   if(plotDynamique){
#     plot3d(coordArray[1,1,],coordArray[1,3,],coordArray[1,2,],type = 's',
#            xlab ="x", ylab="y",zlab="z",radius = 20,col=darkcols[bleachStateArray[1,]] ,
#            xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
#     plot3d(coordArray[1,1,50],coordArray[1,3,50],coordArray[1,2,50],type = 's',radius = 20,col='red',add=TRUE)
#     #cyl3d(rx=350,ry=350,zmax=700,col='red',alpha=0.3,ctr=c(0,0,-250),depth_mask = FALSE)
#     rgl.snapshot(paste(path4plot,1,".png",sep=''))
#
#     a = "mlqksd"
#     while(a=="mlqksd"){
#       a <- readline(prompt="Adjust position and \nPress [enter] to continue")
#       Sys.sleep(1)
#     }
#
#
#     for (i in 2:tmax){
#       rgl.clear()
#       plot3d(coordArray[i,1,],coordArray[i,3,],coordArray[i,2,],type = 's',
#              xlab ="x", ylab="y",zlab="z",radius = 20,col=darkcols[bleachStateArray[i,]]  ,
#              xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
#       plot3d(coordArray[i,1,50],coordArray[i,3,50],coordArray[i,2,50],type = 's',radius = 20,col='red',add=TRUE,alpha= bleachStateArray[i,50]/5)
#       lines3d(coordArray[1:i,1,50],coordArray[1:i,3,50],coordArray[1:i,2,50],col='red',lty=2)
#       #cyl3d(rx=350,ry=350,zmax=700,col='red',alpha=0.2,ctr=c(0,0,-250),depth_mask = FALSE)
#       rgl.snapshot(paste(path4plot,i,".png",sep=''))
#     }
#   }
#
#   if(!is.na(plotAtN) & is.numeric(plotAtN)){
#     plot3d(coordArray[plotAtN,1,],coordArray[plotAtN,3,],coordArray[plotAtN,2,],type = 's',
#            xlab ="x", ylab="y",zlab="z",radius = 20,col=darkcols[bleachStateArray[1,]] ,
#            xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
#     plot3d(coordArray[plotAtN,1,50],coordArray[plotAtN,3,50],coordArray[plotAtN,2,50],type = 's',radius = 20,col='red',add=TRUE)
#     if(plotAtN>1){
#       lines3d(coordArray[1:plotAtN,1,50],coordArray[1:plotAtN,3,50],coordArray[1:plotAtN,2,50],col='red',lty=2)
#     }
#     cyl3d(rx=350,ry=350,zmax=700,col='red',alpha=0.3,ctr=c(0,0,-250),depth_mask = FALSE)
#   }
#
#
# }
#
#
#
# # ##################################################
# #draft part
# ####################################################
#
#
# #plot(1:tmax, calcSignal(nbInFocus, bkgSignalperFrame, brightperFrame,timePerFrame)[1:tmax])
#
# #nbInFocusPole <- nbInFocus
# ##nbInFocusCentre <- nbInFocus     #c(1250,
# #
# #
# #
# # plot(1:tmax, nbInFocusPole[1:tmax],xlim=c(0,200),ylim=c(0,max(nbInFocus)*1.2),type='b',xlab="Time",ylab="Nb Fluorophores")
# # lines(1:tmax, nbInFocusCentre[1:tmax], col='blue',type="b")
# #
# #
# #
#
#
# plot(pos0$data,asp='1')
#
#
# i=150
# plot3d(coordArray[i,1,],coordArray[i,3,],coordArray[i,2,],type = 's',
#        xlab ="x", ylab="y",zlab="z",radius = 20,col=darkcols[bleachStateArray[i,]]  ,
#        xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
# plot3d(coordArray[i,1,50],coordArray[i,3,50],coordArray[i,2,50],type = 's',radius = 20,col='red',add=TRUE,alpha= bleachStateArray[i,50]/5)
# lines3d(coordArray[1:i,1,50],coordArray[1:i,3,50],coordArray[1:i,2,50],col='red',lty=2)
#
# wz <- function(z, w0 = 300, lambda = 930){
#   zr = pi * w0**2 / lambda
#   return(w0**2 * (1+(z/zr)**2))
# }
# PSF <- function(rhoVal,z, w0 = 300){
#   4*w0**4 /( pi**2 * wz(z)**2) * exp(-4 * rhoVal**2 / wz(z) )
# }
#
# cyl3d(rx=350,ry=350,zmax=700,col='red',alpha=0.2,ctr=c(0,0,-250),depth_mask = FALSE)
#
# rho <- function(x,y,xref=0, yref=0){
#   return(sqrt((x-xref)**2 + (y-yref)**2))
# }
#
# emPhoton <- function(illum, CountRate = 5 ){
#   #CountRate photons per ms
#   lambda <- round(illum * CountRate)
#   sum(sapply(lambda, FUN = rpois, n=1))
# }
#
# writeWebGL(dir=file.path("/Users/jgodet/Documents/FLIPdiffusion/", "webGL"))
#
#
# pos0 <- createFluo3D(1000, radius, len, boxSize)
# illum <- PSF(rhoVal = rho(x = pos0$data$x, y =  pos0$data$z), z =  pos0$data$y)
# illumCu <- cut (illum, 100)
# #colML <- matlab.like(30)
# #http://colorbrewer2.org/#type=sequential&scheme=YlOrRd&n=4
# #YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
# YlOrRd <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")
# colML <- colorRampPalette(c("white",YlOrRd), space = "Lab")(100)
# colML <- colorRampPalette(c("white","blue", "red"))(100)
# #colML <- rev(heat.colors(30))
# #colML <- colorRampPalette(Reds, space = "Lab")(100)
#
# plot(pos0$data$z,pos0$data$x,
#      xlab ="x", ylab="y",bg=colML[illumCu],asp=1, pch=21, col="black" ,cex=1)#,
# #  xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),axes = FALSE)
#
#
#
#
#
#
# plot3d(pos0$data$x,pos0$data$y,pos0$data$z,type = 's',
#        xlab ="x", ylab="y",zlab="z",radius = 10,col=colML[illumCu]  ,
#        xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
#
# plot3d(coordArray[i,1,],coordArray[i,3,],coordArray[i,2,],type = 's',
#        xlab ="x", ylab="y",zlab="z",radius = 20,col=darkcols[bleachStateArray[i,]]  ,
#        xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize),axes = FALSE)
#
# exp(-1/10 * 5 * 1:10)
#
#
#
