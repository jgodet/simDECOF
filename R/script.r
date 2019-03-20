# script.r
# written by JuG
# March 24 2018


Nfluo = 400                  #nombre de fluorophores
radius = 400                  #bacteria radius in nm
len = 800                     #rod body half length in nm
boxSize = 1500





#initial position of Nfluo melecules
pos0 <- createFluo3d(Nfluo, radius, len, boxSize)

#brownian diffusion
diffArray <-createDiffusionArray(initialPos = pos0,nSteps =500, timeStep = 1e-3,diffusion =.05)



pvdIapp <- pvdJapp  <- pvdLapp  <- pvdDapp <- rep(FALSE, 500)
start <- round(runif(n = 1,min = 1,max = 500))
len <- round(rpois(n = 1, lambda = 20))
pvdIapp[start:(start+len)] <- TRUE
start <- round(runif(n = 1,min = 1,max = 500))
len <- round(rpois(n = 1, lambda = 20))
pvdJapp[start:(start+len)] <- TRUE
start <- round(runif(n = 1,min = 1,max = 500))
len <- round(rpois(n = 1, lambda = 20))
pvdLapp[start:(start+len)] <- TRUE
start <- round(runif(n = 1,min = 1,max = 500))
len <- round(rpois(n = 1, lambda = 20))
pvdDapp[start:(start+len)] <- TRUE

dataCol <- data.frame(pvdIapp,pvdJapp,pvdLapp,pvdDapp)
apply(dataCol, MARGIN = 1, FUN = sum)

require(rgl)
plot3d(x=pos0$data$x,y=pos0$data$y,z=pos0$data$z,type = 'p',xlim = c(-400,400),ylim=c(-400,400),zlim = c(-1200,1200),xlab = "",ylab="",zlab="")
for (i in sample(x = 1:Nfluo,size = 100,replace = F)){
  lines3d(diffArray[,,i],col=i)
}


plot3d(x=pos0$data$x,y=pos0$data$y,z=pos0$data$z,type = '',xlim = c(-400,400),ylim=c(-400,400),zlim = c(-1200,1200),xlab = "",ylab="",zlab="",col=1)
aspect3d("iso")
for (i in sample(x = 1:200,size = 100,replace = F)){
  start <- round(runif(n = 1,min = 1,max = 480))
  len <- round(rpois(n = 1, lambda = 20))
  end <- ifelse(start+len <500, start+len, 500)
  lines3d(diffArray[start:end,,i],col='red')
}

for (i in sample(x = 1:400,size = 400,replace = F)){
  start <- round(runif(n = 1,min = 1,max = 480))
  len <- round(rpois(n = 1, lambda = 20))
  end <- ifelse(start+len <500, start+len, 500)
  lines3d(diffArray[start:end,,i],col='blue')
}

for (i in sample(x = 1:400,size = 400,replace = F)){
  start <- round(runif(n = 1,min = 1,max = 480))
  len <- round(rpois(n = 1, lambda = 20))
  end <- ifelse(start+len <500, start+len, 500)
  lines3d(diffArray[start:end,,i],col='orange')
}

for (i in sample(x = 1:400,size = 400,replace = F)){
  start <- round(runif(n = 1,min = 1,max = 480))
  len <- round(rpois(n = 1, lambda = 20))
  end <- ifelse(start+len <500, start+len, 500)
  lines3d(diffArray[start:end,,i],col='green')
}

lines3d(diffArray[,,10])#,type = 'l',xlim = c(-400,400),c(-400,400),zlim = c(-1200,1200))
lines3d(diffArray[,,15])#,type = 'l',xlim = c(-400,400),c(-400,400),zlim = c(-1200,1200))
lines3d(diffArray[,,20])#,type = 'l',xlim = c(-400,400),c(-400,400),zlim = c(-1200,1200))
lines3d(diffArray[,,30])#,type = 'l',xlim = c(-400,400),c(-400,400),zlim = c(-1200,1200))
aspect3d("iso")


#illumination matrix
illMat <- createIllumArray(diffArray)

#plot profile d'illumination sur 1 frame
plotIllum(diffArray, posI=84, plot2D = TRUE)

#plot track
plotTrace(diffArray, whichMol = c(20:22))



plot(1:dim(diffArray)[1],createTrace(diffArray, CR = 5, tScale = 1), type='l', ylab="Photons",xlab='Time, ms')


photons <- apply(illMat,1,emPhoton, CountRate = 500, timeScale = 1,sum=TRUE)
median(photons)


photonsMol10 <- emPhoton(illum = illMat[,13],CountRate = 50,timeScale = 1, sum=TRUE)

YlOrRd <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")
colML <- colorRampPalette(c("white",YlOrRd), space = "Lab")(10)
plotTrace(diffArray, whichMol = c(20), coul = colML[cut(photonsMol10,10)])

photons
signal <- bin(photons, 1)

plot(1:length(signal),signal, type='l')


par(mfrow=c(2,1))
plot(1:dim(diffArray)[1],createTrace(diffArray, CR = 5, tScale = 1), type='l')
plot(1:dim(diffArray)[1],createTrace(diffArray, CR = 5, tScale = 1), type='l')

#volume bacterie
V0 = pi * radius**2 * (4/3*radius + len) * 1e-9 #en L (0.56 µm**3 == 0.56fL)

#volume PSF
w0=300
zr= (pi*w0**2)/930
Vgl = (pi**2 * w0**2 * zr/(4))*1e-9


# sum(PSF)
#
# sum(mapply(-5000:5000,FUN = sumZ))*1e-9
#
# sumZ <- function(z){sum(PSF2p(rhoVal = 0:5000,z=z, w0 = 300, lambda = 930)/PSF2p(rhoVal = 0,z=0, w0 = 300, lambda = 930))}
#
# integrate(PSF2p,lower = 0, upper = 500,z=0)
#
# plotIllum(diffArray, posI=82, plot2D = TRUE)


