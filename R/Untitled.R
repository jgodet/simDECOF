
Nphotons <- numeric()
mu <- numeric()
sdq <- numeric()
sdr <- numeric()
j=1
seqi <- c(seq(10,120,by=20))
seqi <- c(.05, .075,.1,.2,.3,.5,.75,1,1.25,1.5,1.75,2,5,10, 20)
for (i in seqi){
  phot <- simPhotons(CR=5,D = 1,Nfluo = i, radius = 300,len = 1000, boxSize = 1500)
  a <- fitSSmodel(phot)
  Nphotons[j] <- a[[1]]
  mu[j] <- a[[2]]
  sdq[j] <- a[[3]]
  sdr[j] <- a[[4]]
  print(j)
  j = j+1
}



plot(seqi, Nphotons)
lm(Nphotons ~ seqi)
abline(lm(Nphotons ~ seqi-1))
  abline(c(0,1),lty=2)
plot(seqi, sdq**2)
abline(lm(sdq**2 ~ seqi))
plot(seqi, sdr)
plot(seqi, mu)

plot(seqi, Nphotons**2/sdq)
abline(lm((Nphotons**2/sdq)~seqi))
summary(lm((Nphotons**2/sdq)~seqi)) #donne le nombre de Particules dans V0

Nv0 <- Nphotons**2/sdq**2
summary(Nphotons/Nv0 / VpsfV0) #CR
summary(Nphotons/Nv0 / VpsfV0) #CR

lm(Nphotons~seqi)
lm(sdq~seqi)
abline(lm(sdq~seqi))
lm(sdr~seqi)
abline(lm(sdr~seqi))



pathFile <- "/Users/jgodet/Seafile/Ma bibliothèque/Code/simDECOF/data/P453_02mWdep2_004.asc"
fluxPhotons <- readFifo(pathFile)
plot(1:length(fluxPhotons), fluxPhotons, type='l')
fluxPhotons[10000:11000]
require(MASS)
fitdistr(fluxPhotons[5000:6000], "Poisson")



Power <- c(2, 3, 4, 5, 6, 7)
Signal <- c( 21.8341658, 31.2727273, 46.3426573, 61.1218781, 78.8971029 ,86.7152847 )

Power <- c(2, 3, 4, 5)
Signal2 <- c( 37.5244755 ,  56.9980020, 80.1088911, 100.0579421 )

plot(Power, Signal2)


pathFile2 <- "/Users/jgodet/Seafile/Ma bibliothèque/Code/simDECOF/data/180320/fitc-tmr 830_022.asc"
fluxPhotons <- readFifo(pathFile2)
plot((1:length(fluxPhotons))/1000, fluxPhotons, type='l',xlab='Time, s', ylab='Count, kHz')
fitdistr(fluxPhotons[30000:60000], "Poisson")

findStart <- function(fluxPhotons){
  darknoise <- mean(fluxPhotons[1:500])
  return(min(which(fluxPhotons > 10*darknoise))+1)
}

start <- findStart(fluxPhotons)
fitdistr(fluxPhotons[start:start+1000], "Poisson")

val <- fluxPhotons[start:(start+1000)]

acf(fluxPhotons[start:(start+100)])


val.ts = ts(val, frequency=1, start=1, end=1000)
trend_val = ma(val.ts, order = 50, centre = T)


plot(val.ts)
lines(trend_val)

install.packages("fpp")
library(fpp)
data(ausbeer)
install.packages("forecast")
library(forecast)
trend_beer = ma(timeserie_beer, order = 4, centre = T)
timeserie_beer = tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))


pathFile3 <- "/Users/jgodet/Seafile/Ma bibliothèque/Code/simDECOF/data/FIFO/P453_02mWdep_003.asc"
fluxPhotons <- readFifo(pathFile3)

pathFile3 <- "/Users/jgodet/Seafile/Ma bibliothèque/Code/simDECOF/data/eGFP/egfp26mW_001.asc"
fluxPhotons <- readFifo(pathFile3)
fitdistr(fluxPhotons$DiffTime, "exponential")


rate <- numeric()
powVal <- c(1,seq(2,40,by=2))
for (i in seq(along=powVal)){
  if(powVal[i]<10){
  pathFile3 <- paste("/Users/jgodet/Seafile/Ma bibliothèque/Code/simDECOF/data/eGFP/egfp0",powVal[i],"mW_001.asc",sep='')
  }else{
  pathFile3 <- paste("/Users/jgodet/Seafile/Ma bibliothèque/Code/simDECOF/data/eGFP/egfp",powVal[i],"mW_001.asc",sep='')
  }
  fluxPhotons <- readFifo(pathFile3)
  fitdistr(fluxPhotons$DiffTime, "exponential")
  rate[i] <- unname(fitdistr(fluxPhotons$DiffTime, "exponential")[[1]])
  }

plot(powVal,rate)



binPhotons <- function(fluxPhotons, binTime=1e-3, windowTime=20){
  if(missing(windowTime)){
    windowTime <- max(fluxPhotons$MacroTime + 1)
  }
  out <- cut(fluxPhotons$MacroTime, seq(0,windowTime,by=binTime))
  return(as.vector(table(out)))
}

flux <- binPhotons(fluxPhotons, binTime=1e-3, windowTime=20)
plot(1:length(flux),flux, type='l')
acf(flux,lag.max = 150)

plot(1:length(flux),flux, type='l',xlim=c(5200,5300))
lines((1:200000)/10,flux, type='l',xlim=c(52000,53000),col='red')
flux[1125:1150]

4* .3**2 / 10e-3
