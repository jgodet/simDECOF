# createFluo3d.r
# written by JuG
# March 24 2018


#' Create N molecule fluorescente in a 2D/3D space
#' @author JuG
#' @description
#' @param Nfluo : Nombre de fluorophores
#' @param sizex : pos max in x
#' @param sizey : pos max in y
#' @details
#' @examples
#' #sim molecule in bacteria cytoplasm
#'  require(rgl)
#'  Nfluo = 1000                   #nombre de fluorophores
#'  radius = 350                  #bacteria radius in nm
#'  len = 750                     #rod body half length in nm
#'  boxSize = 1500                #canvas size in nm
#'  pos0 <- createFluo3d(Nfluo, radius, len, boxSize)
#'  plot3d(pos0$data$x,pos0$data$z,pos0$data$y,type = 's',
#'  xlab ="x", ylab="y",zlab="z",radius = 10,col='blue' ,
#'  xlim =c(-boxSize,boxSize),ylim=c(-boxSize,boxSize),zlim=c(-boxSize,boxSize))
#'
#' @return ppp (package spatstat)
#' @export


createFluo3d<- function(Nfluo, r,a,boxSize){
  if(!require('spatstat')){install.packages('spatstat')}
  require('spatstat')
  # create N molecule fluorescente in a 2D/3D space
  #@ Nfluo : Nombre de fluorophores
  #@ sizex : pos max in x
  #@ sizey : pos max in y
  # return ppp (package spatstat)
  sel <- numeric()
  df <- data.frame (x=NA , y=NA, z=NA )
  while(sum(sel) < Nfluo){
    z <- runif(1,-r-a,r+a)
    x <- runif(1, -r,r)
    y <- runif(1, -r,r)
    checkR <-   (x*x + y*y + 1/4 * (abs(z-a) + abs(z+a) - 2*a)^2) <= (r*r)
    sel <- c(sel, checkR)
    if(checkR){ df <- rbind(df,data.frame(x,y,z))}
  }
  df <- df[-1,]
  pos0 <- ppx(df,coord.type=c("s","s","s"))
  return(pos0)
}
