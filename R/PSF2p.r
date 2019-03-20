# PSF2p.r
# written by JuG
# March 24 2018


#' Compute PSF for TPE (Lorentzian - 2D Gaussian)
#' @author JuG
#' @description
#' @param rhoval rho distance in nm
#' @param z z position in nm
#' @param w0 beam waist at 1/e nm
#' @details
#' @examples
#'
#' @return
#' @export


PSF2p<- function(rhoVal,z, w0 = 300, lambda = 930){
    zr = pi * w0**2 / lambda
    wz <- w0**2 * (1+(z/zr)**2)
    4*w0**4 /(pi**2 * wz**2) * exp(-4 * rhoVal**2 / wz )
}

