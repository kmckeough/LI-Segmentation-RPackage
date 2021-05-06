#' Modify Custom Color Scale
#'
#' Hacky way to alter range of color scale to accommodate low count images.
#' Emphasizes changes in color between pixels closer to the background.
#'
#' @param colors a color scale in the form of a vector of colors
#' @param bkg_color background color of the image
#' @param n scale. Higher numbers emphasize lower values. 1 is no scale.
#' @return the total summation component of the Ising distribution
#' @export
#'
#'


modifyColor<-function(colors,bkg_color='black',n=1){

  color<-bkg_color
  for(ii in 1:length(colors)){
    color<-c(color,rep(colors[ii],ii*n))
  }

  return(color)
}
