#' Rectangle Base for Simulation
#'
#' Create a rectangle from which to simulate observed images from
#'
#' @param bkg_param value of bkg noise
#' @param max_param value of maximum step
#' @param n_img size of pixels of image
#' @param width width of steps
#' @param height height of steps
#' @export
#'


rectBase<-function(bkg_param,
                    max_param,
                    n_img,
                    width,
                    height){

  #Initialize background
  init_img<-array(bkg_param,dim=c(n_img,n_img))



  # Add Stairs
  start_y<-floor((n_img-height)/2)
  start_x<-floor((n_img-width)/2)

  init_img[start_x:(start_x+width),
             start_y:(start_y+height)]<-max_param


  return(init_img)

}
