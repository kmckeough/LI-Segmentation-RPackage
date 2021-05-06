#' Plot A Grid of Images
#'
#' Plot the source. Options to include coordinates and boundaries
#'
#' @param bkg_param value of bkg noise
#' @param max_param value of maximum step
#' @param n_img size of pixels of image
#' @param n_stairs number of steps
#' @param width_step width of steps
#' @param height_step height of steps
#' @export
#'


stairBase<-function(bkg_param,
                    max_param,
                    n_img,
                    n_stairs = 4,
                    width_step,
                    height_step){
  #Initialize background
  init_img<-array(bkg_param,dim=c(n_img,n_img))

  #Mean stair level
  stair_levels<-seq(bkg_param,max_param,len=n_stairs+1)[-1]

  # Add Stairs
  start_y<-floor((n_img-height_step)/2)
  start_x<-floor((n_img-n_stairs*width_step)/2)
  for(ii in 1:n_stairs){
    init_img[(start_x+width_step*(ii-1)+1):(start_x+width_step*ii),
             start_y:(start_y+height_step-1)]<-stair_levels[ii]
  }

  return(init_img)

}
