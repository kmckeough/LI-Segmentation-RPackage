#' Multi Bound
#'
#' Add a series of shaded boundaries onto the plotSource image
#' Ignore warnings, hacky fix of image limits
#'
#' @param ggobj gg plot object
#' @param bound_counts bound counts vector
#' @return the total summation component of the Ising distribution
#' @export
#'
#'
#'

multiBound<-function(ggobj,
                     bound_counts){

  n<-sqrt(length(bound_counts))
  bound_array<-melt(array(bound_counts,dim=c(n,n)))
  names(bound_array)<-c('Y','X','counts')

  if(!all(is.na(c(coord.range.x,coord.range.y)))){

        pixel.x<-seq(coord.range.x[1],
                 coord.range.x[2],
                 len = n)
    pixel.y<-seq(coord.range.y[1],
                 coord.range.y[2],
                 len = n)

    for(ii in 1:n){
      bound_array$X[bound_array$X==ii]<-pixel.x[ii]
      bound_array$Y[bound_array$Y==ii]<-pixel.y[ii]
    }

  }

  for(ii in 1:max(bound_array$counts)){
    roi<-as.data.frame(bound_array[bound_array$counts==ii,])
    roi$w <- pixel.x[2]-pixel.x[1]
    roi$h<-pixel.y[2]-pixel.y[1]
    ggobj<-ggobj + geom_tile(roi,mapping=aes(x=X,y=Y, width=w,height=h),fill=bound_color[ii])
      #geom_point(roi,mapping=aes(x=X,y=Y),color=bound_color[ii],shape=15)#+
      #coord_equal(ratio=1/cos(mean(bound_array$X)*pi/180))#+
      #xlim(max(bound_array$X),min(bound_array$X))+
      #ylim(min(bound_array$Y),max(bound_array$Y))
  }

  return(ggobj)
  }
