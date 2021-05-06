#' Plot Source
#'
#' Plot the source. Options to include corrdinates and boundaries
#'
#' @param img image
#' @param bound boundar
#' @param bound_list list of boundaries
#' @param bound_size width of boundary
#' @param coord.range.x coord range x
#' @param coord.range.y coordinate range y
#' @param crop.range.x crop
#' @param crop.range.y crop
#' @param save TRUE if save
#' @param legend_label legend title
#' @param plot_file destination of saved file
#' @param colors color scale
#' @param title title of image
#' @param limits limits of scale
#' @param na.value NA value color
#' @param width width of saved image
#' @param height height of saved image
#' @param bound_color color of bound or vector of colors of bounds
#' @param bkg_color color of bkg to image
#' @param legend_color color of legend text
#' @param show_legend show legend  on image
#' @param axis_color color of axis labels
#' @return the total summation component of the Ising distribution
#' @export
#'
#'
plotSource<-function(img,
                     bound=NA,
                     bound_list=NA,
                     bound_size=1,
                     coord.range.x = NA,
                     coord.range.y = NA,
                     crop.range.x = NA,
                     crop.range.y = NA,
                     save=FALSE,
                     legend_label='Counts',
                     plot_file='dropCurve_boundary.png',
                     colors = brewer.pal(9,'Blues'),
                     title='',
                     limits=NULL,
                     na.value="grey50",
                     width=11.6,
                     height=9,
                     bound_color='red',
                     bkg_color='black',
                     legend_color='white',
                     axis_color='white',
                     show_legend=TRUE){

  # Format Image
  colnames(img)<-NULL
  rownames(img)<-NULL
  plot.base<-melt(img)
  names(plot.base)<-c('Y','X','counts')


  if(!all(is.na(c(coord.range.x,coord.range.y)))){
    nx<-nrow(img)
    ny<-ncol(img)
    pixel.x<-seq(coord.range.x[1],
                 coord.range.x[2],
                 len = nx)
    pixel.y<-seq(coord.range.y[1],
                 coord.range.y[2],
                 len = ny)

    for(ii in 1:nx){
      plot.base$X[plot.base$X==ii]<-pixel.x[ii]
    }
    for(ii in 1:ny){
      plot.base$Y[plot.base$Y==ii]<-pixel.y[ii]
    }

  }

  if(!all(is.na(bound))){
    plot.base$Interval<-c(bound)
  }

  if(!all(is.na(bound_list))){
    for(ii in 1:length(bound_list)){
      plot.base[paste('Interval',ii,sep='_')]<-c(bound_list[[ii]])
    }
  }


  ## If crop
  if(!all(is.na(c(crop.range.x,crop.range.y)))){
    plot.base<-plot.base[plot.base$X<=max(crop.range.x) &
                           plot.base$X>=min(crop.range.x),]

    plot.base<-plot.base[plot.base$Y<=max(crop.range.y) &
                           plot.base$Y>=min(crop.range.y),]

  }

  gp_base<-ggplot(plot.base,aes(x=X,y=Y,fill= counts))

  # Create ggplot
  gp_base<-gp_base+
    geom_raster(show.legend=show_legend)+
    scale_fill_gradientn(colours = colors,limits=limits, na.value = na.value)+
    theme(axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          plot.title  = element_text(color=axis_color))+
    ggtitle(title)+
    theme(plot.background = element_rect(fill = bkg_color),
          legend.background = element_rect(fill="NA"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(color=legend_color,size=6),
          legend.title=element_text(color=legend_color,size=6),
          legend.position=c(0.9,0.5))+
    guides(fill = guide_colourbar(barwidth = 0.3,barheight = 10))+
    labs(fill=legend_label)

  # Add RA/ DEC Coordinates

  if(!all(is.na(c(coord.range.x,coord.range.y)))){

    gp_base<- gp_base + theme(axis.ticks=element_line(color=axis_color),
                    axis.title.x=element_text(color=axis_color,size=12),
                    axis.text.x=element_text(color=axis_color,hjust=0.5),
                    axis.text.y=element_text(color=axis_color, angle=90,hjust=0.5),
                    axis.title.y=element_text(color=axis_color,size=12),
                    plot.margin=unit(c(0.1,1,0.1,0.1),"cm"))+
      xlab('RA')+
      ylab('DEC')+
      scale_x_reverse( )

  }

  # Add boundary
  if(!all(is.na(bound))){
    gp_base<-gp_base + geom_contour(aes(z=Interval),bins=1,color=bound_color,
                                    size=bound_size,breaks=0.25)#,binwidth=0.25)
  }

  # Add multiple boundaries
  if(!all(is.na(bound_list))){
    for(ii in 1:length(bound_list)){
      gp_base<-gp_base + geom_contour(aes_string(z=paste('Interval',ii,sep="_")),
                                      bins=1,color=bound_color[ii],size=1)
    }
  }


  # Save Image
  if(save){
    ggsave(plot_file,gp_base,width=width,height=height,units='cm')
  }

  return(gp_base)

}
