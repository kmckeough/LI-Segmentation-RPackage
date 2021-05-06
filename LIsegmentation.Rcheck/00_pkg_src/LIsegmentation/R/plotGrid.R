#' Plot A Grid of Images
#'
#' Plot the source. Options to include coordinates and boundaries.
#'
#' @param img_list image
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
#' @param line_type shape of line for boundary
#' @param axis_color color of axis numbers,text
#' @param legend_horiz orientation of the legend
#' @param legend_text_size size of text of the legend
#' @param n_row number of rows in grid
#' @param n_col number of columns in legend
#' @param barwidth legend width
#' @param barheight legend heigh
#' @param label_margin margins
#' @param col_names names of columns
#' @param row_names names of rows
#' @return the total summation component of the Ising distribution
#' @export
#'
#'
plotGrid<-function(img_list,
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
                   line_type=1,
                     bkg_color='black',
                     legend_color='white',
                     axis_color='white',
                     show_legend=TRUE,
                   legend_horiz=TRUE,
                   legend_text_size=1,
                   n_row=length(img_list),
                   n_col=1,
                      barwidth=0.3,
                   barheight=5*length(img_list),
                      label_margin=c(1,1,1,1),
                   col_names=NA,
                   row_names=NA){


  # Set up Data Frame


    plot_grid<-c()
    max_nbound<-max(unlist(lapply(bound_list,length)))
    #id_list<-1:length(img_list)
    n_id<-length(img_list)
    ii_row<-1
    ii_col<-1
    if(all(is.na(c(row_names,col_names)))){
      row_names<-rep('',n_row)
      col_names<-rep('',n_col)
    }
    names(row_names)<-1:n_row
    names(col_names)<-1:n_col
    for(ii in 1:length(img_list)){
      plot_melt<-melt(img_list[[ii]])
      bound_melt<-c()
      if(!all(is.na(bound_list))){
        for(jj in 1:max_nbound){
          bound<-bound_list[[ii]][[jj]]
          bound[bound==-1]<-0
          bound_melt<-cbind(bound_melt,melt(bound)$value)
        }
        plot_grid<-rbind(plot_grid,cbind(ii_row,
                                         ii_col,
                                       plot_melt$value,
                                       plot_melt$Var1,
                                       plot_melt$Var2,
                                       bound_melt))

      }else{
        plot_grid<-rbind(plot_grid,cbind(ii_row,
                                         ii_col,
                                         plot_melt$value,
                                         plot_melt$Var1,
                                         plot_melt$Var2))
      }
      if(ii_col<n_col){
        ii_col<-ii_col+1
      }else{
        ii_col<-1
        ii_row<-ii_row+1
      }

    }

    plot_grid<-apply(plot_grid,2,as.numeric)
    plot_grid<-as.data.frame(plot_grid)
    if(!all(is.na(bound_list))){
      names(plot_grid)<-c('row_id','col_id','counts','RA','DEC',paste('interval',1:max_nbound,sep=''))
    }else{
      names(plot_grid)<-c('row_id','col_id','counts','RA','DEC')
    }
    plot_grid$row_id<-as.factor(plot_grid$row_id)
    plot_grid$col_id<-as.factor(plot_grid$col_id)

    gp_base<-ggplot(plot_grid,aes(x=RA,y=DEC,fill= counts))+
      #facet_wrap(.~id,nrow=n_row,ncol=n_col)+
      facet_grid(rows=vars(row_id), cols=vars(col_id),
                 labeller = labeller(row_id = row_names,
                                     col_id = col_names))+
      geom_raster(show.legend=show_legend)+
      scale_fill_gradientn(colours = colors,limits=limits, na.value = na.value)+
      theme(plot.background = element_rect(fill = bkg_color),
            legend.background = element_rect(fill="NA"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(label_margin[1],label_margin[2],
                                 label_margin[4],label_margin[3]))+
      ggtitle(title)

    if(legend_horiz){
      gp_base<-gp_base+theme(legend.text=element_text(color=legend_color,size=rel(1*legend_text_size)),
                             legend.title=element_text(color=legend_color,size=rel(1.5*legend_text_size)),
                             legend.position = 'bottom')+
        guides(fill = guide_colourbar(barwidth = barheight,barheight = barwidth,
                                      title.vjust = 1.1))+
        labs(fill=legend_label)
    }else{
      gp_base<-gp_base+theme(legend.text=element_text(color=legend_color,size=rel(1*legend_text_size)),
                             legend.title=element_text(color=legend_color,size=rel(1.5*legend_text_size)))+
        guides(fill = guide_colourbar(barwidth = barwidth,barheight = barheight))+
        labs(fill=legend_label)
    }

      gp_base<-gp_base+theme(axis.ticks=element_blank(),
                             axis.title.x=element_blank(),
                             axis.text.x=element_blank(),
                             axis.text.y=element_blank(),
                             axis.title.y=element_blank(),
                             panel.background=element_blank(),
                             strip.background=element_blank(),
                             strip.text=element_text(colour= legend_color,size=rel(1*legend_text_size)),
                             strip.text.y.right = element_text(angle = 0))
      # Add multiple boundaries
      if(!all(is.na(bound_list))){
        for(ii in 1:max_nbound){
          gp_base<-gp_base + geom_contour(aes_string(z=paste('interval',ii,sep="")),bins=1,
                                          color=bound_color[ii],size=bound_size,
                                          linetype = line_type[ii])
        }
      }


      # Save Image
      if(save){
        ggsave(plot_file,gp_base,width=width,height=height,units='cm')
      }

return(gp_base)

}
