#' LIRA Post-Process
#'
#' Run with LIRA output to re-format .out file to .txt file and show MCMC
#' diagnostic plots
#'
#' @param out LIRA output .out file
#' @param param LIRA output .param file
#' @param burn LIRA output .param file
#' @param plot plot  = TRUE to plot trace plots log likelihood, expected MS counts,
#' and bkgscale (if fit)
#' @param save_file path in which to save the reformatted output. FALSE if not saved
#' @return LIRA iterations in array format, one iteration per row
#' @export
#'


liraPost<-function(out,
                   param,
                   burn=0,
                   plot = TRUE,
                   save_file = FALSE){

  output<-read.table(out,header = FALSE)


  n<-ncol(output)
  iter<-nrow(output)/n
  iter_LIRA<-c()
  for(ii in (burn+1):iter){
    iter_LIRA<-rbind(iter_LIRA,unlist(output[(n*ii+1):(n*(ii+1)),1:n]))
  }

  iter_LIRA<-iter_LIRA[-nrow(iter_LIRA),]

  if(save!=FALSE){
    # remove last row (for some reason LIRA not printing correctly)
    write.table(iter_LIRA,paste(base[ii],'_lira_iters.txt',sep=''))
  }

  param<-as.data.frame(read.table(param,header=TRUE))[(burn+1):(nrow(iter_LIRA)-1),]

  g1<-ggplot(param, aes(y=logPost, x=iteration))+
    geom_line()

  g2<-ggplot(param, aes(y=expectedMSCounts, x=iteration))+
    geom_line()

  g3<-ggplot(param, aes(y=bkgScale, x=iteration))+
    geom_line()

  if(plot){
    grid.arrange(g1, g2,g3, nrow = 2)
  }

  return(iter_LIRA)
}

