#' Ad Hoc Boundary Algorithm
#'
#' Take any set of pixel assignments and find the global MAP within the set
#'
#' @param Zset a specified set of pixel asignments. One image per row.
#' @param lambda the iterations of the LIRA output. One iteration per row.
#' @param param the iterations of the parameters in the ising model.
#' One iteration per row.
#' @return the MAP in the specified set of pixel assignments
#' @export
#'
#'
adhocBound<-function(Zset=NA,
                     lambda,
                     param){


  while(nrow(Zset)>2){
    Zupdate<-foreach(i=seq(1,nrow(Zset),by=2),.combine=rbind) %dopar% {
      Z1<-Zset[i,]

      if(i == nrow(Zset)){
        Z1
      }else{
      Z2<-Zset[i+1,]

      logs<-postRatio(Z1,Z2,lambda,param)
      R<-ratioCheck(logs[,1],logs[,2])

      if(R>1){
        Z1
      }else{
        Z2
      }
      }

    }

    Zset<-Zupdate
  }

  Z1<-Zset[1,]
  Z2<-Zset[2,]

  logs<-postRatio(Z1,Z2,lambda,param)
  R<-ratioCheck(logs[,1],logs[,2])

  if(R>1){
    return(Z1)
  }else{
    return(Z2)
  }


}
