#' Load Partition Function
#'
#' Load the partition function from the file name. Partition function is created
#' using Mathematic files found on Paul Beale's home page:
#' https://spot.colorado.edu/~beale/index1.html
#'
#' @param n size of image
#' @param g_file file name of partition function
#' @return data frame containing the energy (U) and corresponding counts of each state (G)
#' @export
#'
#'


loadPartition<-function(n,
                         g_file){
  #Using Beale output
  g<-scan(g_file,what=character(),sep=',')
  energy<-seq(-n^2*2,n^2*2,by=4)
  G<-sapply(g,function(x){log(as.bigz(x))})
  return(data.frame(U=energy[-c(2,length(g)-1)],G=G[-c(2,length(g)-1)]))
}
