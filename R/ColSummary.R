#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#' Minimum, maximum, and variance of discretized kernel column sums used a summary statistics for discretized growth transition kernels
#'
#' Calculates Minimum, maximum,and variance of column sums for a list of discretized kernel matrices as output from \code{\link{SimulateModel}}
#' @param dKernList Discretized kernel list from \code{\link{SimulateModel}} [matrix]
#' @param index Indices of discretized kernel list objects to include [integer]
#' @return Matrix of min, max, and variance of column sums [matrix]
#' @export

ColSummary<-function(dKernList,index=1:365){
  ind<-index
  dKerns<-list()
  for(i in 1:length(index)){
    dKerns[[i]]<-dKernList[[ind[i]]]$dK
  }
  minColSum<-c()
  maxColSum<-c()
  varColSum<-c()
  for(i in 1:(length(dKerns))){
    minColSum[i]<-min(colSums(dKerns[[i]]))
    maxColSum[i]<-max(colSums(dKerns[[i]]))
    varColSum[i]<-var(colSums(dKerns[[i]]))
    }
  return(data.frame(minColSum=minColSum,maxColSum=maxColSum,varColSum=varColSum))
}
