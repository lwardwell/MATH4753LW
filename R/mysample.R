#' Sample Simulation Function
#'
#' @param n Size of sample
#' @param iter Number of iterations
#' @param time Time between iterations
#'
#' @importFrom grDevices rainbow
#' @returns A barplot of the sample
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' mysample(n = 100)
#'
#' # Example 2: Increase the number of iterations
#' mysample(n = 100, iter = 20)
#'
#' # Example 3: Reduce the sleep time between iterations
#' mysample(n = 100, iter = 5, time = 0.1)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
