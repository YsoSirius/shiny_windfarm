#' @title Plot the evolution of fitness values
#' @name plotEvolution
#' @description  Plot the evolution of energy outputs and efficiency rates
#' over the whole generations. Plots min, mean and max values.
#' @export
#'
#' @importFrom graphics plot lines grid points par
#'
#' @param result The output matrix of \code{\link{genAlgo}}, which has
#' stored all relevant information. (matrix)
#' @param ask Should R wait for interaction for subsequent plotting.
#' Default is "T" (character)
#' @param spar A numeric value determining how exact a spline should
#' be drawn. Default is 0.5 (numeric)
#'
#' @return NULL
#'
#' @author Sebastian Gatscha
plotEvolution <- function(result,ask=1, spar=0.5){
  # result=ResG;ask=T; spar=0.5
  # library(stats);

  
  result1 <- as.data.frame(do.call("rbind", result[,1]))
  x <- 1:length(result1$MaxEnergyRedu)
  
  if (ask==1){
    plot(result1$minParkwirkungsg, xaxt='n', main="Wind Farm Efficiency per Generation", xlab="Generation",
         ylab="Park Efficiency in %", cex=1.2,col="red", pch=20,
         ylim= c(min(result1$minParkwirkungsg),max(result1$maxParkwirkungsg)))
    axis(1,at = 1:nrow(result1),tick=T)
    #mtext(result[,4],side=1,col=Col, at=1:length(dir1))
    grid(col = "black")
    points(result1$meanParkwirkungsg,ylab="MeanxParkwirkungsg", cex=1.2,col="blue", pch=20)
    points(result1$maxParkwirkungsg,ylab="maxParkwirkungsg", cex=1.2,col="green", pch=20)
    
    
    lmin <- smooth.spline(x,result1$minParkwirkungsg, spar=spar); lines(lmin, col='red', lwd=1.2)
    lmea <- smooth.spline(x,result1$meanParkwirkungsg, spar=spar); lines(lmea, col='blue', lwd=1.2)
    lmax <- smooth.spline(x,result1$maxParkwirkungsg, spar=spar); lines(lmax, col='green', lwd=1.2)
  }
  
  
  if (ask==2) {   
     plot(result1$MeanEnergyRedu,xaxt='n',main="Wind Farm Energy Yield per Generation",xlab="Generation",ylab="Energy in kW", cex=1.2,
          col="blue", pch=20, ylim= c(min(result1$MinEnergyRedu),max(result1$MaxEnergyRedu)))
     axis(1,at = 1:nrow(result1),tick=T)
     grid(col = "black")
     points(result1$MaxEnergyRedu,ylab="maxParkwirkungsg", cex=1.2,col="green", pch=20)
     points(result1$MinEnergyRedu, cex=1.2,col="red", pch=20)
     
     emean <- smooth.spline(x,result1$MeanEnergyRedu, spar=spar); lines(emean, col='blue', lwd=1.2)
     emax <- smooth.spline(x,result1$MaxEnergyRedu, spar=spar); lines(emax, col='green', lwd=1.2)
     emin <- smooth.spline(x,result1$MinEnergyRedu, spar=spar); lines(emin, col='red', lwd=1.2)
   }
  
  
}
