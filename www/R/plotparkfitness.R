#' @title Plot the genetic algorithm results
#' @name plotparkfitness
#' @description Plot the evolution of fitness values with the influences of
#' selection, crossover and mutation.
#' @export
#'
#' @importFrom graphics par layout lines grid plot points axis legend abline
#' mtext
#' @importFrom grDevices colorRampPalette
#' @importFrom stats smooth.spline
#' @importFrom calibrate textxy
#' @importFrom dplyr select
#'
#'
#' @param result An output matrix of \code{\link{genAlgo}}, which has
#' stored all relevant information. (matrix)
#' @param spar A numeric value determining how exact a spline should
#' be drawn. Default is 0.1 (numeric)
#'
#' @return NULL
#'
#' @author Sebastian Gatscha
plotparkfitness <- function(result,spar=0.5){
  #spar=0.3; result=resultTauern8_17_1;
  # rslt <- as.data.frame(do.call("rbind", result[,'allparkcoeff']))
  mutres <- as.data.frame(do.call("rbind", result[,'mut_rate']))
  nindiv1 <- as.data.frame(do.call("cbind", result[,'nindiv']))
  nindiv1 <- nindiv1[-seq(4,length(nindiv1),4)]

  opar <- graphics::par(no.readonly = T)
  selcross <- unlist(result[,'selcross'])
  selteil <- selcross[seq(2,length(selcross),2)]
  crossteil <- selcross[seq(1,length(selcross),2)]
  
  # graphics::layout(matrix(c(1,1,1,1,2,3,4,5),2,4, byrow = TRUE));
  graphics::layout(matrix(c(1,1,1,2,3,4),2,3, byrow = TRUE));
  
  # graphics::par(mfrow=c(2,2))
  # rbPal <- grDevices::colorRampPalette(c('red','green'));
  # Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness),breaks = 4))]

  # plot(rslt$minparkfitness, xaxt='n', main="Energy Yield per Generation",
  #                ylab="Parkfitness in %", cex=1.2,col="red", pch=20,
  #                ylim= c(min(rslt$minparkfitness),max(rslt$maxparkfitness)));
  # graphics::axis(1,at = 1:nrow(rslt),tick=T)
  # graphics::points(rslt$meanparkfitness,ylab="MeanParkF", cex=1.2,col="blue", pch=20);
  # graphics::points(rslt$maxparkfitness,ylab="maxParkF", cex=1.2,col="green", pch=20)
  # x <- 1:length(rslt$maxparkfitness)
  # lmin <- stats::smooth.spline(x,rslt$minparkfitness, spar=spar); graphics::lines(lmin, col='red', lwd=1.2);
  # lmea <- stats::smooth.spline(x,rslt$meanparkfitness, spar=spar); graphics::lines(lmea, col='blue', lwd=1.2);
  # lmax <- stats::smooth.spline(x,rslt$maxparkfitness, spar=spar); graphics::lines(lmax, col='green', lwd=1.2)
  # graphics::grid(col = "gray")

  graphics::par(mar=c(5,5,3,2))
  farbe <- rep(seq(1,3,1),length(nindiv1)/3);farbe;   ndindiplot <- as.integer(nindiv1)
  plot(ndindiplot,type="b",col=farbe,cex=2,pch=20, main="N-Individuen",axes = FALSE,
       ylab="N",xlab="Generation",ylim=c(0,max(ndindiplot)+100))
  graphics::axis(side = 2,tick = TRUE); axis(side = 1,tick = TRUE,at =seq(1,length(ndindiplot),3),
                                             labels =(1:(length(ndindiplot)/3)))
  graphics::legend("topleft",title="Amount of Individuals in: ",lty = c(1,1,1),cex=0.9,inset = c(0.01,0.01),
         box.lty=0,box.lwd=0,c("Fitness","Selection","Crossover"),col=farbe[1:3],xjust = 0)

  plot(1*100/selteil,ylim=c(20,110),type="b",cex=2,col="green",pch=20,main="Selection percentage",
                 ylab="Percentage",xlab="Generation")
  graphics::grid(col = "gray")
  selrpl <- 1*100/selteil;timeticksel <- which(selrpl>75);   selrplval <- selrpl[selrpl>75]
  calibrate::textxy(timeticksel,selrplval,labs = timeticksel,cex = 0.7)


  plot(crossteil,col=crossteil,main="n Crossoverparts",xlab="Generation", ylab="Crossover Points",
                 ylim=c(1,8),cex=1,pch=15); graphics::grid(col = "gray")
  timetickcro <- which(crossteil>median(crossteil));   crorplval <- crossteil[crossteil>median(crossteil)]
  calibrate::textxy(timetickcro,crorplval,labs = timetickcro,cex = 0.5)

  plot(as.numeric(t(mutres)),type="b",main="Mutation Rate",xlab="Generation",
                 ylab="Mutation Rate",cex=1,pch=15)
  mutrpl <- as.numeric(t(mutres));   timetick <- which(mutrpl>median(mutrpl));
  mutrplval <- mutrpl[mutrpl>median(mutrpl)]
  calibrate::textxy(timetick,mutrplval,labs = timetick,cex = 0.7)
  graphics::grid(col = "gray")

  graphics::par(opar)
}




