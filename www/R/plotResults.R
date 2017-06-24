#' @title Plot the best Results
#' @name plotResult
#' @description  Plot the best resulting solutions of the genetic algorithm.
#' Depending on \code{plotEn}, either the best energy or efficiency solutions
#' can be plotted. \code{best} indicates the amount of best solutions that
#' should be plotted.
#'
#' @export
#'
#' @importFrom raster crs getData crop mask projectRaster raster getData
#' reclassify plot calc extract cellStats terrain resample overlay res
#' @importFrom sp spTransform
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics mtext par plot
#' @importFrom utils read.csv
#' @importFrom calibrate textxy
#' @importFrom stats dist
#'
#' @param result An output matrix of the function \code{\link{genAlgo}},
#' which has stored all relevant information. (matrix)
#' @param Polygon1 The considered area as shapefile. (SpatialPolygons)
#' @param best A numeric value indicating how many of the best individuals
#' should be plotted. (numeric)
#' @param plotEn A numeric value that indicates if the best energy or
#' efficiency output should be plotted. If (plotEn==1) plots the best energy
#' solutions and (plotEn==2) plots the best efficiency solutions. (numeric)
#' @param topographie A logical value, indicating whether terrain effects
#' should be considered and plotted or not. (logical)
#' @param Grid The grid as SpatialPolygons, which is obtained from
#' \code{\link{GridFilter}} and used for plotting.
#' @param Projection A desired Projection can be used instead
#' of the default Lambert Azimuthal Equal Area Projection. (character)
#'
#' @return Returns a data.frame of the best (energy/efficiency) individual
#' during all iterations. (data.frame)
#'
#' @author Sebastian Gatscha
plotResult <- function(result,Polygon1,best=1,plotEn=1,
                       topographie=FALSE,Grid,Projection){
  # library(raster); library(stats); library(sp); library(calibrate)
  # result = ResG;Polygon1 = shap; best = 1; plotEn = 2; topographie = F; Grid = Grid
  #rm(order2,orderb,orderc,runs,a,ar,by_cycl,b,ProjLAEA,result,i,Polygon1,windraster,windr,best,EnergyBest,EfficiencyBest,Col,Col1,plotEn,op,order1)
  
  ## Set graphical parameters
  op <- par(ask=FALSE);   on.exit(par(op));   par(mfrow=c(1,1))
  
  ## Check Projections and reference systems
  if (missing(Projection)) {
    ProjLAEA = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  } else {
    ProjLAEA <- Projection;
  }
  if (as.character(raster::crs(Polygon1)) != ProjLAEA) {
    Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjLAEA)
  }
  
  
  ## Creat a color ramp
  rbPal1 <- grDevices::colorRampPalette(c('green','red'))
  
  ## Plot Best Energy
  if (plotEn == 1) {
    
    a <- sapply(result[,2], "[", "EnergyOverall")
    b <- data.frame(sapply(a, function(x) x[1]))
    order1 <- order(b, decreasing = T)
    result <- result[,2][order1]
    ledup <- length(result)
    
    rectid <- (lapply(result, function(x) x$Rect_ID));
    
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)
    
    
    result <- result[[best]]
    
    #EnergyBest <- do.call("cbind", result[[i]])
    EnergyBest <- data.frame(result)
    ## Assign the colour depending on the individual wind speed (from windraster and influence)
    br = length(levels(factor(EnergyBest$AbschGesamt)))
    if (br > 1) {
      Col <- rbPal1(br)[as.numeric(cut(as.numeric(EnergyBest$AbschGesamt),breaks = br))]
    } else {
      Col = "green"
    }
    
    EnergyBest$EnergyOverall <- round(EnergyBest$EnergyOverall, 2)
    EnergyBest$EfficAllDir <- round(EnergyBest$EfficAllDir, 2)
    
    plot(Polygon1, col="lightblue", main=paste("Best Energy:", best, "\n","Energy Output",
                                               EnergyBest$EnergyOverall[[1]],"kW", "\n", "Efficiency:",
                                               EnergyBest$EfficAllDir[[1]]));
    # plot(srtm_crop,add=T,alpha=0.4)
    plot(Grid,add=T)
    
    # graphics::mtext("Total wake effect in %", side = 2)
    graphics::points(EnergyBest$X,EnergyBest$Y,cex=2,pch=20,col=Col)
    graphics::text(EnergyBest$X, EnergyBest$Y, round(EnergyBest$AbschGesamt,0), cex=0.8, pos=1, col="black")
    #("Wake Effect in %")
    
    distpo <- stats::dist(x = cbind(EnergyBest$X,EnergyBest$Y),method = "euclidian")
    graphics::mtext(paste("minimal Distance", round(min(distpo),2)), side = 1,line=0)
    graphics::mtext(paste("mean Distance", round(mean(distpo),2)), side = 1,line=1)
    #mtext(paste("max. Distance", round(max(distpo),2)), side = 1,line=2)
    
    
    
    
    ResPlotResult <- EnergyBest
  }
  
  ## Plot Best Efficiency
  if (plotEn == 2){
    a <- sapply(result[,3], "[", "EfficAllDir")
    b <- data.frame(sapply(a, function(x) x[1]))
    order2 <- order(b, decreasing = T)
    result <- result[,3][order2]
    ledup <- length(result)
    rectid <- lapply(result, function(x) x$Rect_ID)
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)
    
    
    result <- result[[best]]
    
    EfficiencyBest <- data.frame(result)
    ## Assign the colour depending on the individual wind speed (from windraster and influence)
    br = length(levels(factor(EfficiencyBest$AbschGesamt)))
    if (br > 1) {
      Col1 <- rbPal1(br)[as.numeric(cut(EfficiencyBest$AbschGesamt,breaks = br))]
    } else {
      Col1 = "green"
    }
    
    EfficiencyBest$EnergyOverall <- round(EfficiencyBest$EnergyOverall, 2)
    EfficiencyBest$EfficAllDir <- round(EfficiencyBest$EfficAllDir, 2)
    raster::plot(Polygon1, col="lightblue", main=paste("Best Efficiency:", best, "\n","Energy Output",
                                                       EfficiencyBest$EnergyOverall[[1]],"kW", "\n", "Efficiency:",
                                                       EfficiencyBest$EfficAllDir[[1]]));
    #plot(windraster,add=T, col=rainbow(50,alpha = 0.3))
    plot(Grid,add=T)
    # graphics::mtext("Gesamtabschattung in %", side = 2)
    
    graphics::points(EfficiencyBest$X,EfficiencyBest$Y,col=Col1,cex=2,pch=20)
    graphics::text(EfficiencyBest$X, EfficiencyBest$Y, round(EfficiencyBest$AbschGesamt,0), cex=0.8, pos=1)
    
    
    distpo <- stats::dist(x = cbind(EfficiencyBest$X,EfficiencyBest$Y),method = "euclidian")
    graphics::mtext(paste("minimal Distance", round(min(distpo),2)), side = 1,line=0)
    graphics::mtext(paste("mean Distance", round(mean(distpo),2)), side = 1,line=1)
    #mtext(paste("max. Distance", round(max(distpo),2)), side = 1,line=2)
    
    
    
    
    ResPlotResult <- EfficiencyBest
  }
  
  return(ResPlotResult)
}

