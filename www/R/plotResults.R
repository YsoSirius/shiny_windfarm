plotResult <- function(result, Polygon1, best = 3, plotEn = 1,
                       topographie = FALSE, Grid, Projection,
                       sourceCCLRoughness, sourceCCL,
                       weibullsrc){
  op <- par(ask = FALSE)
  on.exit(par(op))
  par(mfrow = c(1, 1))
  
  # result = GA
  
  ## Check Projections and reference systems
  Polygon1 <- windfarmGA::isSpatial(Polygon1)
  
  if (missing(Projection)) {
    ProjLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  } else {
    ProjLAEA <- Projection
  }
  if (is.na(sp::proj4string(Polygon1))) {
    ProjLAEA <- result[1,'inputData'][[1]]['Projection',][[1]]
    sp::proj4string(Polygon1) <- ProjLAEA
    # stop("Polygon is not projected.", call. = FALSE )
    message("Polygon is not projected. Same projection from result will be assumed.")
  }
  if (as.character(raster::crs(Polygon1)) != ProjLAEA) {
    Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjLAEA)
  }
  
  if (missing(sourceCCL)) {
    sourceCCL <- NULL
  }
  if (missing(sourceCCLRoughness)) {
    sourceCCLRoughness <- NULL
  }
  
  
  ## Check Weibull Rasters
  if (missing(weibullsrc)) {
    weibullsrc <- NULL
    col2res <- "lightblue"
  } else {
    PolyCrop <- sp::spTransform(Polygon1,
                                CRSobj = sp::proj4string(weibullsrc[[1]]))
    if (class(weibullsrc) == "list" & length(weibullsrc) == 2) {
      wblcroped <- lapply(weibullsrc, function(x){
        raster::crop(x, raster::extent(PolyCrop))})
      wblcroped <- lapply(wblcroped, function(x){
        raster::mask(x, PolyCrop)})
      Erwartungswert <- wblcroped[[2]] * (gamma(1 + (1 / wblcroped[[1]])))
    } else if (class(weibullsrc) == "list" & length(weibullsrc) == 1) {
      wblcroped <- raster::crop(weibullsrc[[1]], raster::extent(PolyCrop))
      wblcroped <- raster::mask(weibullsrc[[1]], PolyCrop)
      Erwartungswert <- wblcroped[[1]]
    } else if (class(weibullsrc) == "RasterLayer") {
      wblcroped <- raster::crop(weibullsrc, raster::extent(PolyCrop))
      wblcroped <- raster::mask(weibullsrc, PolyCrop)
      Erwartungswert <- wblcroped
    }
    col2res <- "transparent"
    alpha <- 0.9
    Erwartungswert <- raster::projectRaster(Erwartungswert, 
                                            crs = CRS(ProjLAEA))
    # plot(Erwartungswert)
  }
  
  
  ## Creat a color ramp
  rbPal1 <- grDevices::colorRampPalette(c('green','red'))
  

  ## Plot Best Energy
  if (plotEn == 1) {
    
    a <- sapply(result[, 2], function(i) subset.matrix(i, 
                                                       select = "EnergyOverall"))
    
    b <- a[1, ]
    order1 <- order(b, decreasing = T)
    result <- result[,2][order1]
    ledup <- length(result)
    
    rectid <- lapply(result, function(x) x[,8])
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
    
    plot(Polygon1, col = "lightblue", 
         main = paste("Best Energy:", best, "\n","Energy Output",
                      EnergyBest$EnergyOverall[[1]], "kW", "\n", "Efficiency:",
                      EnergyBest$EfficAllDir[[1]]))
    
    # plot(srtm_crop,add=T,alpha=0.4)
    plot(Grid, add = T)
    
    # graphics::mtext("Total wake effect in %", side = 2)
    graphics::points(EnergyBest$X,
                     EnergyBest$Y,
                     cex = 2, pch = 20, col = Col)
    graphics::text(EnergyBest$X, 
                   EnergyBest$Y, 
                   round(EnergyBest$AbschGesamt), cex = 0.8, pos = 1, col = "black")
    #("Wake Effect in %")
    
    distpo <- stats::dist(x = cbind(EnergyBest$X,EnergyBest$Y),
                          method = "euclidian")
    graphics::mtext(paste("minimal Distance",
                          round(min(distpo), 2)), side = 1, line = 0)
    graphics::mtext(paste("mean Distance", 
                          round(mean(distpo), 2)), side = 1, line = 1)
    #mtext(paste("max. Distance", round(max(distpo),2)), side = 1,line=2)
    
    
    
    
    ResPlotResult <- EnergyBest
  }
  
  ## Plot Best Efficiency
  if (plotEn == 2) {
    
    a <- sapply(result[, 2], function(i) subset.matrix(i, 
                                                       select = "EfficAllDir"))
    
    b <- a[1, ]
    order2 <- order(b, decreasing = T)

    result <- result[,3][order2]
    ledup <- length(result)
    
    rectid <- lapply(result, function(x) x[,8])
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)
    
    
    result <- result[[best]]
    
    EfficiencyBest <- data.frame(result)
    ## Assign the colour depending on the individual wind speed (from windraster and influence)
    br = length(levels(factor(EfficiencyBest$AbschGesamt)))
    if (br > 1) {
      Col1 <- rbPal1(br)[as.numeric(cut(EfficiencyBest$AbschGesamt, breaks = br))]
    } else {
      Col1 = "green"
    }
    
    EfficiencyBest$EnergyOverall <- round(EfficiencyBest$EnergyOverall, 2)
    EfficiencyBest$EfficAllDir <- round(EfficiencyBest$EfficAllDir, 2)
    raster::plot(Polygon1, col = "lightblue", 
                 main = paste("Best Efficiency:", best, "\n","Energy Output",
                              EfficiencyBest$EnergyOverall[[1]],"kW", "\n", "Efficiency:",
                              EfficiencyBest$EfficAllDir[[1]]))
    
    #plot(windraster,add=T, col=rainbow(50,alpha = 0.3))
    plot(Grid, add = T)
    # graphics::mtext("Gesamtabschattung in %", side = 2)
    
    graphics::points(EfficiencyBest$X, 
                     EfficiencyBest$Y, 
                     col = Col1, cex = 2, pch = 20)
    
    graphics::text(EfficiencyBest$X, 
                   EfficiencyBest$Y, 
                   round(EfficiencyBest$AbschGesamt), cex = 0.8, pos = 1)
    
    
    distpo <- stats::dist(x = cbind(EfficiencyBest$X,
                                    EfficiencyBest$Y),
                          method = "euclidian")
    graphics::mtext(paste("minimal Distance",
                          round(min(distpo), 2)), side = 1, line = 0)
    graphics::mtext(paste("mean Distance",
                          round(mean(distpo), 2)), side = 1, line = 1)
    #mtext(paste("max. Distance", round(max(distpo),2)), side = 1,line=2)
    
    ResPlotResult <- EfficiencyBest
  }
  
  invisible(ResPlotResult)
}