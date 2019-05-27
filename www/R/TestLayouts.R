

windmanipul <- function(dirspeed){
  dirspeed$wd <- round(dirspeed$wd,0)
  dirspeed$wd <-  round(dirspeed$wd/100,1)*100; 
  if (any(names(dirspeed) == "probab") == FALSE) {
    dirspeed$probab <- 100/nrow(dirspeed)
  }
  dirspeed$probab <- round(dirspeed$probab,0)
  if (sum(dirspeed$probab) != 100) {
    dirspeed$probab <- dirspeed$probab*(100/sum(dirspeed$probab))
  }
  if   (any(duplicated(dirspeed$wd)==TRUE)) {
    for (i in 1:nrow(dirspeed[duplicated(dirspeed$wd)==F,])){
      temp <- dirspeed[dirspeed$wd ==  dirspeed[duplicated(dirspeed$wd)==F,][i,'wd'],];
      temp$ws <-with(temp, sum(ws * (probab/sum(probab))));
      temp$probab <- sum(temp$probab);
      dirspeed[dirspeed$wd ==  dirspeed[duplicated(dirspeed$wd)==F,][i,'wd'],]$ws <- round(temp$ws,2)[1]
      dirspeed[dirspeed$wd ==  dirspeed[duplicated(dirspeed$wd)==F,][i,'wd'],]$probab <- round(temp$probab,2)[1]
    }
  }
  dirspeed <- dirspeed[!duplicated(dirspeed$wd)==TRUE,];
  dirspeed <- dirspeed[with(dirspeed, order(wd)), ]
  
  return(dirspeed)
  
}


## Test a layout with GridRestrictions 
CalculEnTest <- function(selection, input,cclRaster,referenceHeight,RotorHeight,
                         SurfaceRoughness,topograp,windraster, Polygon, 
                         dirspeed,srtm_crop,probabDir){
  #selection=Grid_CenterMat;input=inputDatLay;
  resol_fa= as.numeric(input["Grid Shape Factor",1])
  rot = as.numeric(input["Rotorradius",1])
  resol1 =resol_fa*rot
  
  #print(class(selection));print(head(selection));  print(paste("ok1"))
  ## Calculate EnergyOutput for every config i and for every angle j
  e <- calculateEnTST(selection, referenceHeight, RotorHeight,SurfaceRoughness,cclRaster = cclRaster,
                   windraster = windraster, wnkl = 20, distanz=100000, polygon1 = Polygon, 
                   resol=resol1, RotorR = rot, dirSpeed = dirspeed, srtm_crop = srtm_crop,topograp=topograp)
  #print(paste("ok2"))
  # Get a list from unique Grid_ID elements for every park configuration respective to every winddirection considered. 
  ee  <- lapply(e, function(x){split(x, duplicated(x$Punkt_id))$'FALSE'})
  # Select only relevant information from list
  ee  <- lapply(ee, function(x){dplyr::select(x,-Ax,-Ay,-Laenge_B,-Laenge_A,-Windmean,-WakeR,-A_ov, -Punkt_id)})
  # get Energy Output and Efficiency rate for every wind direction and make a data frame
  enOut <- lapply(ee, function(x){ x[1,c(3,8,10)]}); enOut <- do.call("rbind", enOut)
  # Add the Probability of every direction
  enOut$probabDir <- probabDir
  # Calculate the relative Energy outputs respective to the probability of the wind direction
  enOut$Eneralldire <- enOut$Energy_Output_Red * (enOut$probabDir/100); 
  # Calculate the sum of the relative Energy outputs
  enOut$EnergyOverall <- sum(enOut$Eneralldire);
  # Calculate the sum of the relative Efficiency rates respective to the probability of the wind direction
  enOut$Efficalldire <- sum(enOut$Parkwirkungsgrad * (enOut$probabDir/100))
  
  # Get the total Wake Effect of every Turbine for all Wind directions
  AbschGesamt <- lapply(ee, function(x){ data.frame(x$TotAbschProz)}); 
  AbschGesamt <- do.call("cbind",AbschGesamt)
  AbschGesamt$RowSum <- rowSums(AbschGesamt); 
  AbschGesamt <- AbschGesamt$RowSum
  
  # Get the original X / Y - Coordinates of the selected individual
  xundyOrig <- as.data.frame(selection[,2:3]); xundyOrig
  # Add the Efficieny and the Energy Output of all wind directions and add the total Wake Effect of every Point Location
  xundyOrig$EfficAllDir <- enOut$Efficalldire[1];
  xundyOrig$EnergyOverall <- enOut$EnergyOverall[1];
  xundyOrig$AbschGesamt <- AbschGesamt
  
  # Get the Rotor Radius and the Rect_IDs of the park configuration
  dt <-  ee[[1]]; dt <- dplyr::select(dt,RotorR, Rect_ID);dt; 
  # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
  dt <- cbind(xundyOrig,dt)
  return(dt)
  
  
  
}
testLayout <- function(result,Polygon, n, method="Locator",dataInput, RotorR, fcrR,
                       SurfaceRoughness=0.14,topograp="FALSE",referenceHeight=50,RotorHeight=100,
                       PerOfPol=1, windata){

  # result=Res_Loch[[1]];Polygon = Reck;method = "Random"; RotorR = 30;fcrR = 3;  SurfaceRoughness = 0.3;n = 10; windata = data.in
  
  
  ProjLAEA = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  library(rgeos)
  par(mfrow=c(1,1))
  ResolutionT <- (RotorR*fcrR)

  # if (topograp == "TRUE") {
  #   Polygon <-  spTransform(Polygon, CRSobj = crs(ProjLAEA));   
  #   
  #   ccl <- raster("C:/Users/Bobo/Documents/STUDIUM/_____WS_2015_16/int_SeminarWind/CLC_2006/g100_06.tif")
  #   cclPoly <- crop(ccl,Polygon)
  #   cclPoly1 <- mask(cclPoly,Polygon)
  #   rauhigkeitz <- read.csv("C:/Users/Bobo/Documents/STUDIUM/_____WS_2015_16/int_SeminarWind/CLC_2006/clc_legend.csv",
  #                           header = T,sep = ";");rauhigkeitz
  #   cclRaster <- reclassify(cclPoly1, matrix(c(rauhigkeitz$GRID_CODE,rauhigkeitz$Rauhigkeit_z),ncol = 2))
  # }
  
  dry.grid.filtered <- GridFilter(Polygon,resol = ResolutionT,prop = PerOfPol,plotGrid = F)[[2]]
  
  if (method=="Locator"){
    plot(Polygon); plot(dry.grid.filtered,add=T);
    xy_loc <- locator(n=n); points(xy_loc,pch=20);   
    xy_loc <- as.data.frame(do.call("cbind",xy_loc)); 
  } else if (method=="Data"){
    plot(Polygon); plot(dry.grid.filtered,add=T);
    xy_loc <- as.data.frame(dataInput); names(xy_loc)<-c("x","y")
    points(xy_loc,pch=20)
  } else if (method=="Random") {
    plot(Polygon); plot(dry.grid.filtered,add=T);
    xy_loc <- data.frame(coordinates(spsample(dry.grid.filtered,10,"random"))); 
    points(xy_loc,pch=20)
  }
  
  polybound <- lapply(dry.grid.filtered@polygons, function(x) x@Polygons[[1]]@coords)
  pointsInGrid <- lapply(polybound, function(x) point.in.polygon(xy_loc$x,xy_loc$y,x[,1],x[,2]));   
  
  
  pointsInGrid <- do.call("rbind",lapply(pointsInGrid, function(x) x = sum(x)));   
  if (any(pointsInGrid > 1)){cat("Please select only one point per grid element."); stop()}
  idGrids <- which(pointsInGrid==1);   
  Grid_Centers <- gCentroid(dry.grid.filtered[idGrids,],byid = T); 
  points(Grid_Centers,col="red",pch=20);   
  Grid_CenterMat <- coordinates(Grid_Centers)
  
  dirspeed <- windmanipul(windata)
  probabDir <- dirspeed$probab;   pp <- sum(probabDir)/100;   probabDir <- probabDir/pp; 
  
  ## SRTM Daten
  # Polygon1 <-  spTransform(Polygon, CRSobj = crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"));  
  # extpol <- round(Polygon1@bbox,0)[,2]
  # if (topograp=="TRUE"){
  #   srtm <- getData('SRTM', lon=extpol[1], lat=extpol[2]);  
  #   srtm_crop <- crop(srtm, Polygon1); 
  #   srtm_crop <- mask(srtm_crop, Polygon1)
  #   srtm_crop <- projectRaster(srtm_crop, crs = crs(ProjLAEA));
  # }
  
  # MISSING: Including Weibull-Distrib with a and k Parameter to get mean windspeed
  windraster <-rasterize(Polygon, raster(extent(Polygon), ncol=180, nrow=180))
  
  Grid_CenterMat <- as.data.frame(cbind(ID=seq(1,nrow(Grid_CenterMat),1),Grid_CenterMat))
  names(Grid_CenterMat) <- toupper(names(Grid_CenterMat))
  
  opar <- par(no.readonly = TRUE)
  #referenceHeight=50;RotorHeight=100;SurfaceRoughness=0.14;topograp="TRUE";CCL="FALSE";
  EneTest <- CalculEnTest(selection = Grid_CenterMat,input = inputDatLay,windraster = windraster,
                          Polygon = Polygon,dirspeed = dirspeed,srtm_crop = srtm_crop,probabDir = probabDir,
                          referenceHeight = referenceHeight,RotorHeight = RotorHeight,SurfaceRoughness = SurfaceRoughness,
                          topograp = FALSE,cclRaster=cclRaster)
  # EneTest;
  
  EfficiencyBest <- data.frame(EneTest);   rbPal1 <- colorRampPalette(c('green','red'))
  ## Assign the colour depending on the individual wind speed (from windraster and influence)
  br = length(levels(factor(EfficiencyBest$AbschGesamt)))
  if (br > 1) {
    Col1 <- rbPal1(br)[as.numeric(cut(EfficiencyBest$AbschGesamt,breaks = br))]
  } else {
    Col1 = "green"
  }
  
  par(opar)
  EfficiencyBest$EnergyOverall <- round(EfficiencyBest$EnergyOverall, 2);   EfficiencyBest$EfficAllDir <- round(EfficiencyBest$EfficAllDir, 2)
  plot(Polygon, col="lightblue", main=paste("Energy Output", EfficiencyBest$EnergyOverall[[1]],"kW", "\n", "Efficiency:",EfficiencyBest$EfficAllDir[[1]]));
  #plot(windraster,add=T, col=rainbow(50,alpha = 0.3))
  plot(dry.grid.filtered,add=T);   mtext("Gesamtabschattung in %", side = 1 )
  
  points(EfficiencyBest$X,EfficiencyBest$Y,col=Col1,cex=2,pch=20)
  text(EfficiencyBest$X, EfficiencyBest$Y, round(EfficiencyBest$AbschGesamt,0), cex=0.8, pos=1) 
  
  return(EneTest)
}
plotTestResult <- function(TestResult, Polygon1){
  EfficiencyBest <- as.data.frame(TestResult);   
  # str(EfficiencyBest);
  
  rbPal1 <- colorRampPalette(c('green','red'))
  br = length(levels(factor(EfficiencyBest$AbschGesamt)))
  if (br > 1) {
    Col1 <- rbPal1(br)[as.numeric(cut(EfficiencyBest$AbschGesamt,breaks = br))]
  } else {
    Col1 = "green"
  }
  EfficiencyBest$EnergyOverall <- round(EfficiencyBest$EnergyOverall, 2);   
  EfficiencyBest$EfficAllDir <- round(EfficiencyBest$EfficAllDir, 2)
  plot(Polygon1, col="lightblue", main=paste("Energy Output",
                                            EfficiencyBest$EnergyOverall[[1]],"kW", "\n", "Efficiency:",
                                            EfficiencyBest$EfficAllDir[[1]]));
  
  points(x = xy.coords(EfficiencyBest$X, EfficiencyBest$Y),col=Col1,cex=2,pch=20)
  mtext("Total Wake Effect of Location in %", side = 1 )
  # text(x = xy.coords(EfficiencyBest$X, EfficiencyBest$Y), labels=round(EfficiencyBest$AbschGesamt,0), cex=0.8, pos=1)

}




###### Calculate Energy without Grids, No restrictions in space
testLayoutNoRest <- function(Polygon1, n, method="Locator",ranmethod="random", dataInput, RotorR,
                             SurfaceRoughness=0.14,referenceHeight=50,RotorHeight=100,windata,
                             updateProgress=NULL){
  
  # Polygon1 = Reck;method = "Random"; RotorR = 30;SurfaceRoughness = 0.3;n = 10; windata = data.in; ranmethod="random"; SurfaceRoughness=0.14;referenceHeight=50;RotorHeight=100
  
  
  ProjLAEA = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  par(mfrow=c(1,1))
  
  if (method=="Locator"){
    # plot(Polygon1); 
    xy_loc <- locator(n=n); 
    # points(xy_loc,pch=20);   
    xy_loc <- as.data.frame(do.call("cbind",xy_loc)); 
  } else if (method=="Data"){
    # plot(Polygon1); 
    xy_loc <- as.data.frame(dataInput); colnames(xy_loc)<-c("x","y")
    # points(xy_loc,pch=20)
  } else if (method=="Random") {
    # plot(Polygon1); 
    xy_loc <- data.frame(coordinates(spsample(Polygon1,n,ranmethod))); 
    # points(xy_loc,pch=20)
  }
  
  SpOver <- SpatialPoints(xy_loc, proj4string = CRS(ProjLAEA))
  pointsInGrid <- sp::over(SpOver, Polygon1, returnList = F);  
  pIg <- as.numeric(pointsInGrid$id)
  if(any(is.na(pIg))) {cat("A Point may be outside the Polygon."); stop()}
  
  
  dirspeed <- windmanipul(windata)
  probabDir <- dirspeed$probab;   pp <- sum(probabDir)/100;   probabDir <- probabDir/pp; 
  windraster <-rasterize(Polygon1, raster(extent(Polygon1), ncol=180, nrow=180))
  
  selCENG <- as.data.frame(cbind(ID=seq(1,nrow(xy_loc),1),xy_loc))
  names(selCENG) <- toupper(names(selCENG))
  
  EneTest <- CalculEnTestNoGrid(selection = selCENG,
                                windraster=windraster,
                                rot=RotorR,
                                Polygon2 = Polygon1,
                                dirspeed = dirspeed,
                                probabDir = probabDir,
                                referenceHeight = referenceHeight,
                                RotorHeight = RotorHeight,
                                SurfaceRoughness=SurfaceRoughness,
                                updateProgress
  )
  invisible(EneTest)
}
CalculEnTestNoGrid <- function(selection, referenceHeight, RotorHeight, windraster,
                               rot, Polygon2, dirspeed,probabDir, SurfaceRoughness, updateProgress){
  
  e <- calculateEnTST(sel = selection, referenceHeight = referenceHeight, RotorHeight = RotorHeight,
                      SurfaceRoughness = SurfaceRoughness,cclRaster = cclRaster,
                      windraster=windraster, wnkl = 20, distanz=100000, polygon1 = Polygon2, 
                      resol=90, RotorR = rot, dirSpeed = dirspeed, srtm_crop=srtm_crop, topograp=FALSE,
                      updateProgress)
  
  
  ee  <- lapply(e, function(x){split(x, duplicated(x$Punkt_id))$'FALSE'})
  ee  <- lapply(ee, function(x){dplyr::select(x,-Ax,-Ay,-Laenge_B,-Laenge_A,-Windmean,-WakeR,-A_ov, -Punkt_id)})
  enOut <- lapply(ee, function(x){ x[1,c(3,8,10)]}); enOut <- do.call("rbind", enOut)
  enOut$probabDir <- probabDir
  enOut$Eneralldire <- enOut$Energy_Output_Red * (enOut$probabDir/100); 
  enOut$EnergyOverall <- sum(enOut$Eneralldire);
  enOut$Efficalldire <- sum(enOut$Parkwirkungsgrad * (enOut$probabDir/100))
  AbschGesamt <- lapply(ee, function(x){ data.frame(x$TotAbschProz)}); 
  AbschGesamt <- do.call("cbind",AbschGesamt)
  AbschGesamt$RowSum <- rowSums(AbschGesamt); 
  AbschGesamt <- AbschGesamt$RowSum
  xundyOrig <- as.data.frame(selection[,2:3]);
  xundyOrig$EfficAllDir <- enOut$Efficalldire[1];
  xundyOrig$EnergyOverall <- enOut$EnergyOverall[1];
  xundyOrig$AbschGesamt <- AbschGesamt
  dt <-  ee[[1]]; dt <- dplyr::select(dt,RotorR, Rect_ID);
  dt <- cbind(xundyOrig,dt)
  
  
  invisible(dt)
  
  
  
}
calculateEnTST       <- function(sel, referenceHeight, RotorHeight,SurfaceRoughness,
                                 windraster,wnkl,distanz, polygon1,resol,RotorR,dirSpeed,
                                 srtm_crop,topograp,cclRaster, updateProgress){
  
  PlotCalc=FALSE
  
  sel1 = sel[,2:3];
  ## Assign constant values
  cT <- 0.88;   air_rh <- 1.225;   k = 0.075;
  ## Extract values from windraster, which will be 1 in this case, as they will get multiplied
  ## by the incoming wind speed.
  windpo <- raster::extract(x = windraster, y = as.matrix((sel1)), 
                            buffer = resol*1, small = T, fun = mean, na.rm = T)
  
  ## Terrain Effect Model:
  if (topograp == TRUE) {
    ## Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
    orogr1 <- raster::calc(srtm_crop, function(x) {x/(raster::cellStats(srtm_crop,mean,na.rm=T))})
    orogrnum <- raster::extract(x= orogr1, y = as.matrix((sel1)), buffer=resol*2, small=T,fun= mean,na.rm=T);
    windpo <- windpo * orogrnum
    
    ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
    heightWind <- raster::extract(x= srtm_crop, y = as.matrix((sel1)), small=T,fun= max,na.rm=T);heightWind
    ## Plot the elevation and the wind speed multiplier rasters
    if (PlotCalc==TRUE){
      par(mfrow=c(2,1))
      plot(srtm_crop, main="SRTM Elevation Data");points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(heightWind,0),cex=0.8);plot(polygon1,add=T)
      plot(orogr1, main="Wind Speed Multipliers");points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(windpo,3),cex=0.8);plot(polygon1,add=T)
    }
    
    ## Get Air Density and Pressure from Height Values from the function "BaroHoehe"
    HeighttoBaro <- matrix(heightWind); colnames(HeighttoBaro) <- "HeighttoBaro"
    air_dt <- BaroHoehe(matrix(HeighttoBaro),HeighttoBaro)
    air_rh <- as.numeric(air_dt$rh);
    ## Plot he normal and corrected Air Density Values
    if (PlotCalc==TRUE){
      par(mfrow=c(1,1))
      plot(srtm_crop, main="Normal Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = rep(1.225,nrow(sel1)),cex=0.8);plot(polygon1,add=T)
      raster::plot(srtm_crop, main="Corrected Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(air_dt$rh,2),cex=0.8);plot(polygon1,add=T)
    }
    
    ## Corine Land Cover Surface Roughness values and Elevation Roughness
    SurfaceRoughness0 <- raster::extract(x= cclRaster, y = as.matrix((sel1)),buffer=resol*2,
                                         small=T,fun= mean,na.rm=T);SurfaceRoughness0
    SurfaceRoughness1 <- raster::extract(x=raster::terrain(srtm_crop,"roughness"), y = as.matrix((sel1)),
                                         buffer=resol*2, small=T,fun= mean,na.rm=T);SurfaceRoughness1
    SurfaceRoughness <-SurfaceRoughness*(1+(SurfaceRoughness1/max(raster::res(srtm_crop))));
    elrouind <- raster::terrain(srtm_crop,"roughness")
    elrouindn <- raster::resample(elrouind,cclRaster,method="ngb")
    modSurf <- raster::overlay(x = cclRaster,y = elrouindn, fun=function(x,y){return(x*(1+y/max(raster::res(srtm_crop))))})
    ## Plot the different Surface Roughness Values
    if (PlotCalc==TRUE){
      graphics::par(mfrow=c(1,1)); cexa=0.9
      raster::plot(cclRaster, main="Corine Land Cover Roughness");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(SurfaceRoughness0,2),cex=cexa);
      plot(polygon1,add=T)
      raster::plot(x= raster::terrain(srtm_crop,"roughness",neighbors = 4),
                   main="Elevation Roughness Indicator");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness1),2),cex=cexa);
      plot(polygon1,add=T)
      plot(modSurf, main="Modified Surface Roughness");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness),2),cex=cexa);
      plot(polygon1,add=T)
    }
    
    ## New Wake Decay Constant calculated with new surface roughness values
    k = 0.5/(log(RotorHeight/SurfaceRoughness))
    ## Plot resulting Wake Decay Values
    if (PlotCalc==TRUE){
      graphics::par(mfrow=c(1,1)); cexa=0.9
      plot(x= raster::terrain(srtm_crop,"roughness",neighbors = 4),
           main="Adapted Wake Decay Values - K");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round((k),3),cex=cexa);
      plot(polygon1,add=T)
    }
    
  }
  
  ## For every wind direction, calculate the energy output. Do so by rotating Polygon for all angles and
  ## analyze, which turbine is affected by another one to calculate total energy output.
  ## Save Output in a list.
  alllist <- vector("list", nrow(dirSpeed))
  for (index in 1:nrow(dirSpeed)) {
    ## Get the Coordinates of the individual / wind farm
    xyBgldMa <- as.matrix((sel1));
    
    ## Get mean windspeed for every turbine location from windraster
    pointWind <- windpo * dirSpeed$ws[index]
    
    ## Calculate Windspeed according to Rotor Height using wind profile law. Other law possible with
    ## log MISSING:
    pointWind <- pointWind*((RotorHeight/referenceHeight)^SurfaceRoughness);
    pointWind[is.na(pointWind)] <- 0;
    
    ## Get the current incoming wind direction and assign to "angle"
    angle <- -dirSpeed$wd[index];
    
    ## If activated, plots the turbine locations with angle 0 and opens a second frame for rotated
    ## turbine lovations
    if (PlotCalc == TRUE){
      par(mfrow=c(1,2))
      plot(polygon1, main="Shape at angle 0");
      graphics::points(xyBgldMa[,1],xyBgldMa[,2],pch=20)
      calibrate::textxy(xyBgldMa[,1],xyBgldMa[,2], labs = dimnames(xyBgldMa)[[1]],cex=0.8)
      Polygon3 = maptools::elide(polygon1, rotate=angle, center=apply(sp::bbox(polygon1), 1, mean));
      plot(Polygon3, main=c("Shape at angle:", (-1*angle)))
      graphics::mtext(paste("Direction: ", index, "\nfrom total: ", nrow(dirSpeed)), side = 1)
    }
    
    ## Change Coordinates to Spatial Points and rotate them by the incoming wind direction
    ## and rearrange as coordinates again
    xyBgldMa <- sp::SpatialPoints(sp::coordinates(xyBgldMa))
    xyBgldMa <- maptools::elide(xyBgldMa, rotate=angle, center=apply(sp::bbox(polygon1), 1, mean));
    xyBgldMa <- sp::coordinates(xyBgldMa)
    
    ## If activated, plots the rotated turbines in red.
    if (PlotCalc == TRUE){
      graphics::points(xyBgldMa, col="red",pch=20)
    }
    
    ## If Height is taken into account. 3D Modelling of Wake and Overlapping Areas
    DatFram <- cbind(pointWind, xyBgldMa)
    colnames(DatFram) = c("Windmittel","X","Y")
    
    ## Get the influecing points given with incoming wind direction angle and reduce then to data frame
    BgleInf <- InfluPoints(t = xyBgldMa, wnkl =  wnkl, dist = distanz, polYgon = polygon1, dirct = angle)
    dfAll <- do.call("rbind",BgleInf)
    dfAll <- as.data.frame(dfAll)
    
    ## Create a list for every turbine
    windlist = vector("list",length(sel1[,1]))
    
    ## Assign Windspeed to a filtered list with all turbines and add the desired rotor radius to the
    ## data frame
    maxpo <- max(dfAll$Punkt_id)
    for (i in 1:maxpo){
      windlist[[i]] <- dplyr::filter(dplyr::select(dplyr::tbl_df(dfAll), Punkt_id,
                                                   Ax,Ay,Bx,By,Laenge_B,Laenge_A,alpha,
                                                   Windrichtung), Punkt_id==i)
      windlist[[i]]$Windmean <- DatFram[[1]][i];
    }
    windlist <- do.call("rbind", windlist);
    windlist$RotorR <- as.numeric(RotorR);
    
    ## Calculate the wake Radius and the rotor area for every turbine.
    lnro = nrow(windlist); windlist$WakeR <- 0; windlist$Rotorflaeche <- 0
    for (i in 1:lnro){
      RotD <- as.numeric(windlist[i,]$RotorR)
      if (windlist[i,]$Laenge_B != 0) {
        
        if (topograp==TRUE){
          windlist[i,]$WakeR = (((RotD * 2) + (2*k[windlist[i,]$Punkt_id]*
                                                 (as.numeric(windlist[i,]$Laenge_B))))/2)[1]
        } else {
          windlist[i,]$WakeR = (((RotD * 2) + (2*k*(as.numeric(windlist[i,]$Laenge_B))))/2)[1]
        }
      } else {
        windlist[i,]$WakeR = 0
      }
      windlist[i,]$Rotorflaeche = (RotD^2) *pi
    };
    
    ## Calculate the overlapping area and the overlapping percentage.
    windlist$A_ov <- 0; windlist$AbschatInProz <- 0
    for (o in 1:lnro){
      Rotorf <- as.numeric(windlist[o,]$RotorR)
      leA <- windlist[o,]$Laenge_A
      wakr <- windlist[o,]$WakeR;
      if (windlist[o,]$Laenge_B == 0) {
        windlist[o,]$A_ov <- 0;
      } else {
        if ((wakr - Rotorf) >= leA && leA >= 0) {
          windlist[o,]$A_ov <- as.numeric(windlist[o,]$RotorR^2)*pi;
        }
        if (round((wakr + Rotorf),2) <= round(leA,2)) {
          windlist[o,]$A_ov <- 0
        }
        if ((wakr - Rotorf) <= leA && leA <= (wakr+Rotorf))  {
          windlist[o,]$A_ov <- (Rotorf^2 * round(acos((Rotorf^2 - wakr^2 + leA^2) / (2*leA * Rotorf)),4))+
            (wakr^2 * round(acos((wakr^2 - Rotorf^2 + leA^2) / (2*leA * wakr)),4)) -
            ((1/2)*sqrt( round((Rotorf + wakr + leA),6) * round((-Rotorf + wakr + leA ),6)*
                           round((Rotorf - wakr + leA),6) * round((Rotorf + wakr - leA),6)))
        }
      }
      if (windlist[o,]$A_ov != 0) {
        windlist[o,]$AbschatInProz <- round(((as.numeric(windlist[o,]$A_ov)/
                                                windlist[o,]$Rotorflaeche)*100), 2);
      } else {
        windlist[o,]$AbschatInProz <- 0;
      }
      
    }
    
    ## Calculate the wind velocity reduction.
    windlist$V_red <- 0
    for (p in 1:lnro) {
      RotrR <- windlist[p,]$RotorR
      a <- 1- sqrt(1-cT)
      s <- (windlist[p,]$Laenge_B/RotrR);
      if (topograp==TRUE){
        b <- (1 + (k[windlist[p,]$Punkt_id]*s))^2;
      } else {
        b <- (1 + (k*s))^2;b
      }
      aov <- (windlist[p,]$A_ov / windlist[p,]$Rotorflaeche);
      windlist[p,]$V_red <- (aov *(a / b))
      ve <- windlist[p,]$Windmean * windlist[p,]$V_red;
      windlist[p,]$V_red <- ve
    }
    
    ## Calculate multiple wake effects, total wake influence, the new resulting wind velocity
    ## and add the Grid IDs.
    maPi = max(windlist$Punkt_id)
    windlist$V_i <- 0; windlist$TotAbschProz <- 0; windlist$V_New <- 0; windlist$Rect_ID <- 0
    for (z in 1:maPi) {
      windlist[windlist$Punkt_id==z,]$V_i <-  sqrt(sum(windlist[windlist$Punkt_id==z,]$V_red^2))
      windlist[windlist$Punkt_id==z,]$TotAbschProz <-  sum(windlist[windlist$Punkt_id==z,]$AbschatInProz)
      windlist[windlist$Punkt_id==z,]$V_New <-  windlist[windlist$Punkt_id==z,]$Windmean -
        windlist[windlist$Punkt_id==z,]$V_i
      windlist[windlist$Punkt_id==z,]$Rect_ID <-  sel[z,1]
    }
    
    ## Get a reduced dataframe and split duplicated Point_id, since a turbine with fixed Point_id,
    ## can have several influencing turbines and therefore several data frame elements
    windlist2 <- dplyr::select(windlist,-alpha,-AbschatInProz,-Rotorflaeche,-V_red,-V_i)
    windlist1 <- split(windlist2, duplicated(windlist2$Punkt_id))$'FALSE'
    
    ## Calculate Full and reduced Energy Outputs in kW and Park Efficienca in %. Assign the values to
    ## the list
    EneOutRed <- sum(0.593 * (1/2) * air_rh * (windlist1$V_New ^ 3) *
                       ((as.numeric(windlist1$RotorR)^2)*pi),na.rm=T)/1000;
    EneOutFul <- sum(0.593 * (1/2) * air_rh * (windlist1$Windmean^3)*
                       ((as.numeric(windlist1$RotorR)^2)*pi),na.rm=T)/1000;
    Effic <- (EneOutRed*100)/EneOutFul;
    windlist2$Energy_Output_Red <- EneOutRed; windlist2$Energy_Output_Voll <- EneOutFul;
    windlist2$Parkwirkungsgrad <- Effic;
    windlist2$Windrichtung <- as.numeric(windlist2$Windrichtung) * (-1)
    alllist[[index]] <- windlist2
    
    
    
    if (is.function(updateProgress)) {
      detail = paste("for wind direction", index, "out of", nrow(dirSpeed))
      updateProgress(detail);
    }
    
  }
  
  ## Return the list with all relevant information
  invisible(alllist)
}


