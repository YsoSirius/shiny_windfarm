
PlotTerrain <- function(Polygon1, Energy, Result, inputResol, inputRotHei, best){
  
  resol= as.integer(inputResol)
  RotorHeight <- as.integer(inputRotHei)
  polygon1=Polygon1
  
  if (Energy=="Energy"){
    a <- sapply(Result[,2], "[", "EnergyOverall")
    b <- data.frame(sapply(a, function(x) x[1]))
    order1 <- order(b, decreasing = T)
    result <- Result[,2][order1]
    ledup <- length(result)
    rectid <- (lapply(result, function(x) x$Rect_ID));
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    EBR <-  result[[best]]
  }
  if (Energy =="Efficiency"){
    a <- sapply(Result[,3], "[", "EfficAllDir")
    b <- data.frame(sapply(a, function(x) x[1]))
    order1 <- order(b, decreasing = T)
    result <- Result[,3][order1]
    ledup <- length(result)
    rectid <- (lapply(result, function(x) x$Rect_ID));
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    EBR <-  result[[best]]
  }
  

  
  
  
  
  sel1=EBR[,1:2]
  windpo <- 1
  ProjLaea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84
                                                              +towgs84=0,0,0"));
  extpol <- round(Polygon1@bbox,0)[,2]
  srtm <- raster::getData('SRTM', lon=extpol[1], lat=extpol[2]);
  srtm_crop <- raster::crop(srtm, Polygon1);
  srtm_crop <- raster::mask(srtm_crop, Polygon1)
  
  Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLaea));
  srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLaea));
  
  # Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
  orogr1 <- raster::calc(srtm_crop, function(x) {x/(raster::cellStats(srtm_crop,mean,na.rm=T))})
  orogrnum <- raster::extract(x= orogr1, y = as.matrix((sel1)), buffer=resol*2, small=T,fun= mean,na.rm=T);
  windpo <- windpo * orogrnum
  ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
  heightWind <- raster::extract(x= srtm_crop, y = as.matrix((sel1)), small=T,fun= max,na.rm=T);
  par(mfrow=c(2,2))
  plot(srtm_crop, main="SRTM Elevation Data");graphics::points(sel1$X,sel1$Y,pch=20);
  calibrate::textxy(sel1$X,sel1$Y,labs = round(heightWind,0),cex=0.8);plot(polygon1,add=T)
  plot(orogr1, main="Wind Speed Multipliers");points(sel1$X,sel1$Y,pch=20);
  calibrate::textxy(sel1$X,sel1$Y,labs = round(windpo,3),cex=0.8);plot(polygon1,add=T)
  
  # Get Air Density and Pressure from Height Values
  HeighttoBaro <- matrix(heightWind); colnames(HeighttoBaro) <- "HeighttoBaro"
  air_dt <- BaroHoehe(matrix(HeighttoBaro),HeighttoBaro)
  plot(srtm_crop, main="Normal Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
  calibrate::textxy(sel1$X,sel1$Y,labs = rep(1.225,nrow(sel1)),cex=0.8); plot(polygon1,add=T)
  plot(srtm_crop, main="Corrected Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
  calibrate::textxy(sel1$X,sel1$Y,labs = round(air_dt$rh,2),cex=0.8); plot(polygon1,add=T)
  
  
  
  # INclude Corine Land Cover Raster to get an estimation of Surface Roughness
  # ccl <- raster::raster("C:/Users/Bobo/Documents/STUDIUM/_____WS_2015_16/int_SeminarWind/CLC_2006/g100_06.tif")
  # cclPoly <- raster::crop(ccl,Polygon1); cclPoly1 <- raster::mask(cclPoly,Polygon1)
  # rauhigkeitz <- utils::read.csv("C:/Users/Bobo/Documents/STUDIUM/_____WS_2015_16/int_SeminarWind/CLC_2006/clc_legend.csv",
  #                                header = T,sep = ";");
  # 
  # cclRaster <- raster::reclassify(cclPoly1,
  #                                 matrix(c(rauhigkeitz$GRID_CODE,rauhigkeitz$Rauhigkeit_z),ncol = 2))
  #
  # CorineLandCover Roughness values
  # SurfaceRoughness0 <- raster::extract(x= cclRaster, y = as.matrix((sel1)),buffer=resol*2,
  #                                      small=T,fun= mean,na.rm=T);
  # SurfaceRoughness1 <- raster::extract(x=raster::terrain(srtm_crop,"roughness"), y = as.matrix((sel1)),
  #                                      buffer=resol*2,
  #                                      small=T,fun= mean,na.rm=T);
  # SurfaceRoughness <-SurfaceRoughness0*(1+(SurfaceRoughness1/max(raster::res(srtm_crop))));
  # elrouind <- raster::terrain(srtm_crop,"roughness")
  # elrouindn <- raster::resample(elrouind,cclRaster,method="ngb")
  # modSurf <- raster::overlay(x = cclRaster,y = elrouindn,
  #                            fun=function(x,y){return(x*(1+(y/max(raster::res(srtm_crop)))))})
  # 
  # graphics::par(mfrow=c(1,1)); cexa=0.9
  # raster::plot(cclRaster, main="Corine Land Cover Roughness");points(sel1$X,sel1$Y,pch=20);
  # calibrate::textxy(sel1$X,sel1$Y,labs = round(SurfaceRoughness0,2),cex=cexa); plot(polygon1,add=T)
  # raster::plot(x=raster::terrain(srtm_crop,"roughness",neighbors = 4), main="Elevation Roughness Indicator");
  # graphics::points(sel1$X,sel1$Y,pch=20);
  # calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness1),2),cex=cexa);
  # raster::plot(polygon1,add=T)
  # raster::plot(modSurf, main="Modified Surface Roughness");
  # graphics::points(sel1$X,sel1$Y,pch=20);
  # calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness),2),cex=cexa);
  # raster::plot(polygon1,add=T)
  # 
  # 
  # 
  # k_raster <- raster::calc(modSurf, function(x) {x= 0.5/(log(RotorHeight/x))})
  # # New Wake Decay Constant calculated with new surface roughness values, according to CLC
  # k = 0.5/(log(RotorHeight/SurfaceRoughness))
  # graphics::par(mfrow=c(1,1)); cexa=0.9
  # raster::plot(k_raster, main="Adapted Wake Decay Constant - K");
  # graphics::points(sel1$X,sel1$Y,pch=20);
  # calibrate::textxy(sel1$X,sel1$Y,labs = round((k),3),cex=cexa);
  # raster::plot(polygon1,add=T)
  
  return()
}


# winddata <- as.data.frame(cbind(ws=12,wd=0)); winddata
# ResG <- genAlgo(Polygon1 = shap, Rotor = 20, n = 10, fcrR = 1, referenceHeight = 50, RotorHeight = 100,
#                 SurfaceRoughness = 0.3, Proportionality = 1, iteration = 10, mutr = 0.08,
#                 vdirspe = winddata, topograp = F, elitism = T, nelit = 5, selstate = "FIX",
#                 crossPart1 = "EQU", trimForce = T,
#                 sourceCCL = "C:/Users/Bobo/Desktop/Wing_GA_Pkg/__ExampleFiles/Shapefiles/g100_06.tif",
#                 sourceCCLRoughness = "C:/Users/Bobo/Desktop/Wing_GA_Pkg/__ExampleFiles/Shapefiles/clc_legend.csv")

# Grid <- GridFilter(shap,180,1,F)[[2]]
# plotResult(result = ResG,Polygon1 = shap, best = 1, plotEn = 2, topographie = F, Grid = Grid)
# PlotTerrain(Polygon1 = shap, Energy = "Efficiency",Result = Result,inputResol = 180, inputRotHei =  100, best=3)

