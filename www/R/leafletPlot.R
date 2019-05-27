


leafletPlot <- function(besteSolution, Polygon1, best = 1,
                        RadiWak = 1, IconW = 50, iconcol = 1,
                        opaP = 0.4, pal1 = "blue", bord = 0, ...) {

  a <- sapply(besteSolution[,2], "[", "EnergyOverall")
  b <- data.frame(sapply(a, function(x) x[1]))
  order1 <- order(b, decreasing = T)
  result <- besteSolution[,2][order1]
  rectid <- lapply(result, function(x) x[,8])
  rectidt <- !duplicated(rectid)
  result <- result[rectidt]
  
  besteSolution <- result[[best]]
  # besteSolution <- besteSolution[,2][[best]]
  besteSolutionxy <- besteSolution[,1:2]
  besteSolutionxy <- SpatialPoints(coordinates(besteSolutionxy));
  ProjectionLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 
                  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  ProjectionLonLat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 
                       +towgs84=0,0,0"
  ## Projections. If none is at hand.
  proj4string(besteSolutionxy) <- ProjectionLAEA
  besteSolutionxy <- sp::spTransform(besteSolutionxy, 
                                     CRSobj = ProjectionLonLat)
  ## Transform to matrix after transformation.
  besteSolutionxy <- coordinates(besteSolutionxy)
  ## Transform Polygon1 to Lon/Lat
  Polygon1 <- spTransform(Polygon1, CRSobj = ProjectionLonLat)
  ## Assign Long Lat coordinates to n best Solution and save as data.frame
  besteSolution[,1:2] <- besteSolutionxy
  besteSolution <- as.data.frame(besteSolution)
  
  # headLo <- c(mean(besteSolutionxy[,1]), max(besteSolutionxy[,2]))
  listPopup <- paste(
    "<dl><dt>", best, " Best Wind Farm with: ", 
    round(besteSolution$EnergyOverall, 2), "kWh </dt>",
    "<dd> Total Wake Effect of this Turbine: ", 
    as.character(besteSolution$AbschGesamt),
    "% </dd>")
  
  if (iconcol == 1) {
    windturbIco = "WindTurbine.png"
  } else {
    windturbIco = "windturdk.png"
  }
  
  turbine_icon <- iconList(
    turbine_icon = makeIcon(
      iconUrl = paste0("www/extdata/", windturbIco), 
      iconWidth = IconW, iconHeight = IconW + 20))
  
  colCir <-   grDevices::colorRampPalette(c('green','yellow','red','darkred'))
  br = length(levels(factor(besteSolution$AbschGesamt)))
  
  if (br > 1) {
    ColC <- colCir(br)[as.numeric(cut(besteSolution$AbschGesamt,breaks = br))]
    ## Assign sorted color palette for legend
    ColC1 <- colCir(br)[as.numeric(cut(sort(besteSolution$AbschGesamt),breaks = br))]
    pal <- colorFactor(ColC1, domain = besteSolution$AbschGesamt)
    
  } else {
    ColC <- "green"
    ## Assign sorted color palette for legend
    ColC1 <- colCir(br)
    pal <- colorFactor(ColC1, domain = besteSolution$AbschGesamt)
  }
  
  
  ## Calc radius of effect zone.
  Rad =  (round(besteSolution$AbschGesamt, 2) * RadiWak) / 10 

  ## Start a Leaflet Map with OSM background and another Tile.
  map <- leaflet() %>%
    addTiles(group = "OSM (default)") %>% 
    addProviderTiles("Esri.WorldImagery", group = "Esri") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>% 
    ## Write a Popup with the energy output
    
    
    addPopups(mean(c(
      extent(Polygon1)[2],
      extent(Polygon1)[1])),
      extent(Polygon1)[4] + 0.0005, group = "Title",
      popup = paste("<b>",best,"Best Wind Farm with: ",
                    round(besteSolution$EnergyOverall, 2), "kWh</b>",
                      "<dd><b> Efficiency Rate: ", 
                      round(besteSolution$EfficAllDir, 2), "%</b>",
                      "</dd>"
                      
                      )[[1]], 
              options = popupOptions(closeButton = T, autoPan = T,
                                     closeOnClick = T)) %>% 
    ## Add the turbine symbols
    addMarkers(lng = besteSolutionxy[,1], 
               lat = besteSolutionxy[,2],
               icon = turbine_icon[1], 
               popup = listPopup, group = "Turbines") %>% 
    ## Create Circles in Map
    addCircleMarkers(lng = besteSolutionxy[,1], 
                     lat = besteSolutionxy[,2],
                     radius = Rad,
                     color = ColC,
                     stroke = T, fillOpacity = 0.8, 
                     group = "Wake_Circles") %>% 
    # hideGroup(group="Wake_Circles") %>%  
    
    
    ## Add the Polygon
    addPolygons(data = Polygon1, group = "Polygon", 
                color = pal1, weight = bord,
                fill = T, fillOpacity = opaP) %>% 
    ## Add legend of Wake effects
    addLegend(position = "topleft", pal = pal,
              values = besteSolution$AbschGesamt,
              labFormat = labelFormat(suffix = "%"),
              opacity = 1, title = "Total Wake Effect", 
              layerId = "LGN",
              labels = "Wake") %>% 
    
    ## Add a Raster Image if required
    #   if (windno!=0){
    #     addRasterImage(map, x = windraster,opacity = windno,
    #                    group = "Wind_Speed_Multiplier", colors = "Spectral") %>% 
    #     hideGroup(group="Wind_Speed_Multiplier")
    #     ovLg <- c("Wake_Circles","Title","Polygon","Wind_Speed_Multiplier",
    #               "Turbines")}
    #   if (windno==0){ovLg <- c("Wake_Circles","Title","Polygon","Turbines")}
    
    ## Layers control
  addLayersControl(baseGroups = c("Esri","OSM","Toner"),
                   overlayGroups = c("Wake_Circles",
                                     "Title",
                                     "Polygon",
                                     "Turbines"),
                   options = layersControlOptions(collapsed = T)
  )
  
  map  # Plot the map
}

# leafletPlot(besteSolution = ResG,Polygon1 = shap, best=1, RadiWak = 2, IconW = 40, 
#             bord=4)
