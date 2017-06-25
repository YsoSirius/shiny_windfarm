
source("www/R/plotWindrose.R", local = T)
source("www/R/calculateEn.R", local = T)
source("www/R/GridFilter.R", local = T)
source("www/R/BaroHoehe.R", local = T)
source("www/R/crossover1.R", local = T)
source("www/R/eucDist.R", local = T)
source("www/R/fitness.R", local = T)
source("www/R/genAlgo.R", local = T)
source("www/R/getRects.R", local = T)
source("www/R/InfluPoints.R", local = T)
source("www/R/mutation.R", local = T)
source("www/R/PointToLine2.R", local = T)
source("www/R/readInteger.R", local = T)
source("www/R/readIntegerSel.R", local = T)
source("www/R/selection1.R", local = T)
source("www/R/splitAt.R", local = T)
source("www/R/StartGA.R", local = T)
source("www/R/trimton.R", local = T)
source("www/R/VekWincelCalc.R", local = T)
source("www/R/windfarmGA.R", local = T)
source("www/R/WinkelCalc.R", local = T)
source("www/R/TestLayouts.R", local = T)
source("www/R/plotResults.R", local = T)
source("www/R/plotEvolution.R", local = T)
source("www/R/plotfitnessevolution.R", local = T)
source("www/R/plotparkfitness.R", local = T)
source("www/R/plotcloud.R", local = T)
source("www/R/PlotTerrain.R", local = T)
source("www/R/leafletPlot.R", local = T)
source("www/R/leafletPlotSing.R", local = T)

`%then%` <- shiny:::`%OR%`
ProjLaea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

library(shiny)
library(raster)
library(maptools)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(maptools)
library(rgdal)
library(raster)
library(rgeos)
library(RColorBrewer)
library(sp)
library(shinyjs)

server <- function(input, output) {
  
  ## Navigation
  observeEvent(input$GridButSum, {
    shinyjs::toggle(id = "HidTextDiv", anim = T,animType = "slide",time=0.5)
  })
  observeEvent(input$GridBut, {
    shinyjs::toggle(id = "HidGridDiv", anim = T,animType = "slide",time=0.5)
  })
  observeEvent(input$TransPol, {
    shinyjs::toggle(id = "HidTransPol", anim = T,animType = "slide",time=0.5)
  })
  observeEvent(input$WindButSum, {
    shinyjs::toggle(id = "WindButDiv", anim = T,animType = "slide",time=0.4)
  })
  observe({
    shinyjs::toggleState(id = "Submit", condition = input$CheckGA_SHID)
  })
  observeEvent(input$Submit, {
    shinyjs::disable(id = "ALLINPUTS")
    shinyjs::disable(id = "ALLINPUTS1")
    shinyjs::disable(id = "HidTransPol")
    shinyjs::disable(id = "HidGridDiv")
    shinyjs::disable(id = "CheckButGA_Go")
  })
  observeEvent(input$Reset, {
    shinyjs::reset(id="PlotZent")
    shinyjs::hide(id="WiPlSHi")
    # shinyjs::reset(id="WiPlSHi")
    shinyjs::hide(id="WindButDiv")
    # shinyjs::reset(id="WindButDiv")
    shinyjs::enable(id = "POCSBO")
    # shinyjs::reset(id="POCSBO")
    shinyjs::enable(id = "POCSBO1")
    # shinyjs::reset(id="POCSBO1")
    shinyjs::enable(id = "POCSBO2")
    # shinyjs::reset(id="POCSBO2")
    shinyjs::enable(id = "POCSBO3")
    # shinyjs::reset(id="POCSBO3")
    shinyjs::enable(id = "POCSBO4")
    # shinyjs::reset(id="POCSBO4")
    shinyjs::enable(id = "POCSBO5")
    shinyjs::reset(id="POCSBO5")
    shinyjs::enable(id = "CheckGA_SHID")
    shinyjs::reset(id="CheckGA_SHID")
    shinyjs::enable(id = "ALLINPUTS")
    # shinyjs::reset(id = "ALLINPUTS")
    shinyjs::enable(id = "ALLINPUTS1")
    # shinyjs::reset(id = "ALLINPUTS1")
    shinyjs::enable(id = "HidTransPol")
    # shinyjs::reset(id = "HidTransPol")
    shinyjs::enable(id = "HidGridDiv")
    # shinyjs::reset(id = "HidGridDiv")
  })
  observe({
    shinyjs::onclick("Wind_SHID", shinyjs::show("WiPlSHi"))
  })
  observe({
    shinyjs::toggle(id = "DtPoTst", condition = input$TestMethod== 'Data')
  })
  
  
  observeEvent(input$absoPBU, {
    shinyjs::toggle(id = "absoPanel", anim = T,animType = "slide",time=0.4)
  })
  observeEvent(input$absoPBU1, {
    shinyjs::toggle(id = "absoPanel1", anim = T,animType = "slide",time=0.4)
  })
  
  observeEvent(input$RGUI1_AB, {
    shinyjs::toggle(id = "RGUI1", anim = T,animType = "slide",time=0.4)
  })
  observeEvent(input$RGUI2_AB, {
    shinyjs::toggle(id = "RGUI2", anim = T,animType = "slide",time=0.4)
  })
  observeEvent(input$RGUI3_AB, {
    shinyjs::toggle(id = "RGUI3", anim = T,animType = "slide",time=0.4)
  })
  observeEvent(input$RGUI4_AB, {
    shinyjs::toggle(id = "RGUI4", anim = T,animType = "slide",time=0.4)
  })
  observeEvent(input$RGUI5_AB, {
    shinyjs::toggle(id = "RGUI5", anim = T,animType = "slide",time=0.4)
  })
  
  
  observe({
    condi <- input$Topo_SHID==T
    shinyjs::toggleState(id = "RGUITERR_AB", condition = condi)
  })
  observeEvent(input$RGUITERR_AB, {
    shinyjs::toggle(id = "RGUITERR", anim = T,animType = "slide",time=0.4)
  })
  
  
  # Shape Read Reactive 
  shape <- reactive({
    req(input$Shape_SHID)
    req(!is.null(input$Shape_SHID))
    myshape<- input$Shape_SHID
    dir<-dirname(myshape[1,4])
    for (i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
    }
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)

	# shpnoext <- sub(".+\\/","",getshp); shpnoext
    # shpnoext <- sub(".shp","",shpnoext);shpnoext
    # getproj <- list.files(dir, pattern="*.prj", full.names=TRUE)
    maptools::readShapePoly(getshp, proj4string = CRS(as.character(input$SpRefSys)));
    # rgdal::readOGR(dsn = dir, layer = shpnoext, p4s = (as.character(input$SpRefSys)), verbose = F);

	
  })
  
  
  ## Real World Map - Static
  colorpal <- reactive({
    input$colorsRL
    ccN <- sapply(input$colorsRL, FUN=function(x) col2rgb(x))
    ccN <- leaflet::colorNumeric(t(ccN),1:5)(1)
    ccN
  })
  opacRL <- reactive({
    input$opacityRL
  })
  output$mymap <- renderLeaflet({
    req(input$Shape_SHID)
    req(!is.null(input$Shape_SHID))
    namesInp <- input$Shape_SHID[,1]
    shiny::validate(
      need(expr = length(grep("shp", namesInp, perl=T, value=F))>0, message = "A (*.shp)-File is missing."),
      need(expr = length(grep("dbf", namesInp, perl=T, value=F))>0, message = "A (*dbf)-File is missing."),
      need(expr = length(grep("shx", namesInp, perl=T, value=F))>0, message = "A (*shx)-File is missing."),
      need(expr = length(grep("prj", namesInp, perl=T, value=F))>0, message = "A (*prj)-File is missing.")
    )
    
    popup <- paste0("<strong>Windfarm Area</strong>")
    input$update
    pal <- colorpal()
    opa <- opacRL()
    isolate(leaflet() %>%
      addProviderTiles(input$bmap, options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data=shape(),weight = 3, popup=popup,
                  color=pal, fillColor = pal,opacity = opa, fillOpacity = opa)
    )
  })
  
  colorpal1 <- reactive({
    input$colorsRL1
    ccN <- sapply(input$colorsRL1, FUN=function(x) col2rgb(x))
    ccN <- leaflet::colorNumeric(t(ccN),1:5)(1)
    ccN
  })
  opacRL1 <- reactive({
    input$opacityRL1
  })
  bord <- reactive({
    input$bord
  })
  output$mymapresult <- renderLeaflet({
    Poly <- shape()
    bSo <- GARes()
    opaP <- opacRL1()
    pal1 <- colorpal1()
    bord <- bord()
    if (nrow(bSo) == 1){
      shinyjs::hide(id = "PlBeOn2", anim = F,time = 0.01)
      shinyjs::hide(id = "RadiWa", anim = F,time = 0.01)
    }
    if (input$PlBeOn2 > nrow(bSo)){
      best <- nrow(bSo)
    } else {
      best <- input$PlBeOn2
    }
    
    leafletPlot(besteSolution = bSo, Polygon1=Poly, best=best, RadiWak = input$RadiWa, 
                IconW = input$TurbIco, opaP = opaP, pal1 = pal1, bord = bord)
  })
  
  output$TstLeaf <- renderLeaflet({
    Poly <- shape()
    bSo <- TSTLAY()
    # req(bSo)
    leafletPlotSing(besteSolution = bSo, Polygon1 = Poly,RadiWak=input$RadiWa1,IconW=input$Ico1, 
                    opaC = input$opacityRL2, colpa=input$colorsRL2, bord1=input$bord1)
  })
  
  
  
  # Print Polygon Input Files
  output$PlotWhatNeed <- renderPrint({
    req(input$Shape_SHID)
    req(!is.null(input$Shape_SHID))
    namesInp <- input$Shape_SHID[,1]
    shiny::validate(
      need(expr = length(grep("shp", namesInp, perl=T, value=F))>0, message = "A (*.shp)-File is missing."),
      need(expr = length(grep("dbf", namesInp, perl=T, value=F))>0, message = "A (*dbf)-File is missing."),
      need(expr = length(grep("shx", namesInp, perl=T, value=F))>0, message = "A (*shx)-File is missing."),
      need(expr = length(grep("prj", namesInp, perl=T, value=F))>0, message = "A (*prj)-File is missing.")
    )
    namesInp
  })
  ## Plot Polygons
  output$SHPplot <-   output$SHPplot1 <-  renderPlot({
    req(input$Shape_SHID)
    req(!is.null(input$Shape_SHID))
    namesInp <- input$Shape_SHID[,1]
    shiny::validate(
      need(expr = length(grep("shp", namesInp, perl=T, value=F))>0, message = "A (*.shp)-File is missing."),
      need(expr = length(grep("dbf", namesInp, perl=T, value=F))>0, message = "A (*dbf)-File is missing."),
      need(expr = length(grep("shx", namesInp, perl=T, value=F))>0, message = "A (*shx)-File is missing."),
      need(expr = length(grep("prj", namesInp, perl=T, value=F))>0, message = "A (*prj)-File is missing.")
    )
    par(bg="gray96")
    plot(shape(), col="orange")
  })
  ## Plot Text about transformed Polygon
  observeEvent(input$GridButSum, {
    shape1 <- shape()
    proj4string(shape1) <- CRS(input$SpRefSys)
    shape1 <- spTransform(shape1,CRSobj = CRS(ProjLaea))
    
    output$GridTable <- renderPrint({"INPUT VALUES: "
      cat("############################ GRID SPACING INPUT VALUES ###########################\n")
      cat("Resolution: ", (input$fcrr_SHID * input$Roto_SHID), "\nProportionality: ", input$Propo_SHID,
          "\nFcr of Rotor: ", input$fcrr_SHID, "\nRotorradius: ", input$Roto_SHID)
      cat("\n\n############################ SUMMARY OF OUTPUT POLYGON #######################\n")
      summary(shape1)
    })
  })
  ## Plot Grid at ActionButton
  observeEvent(input$GridBut, {
      output$GRIDplot <- renderPlot({
        shape1 <- shape()
        
        if (is.na(proj4string(shape()))){
            proj4string(shape1) <- CRS(input$SpRefSys)
            shape1 <- spTransform(shape1,CRSobj = CRS(ProjLaea))

        } else if (proj4string(shape())!=ProjLaea) {
            # proj4string(shape()) <- CRS(ProjLaea)
            shape1 <- spTransform(shape1,CRSobj = CRS(ProjLaea))
        } else {
        }
        par(bg="gray96")
        GridFilter(shape = shape1,resol = (input$fcrr_SHID*input$Roto_SHID),
                   prop = input$Propo_SHID, plotGrid = TRUE)
        
    })
  })
  ## CSV-Read and Plot
  CsvReact <- reactive({
    inFile <- input$Wind_SHID
    if (is.null(inFile)){return()}
    shiny::validate(
      need(expr = grepl(pattern = ".csv",x = inFile), message = "The file must be given in .csv")
    )
    db <- read.table(inFile$datapath, header=T, sep=input$DtSep, quote='"', dec = input$DtDec)
    db$wd <- as.integer(db$wd);
    db
    
  })
  ## Plot Windrose
  output$PlotContent <- renderPlot({
    if (is.null(CsvReact())){return()}
    WindInCSV <- CsvReact()
    shiny::validate(
      need(any(colnames(WindInCSV)=="ws"),"The column with wind speeds must be named 'ws'"),
      need(any(colnames(WindInCSV)=="wd"),"The column with wind directions must be named 'wd'"),
      need(class(WindInCSV$ws)=="numeric"|class(WindInCSV$ws)=="integer", 
           "Wind Speeds must be integer or numeric. Floating point values should be separated by '.' instead of ','. \nPlease consider the tab 'Input File Specification'") %then%
      need(class(WindInCSV$wd)=="numeric"|class(WindInCSV$wd)=="integer",
           "Wind Directions must be integer or numeric. Floating point values should be separated by '.' instead of ','. \nPlease consider the tab 'Input File Specification'")
    )

    # par(bg="gray96")
    plotWindrose(data = WindInCSV, spd = WindInCSV$ws,  dir = WindInCSV$wd, 
                 spdres = input$Windrose_SpBin, dirres = input$Windrose_DirBin,
                 palette= input$ColPAlwin)
  }, bg="transparent")
  ## Print Wind Data Table
  output$tableContent <- DT::renderDataTable({
    
    dfDTt <- CsvReact()
    shiny::validate(
      need(dfDTt != "", "Please select a data set") %then%
      need(expr = (class(dfDTt$ws)=="integer"|(class(dfDTt$ws)=="numeric")), message = "Wind speeds are not numeric.")
    )
    
    LE <- length(unique(as.numeric(dfDTt$ws)));
    if (LE>8){LE=8} ;
    IntCol <- seq(min(dfDTt$ws), max(dfDTt$ws), length.out = LE-1) ;
    
    if (LE==1){
      Col <- brewer.pal(3,input$ColPAlwin)
      datatable(dfDTt) %>% 
        formatStyle('ws',
                    backgroundColor = styleInterval(sort(unique(dfDTt$ws)), values = Col[2:3])
        )
    } else if (LE<=2 && LE !=1) {
      Col <- brewer.pal(3,input$ColPAlwin)
      datatable(dfDTt) %>% 
        formatStyle('ws',
                    backgroundColor = styleInterval(sort(unique(dfDTt$ws)), values = Col)
        )
    } else {
      Col <- brewer.pal(length(IntCol)+1,input$ColPAlwin)
      datatable(dfDTt) %>% formatStyle('ws',
                                       backgroundColor = styleInterval(IntCol, values = Col),
                                       fontWeight='bold'
      )
    }
  })

  
  ## Data Points to Test!
  TestPointIn <- reactive({
    TstPoi <- input$TestLaySh
    if (is.null(TstPoi)){return()}
    shiny::validate(
      need(expr = grepl(pattern = ".csv",x = TstPoi), message = "The file must be given in .csv")
    )
    tsttbl <- read.table(TstPoi$datapath, header=T, sep=";", quote='"', dec = ".", row.names = 1)
    colnames(tsttbl) <- toupper(colnames(tsttbl))
    shiny::validate(
      need(all(c("X","Y") %in% colnames(tsttbl)==T), message="The column names of the Coordinates must be named 'X' and 'Y'") %then%
        need(class(tsttbl$X)=="numeric"|class(tsttbl$X)=="integer", "X-Coordinates must be numeric or integer")  %then%
        need(class(tsttbl$Y)=="numeric"|class(tsttbl$Y)=="integer", "Y-Coordinates must be numeric or integer")
    )
    invisible(tsttbl)
  })
  ## Print the Data Points
  output$TstPoiunRs <- renderPrint({
    cat("Input Point Locations:\n")
    str(TestPointIn())
  })
  ## Test a Layout
  TSTLAY <- eventReactive(input$TeLaAct, {
    shiny::validate(
      need(expr = input$Shape_SHID, message = "The shapefile is missing.") %then%
      need(expr = input$Wind_SHID, message = "The wind file is missing.")
    )
    if (input$TestMethod == 'Data'){
      shiny::validate(
        need(expr = input$TestLaySh, message = "The input point file is missing.")
      )
    }
    
    shinyjs::show(id="TstResShw", anim=T,animType = "fade", time=0.3)
    shinyjs::show(id="TstResShw1", anim=T,animType = "fade", time=0.3)
    shinyjs::show(id="TstRlM", anim=T,animType = "fade", time=0.3)
    
    
    Polygon1 <- shape()
    data.in <- CsvReact()
    Polygon3 <- spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating Energy Outputs", value = 0)
    on.exit(progress$close())
    nWindDi <- length(unique(round((data.in$wd/10),0)*10))
    updateProgress <- function(detail = NULL) {
      progress$inc(amount = 1/nWindDi, detail = detail)
    }
    
    DtPoTst <- TestPointIn()
    
    testLayoutNoRest(Polygon1 = Polygon3,
                     method = input$TestMethod,
                     ranmethod = input$RanMethTe,
                     dataInput = DtPoTst,
                     RotorR = input$Roto_SHID,
                     SurfaceRoughness = input$Surface_SHID,
                     referenceHeight = input$RefHeight_SHID,
                     RotorHeight = input$Height_SHID,
                     n = input$NRanTeLa, 
                     windata = data.in,
                     updateProgress)
  })
  ## Print all the Inputs for the Test
  output$InVarTest <- renderPrint({
    Polygon1 <- shape()
    req(input$Shape_SHID)
    req(!is.null(input$Shape_SHID))
    namesInp <- input$Shape_SHID[,1]
    shiny::validate(
      need(expr = length(grep("shp", namesInp, perl=T, value=F))>0, message = "A (*.shp)-File is missing."),
      need(expr = length(grep("dbf", namesInp, perl=T, value=F))>0, message = "A (*dbf)-File is missing."),
      need(expr = length(grep("shx", namesInp, perl=T, value=F))>0, message = "A (*shx)-File is missing."),
      need(expr = length(grep("prj", namesInp, perl=T, value=F))>0, message = "A (*prj)-File is missing.")
    )
    
    proj4string(Polygon1) <- CRS(input$SpRefSys)
    Polygon3 <- spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    data.in <- CsvReact()
    INPUTSTEST <- list(Polygon1 = Polygon3,
                     method = input$TestMethod,
                     ranmethod = input$RanMethTe,
                     dataInput = input$TestLaySh,
                     RotorR = input$Roto_SHID,
                     SurfaceRoughness = input$Surface_SHID,
                     referenceHeight = input$RefHeight_SHID,
                     RotorHeight = input$Height_SHID,
                     n = input$NRanTeLa, 
                     windata = data.in)
    INPUTSTEST
  })
  ## Print the Results from the Test
  output$ResLayDat <- renderPrint({

    TestResult <- TSTLAY()
    colnames(TestResult)[1] <- "X"
    colnames(TestResult)[2] <- "Y"
    TestResult
  })
  ## Plots the Results of the Test
  observeEvent(input$TeLaAct,{
    output$PltTest <- renderPlot({
      TestResult <- TSTLAY()
      colnames(TestResult)[1] <- "X"
      colnames(TestResult)[2] <- "Y"
      
      PolyGon <- shape()
      
      PolyGon <- spTransform(PolyGon, CRSobj = CRS(ProjLaea));
      par(bg="gray96")
      plotTestResult(TestResult, Polygon1=PolyGon)
      rbPal1 <- colorRampPalette(c('green','red'))
      br = length(levels(factor(TestResult$AbschGesamt)))
      if (br > 1) {
        Col1 <- rbPal1(br)[as.numeric(cut(TestResult$AbschGesamt,breaks = br))]
      } else {
        Col1 = "green"
      }
      TestResult$EnergyOverall <- round(TestResult$EnergyOverall, 2);
      TestResult$EfficAllDir <- round(TestResult$EfficAllDir, 2)
      plot(PolyGon, col="lightblue", main=paste("Energy Output",
                                                 TestResult$EnergyOverall[[1]],"kW", "\n", "Efficiency:",
                                                 TestResult$EfficAllDir[[1]]));

      graphics::points(TestResult$X,TestResult$Y,col=Col1,cex=2,pch=20)
      graphics::mtext("Total Wake Effect of Location in %", side = 1 )
      graphics::text(TestResult$X, TestResult$Y, round(TestResult$AbschGesamt,0), cex=0.8, pos=1)
      })
  })
  ## Download the Test Results!
  output$downloadTD <- downloadHandler(
    filename = function(){
      paste0("ResultTS-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      DownLoadData <- TSTLAY();
      colnames(DownLoadData)[1] <- "X";
      colnames(DownLoadData)[2] <- "Y";
      write.csv(x = DownLoadData, file = file)
    }
  )
  

  ### SRTM Does it work when deployed??
  Srtm <- eventReactive(input$Srtm_PLot,{
    Polygon1 <- shape()
    ProjLAEA = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    Polygon1 <-  sp::spTransform(Polygon1, CRSobj =
                                   raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"));
    extpol <- round(Polygon1@bbox,0)[,2]
    srtm <- local(raster::getData(name = 'SRTM', path = getwd(), download = T, lon=extpol[1], lat=extpol[2]));
    srtm_crop <- raster::crop(srtm, Polygon1);
    srtm_crop <- raster::mask(srtm_crop, Polygon1)
    Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLAEA));
    srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLAEA));
  })
  output$srtmpl <- renderPlot({
    Srtm <- Srtm()
    plot(Srtm)
  })
  
  
  # Run a GA-Optimization
  GARes <- eventReactive(input$Submit, {
    
    shiny::validate(
      need(expr = input$Shape_SHID, message = "The shapefile is missing.") %then%
      need(expr = input$Wind_SHID, message = "The wind file is missing.")
    )
    
    Polygon1 <- (shape())
    data.in <- (CsvReact())
    
    if (is.na(proj4string(Polygon1))){
      proj4string(Polygon1) <- CRS(input$SpRefSys)
      Polygon1 <- sp::spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    } else if (proj4string(Polygon1)!=ProjLaea) {
      Polygon1 <- sp::spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    } else {
    }
    
    nIte <- as.numeric(input$Ite_SHID)
    progress1 <- shiny::Progress$new()
    progress1$set(message = "Computing Data", value = 0)
    on.exit(progress1$close())
    updateProgress1 <- function(detail = NULL) {
      progress1$inc(amount = 1/nIte, detail = detail)
    }
    

    TopoIn <- as.logical(input$Topo_SHID)
    ElitFT <- as.logical(input$Elit_SHID)
    if(input$Trim_SHID=="Probabilistic"){TrimToGA <- TRUE} else {TrimToGA <- FALSE}
    
    if (input$Topo_SHID==TRUE){
      req(input$CLC_TIF)
      if (is.null(input$CLC_TIF)){return()}
      shiny::validate(
        need(expr = grepl(pattern = ".tif",x = input$CLC_TIF), message = "The file must be given in .tif")
      )
      CCLra <- input$CLC_TIF
      ccl <- CCLra[1,4]
      inFile <- input$CLC_Leg
      if (is.null(inFile)){return()}
      shiny::validate(
        need(expr = grepl(pattern = ".csv",x = inFile), message = "The file must be given in .csv")
      )
      CCLRough <- inFile$datapath
    } else {
      ccl = ""
      CCLRough = ""
    }
    
    GA <- genAlgo(Polygon1 = Polygon1,
            Rotor = input$Roto_SHID, 
            fcrR = input$fcrr_SHID,
            n = input$nTurb_SHID,
            SurfaceRoughness =  input$Surface_SHID, 
            referenceHeight = input$RefHeight_SHID,
            Proportionality = input$Propo_SHID,
            iteration = input$Ite_SHID, 
            mutr = input$Mutr_SHID,
            vdirspe = data.in, 
            topograp = TopoIn, 
            RotorHeight = input$Height_SHID,
            elitism = ElitFT, 
            nelit = input$nElit_SHID, 
            selstate = input$Selec_SHID,
            crossPart1 = input$Cross_SHID, 
            trimForce = TrimToGA,
            Projection =  ProjLaea,
            sourceCCL = ccl,
            sourceCCLRoughness = CCLRough,
            updateProgress1
            )
    })
  
  output$ResGA <- renderPrint({
    GARes()
  })

  ResGAPrint <- reactive({
    Polygon1 <- shape()
    TestResult <- GARes()
    
    if (nrow(TestResult) == 1){
      shinyjs::hide(id = "PlBeOn", anim = F,time = 0.01)
      shinyjs::disable(id = "RGUI5_AB")
    } else {
      shinyjs::show(id = "PlBeOn", anim = F,time = 0.01)
      shinyjs::enable(id = "RGUI5_AB")
    }
    if (nrow(TestResult)>=4){
      shinyjs::enable(id = "RGUI2_AB")
      shinyjs::enable(id = "RGUI4_AB")
      shinyjs::show(id = "EvoSp", anim = F,time = 0.01)
      shinyjs::show(id = "EvoAsk", anim = F,time = 0.01)
    } else {
      shinyjs::disable(id = "RGUI2_AB")
      shinyjs::disable(id = "RGUI4_AB")
      shinyjs::hide(id = "EvoSp", anim = F,time = 0.01)
      shinyjs::hide(id = "EvoAsk", anim = F,time = 0.01)
    }

    
    
    if (input$EneEff == "Energy"){plotEn=1} else {plotEn=2}
    if (is.na(proj4string(Polygon1))){
      proj4string(Polygon1) <- CRS(input$SpRefSys)
      Polygon1 <- spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    } else if (proj4string(shape())!=ProjLaea) {
      Polygon1 <- spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    }
    
    if (input$PlBeOn > nrow(TestResult)){
      best <- nrow(TestResult)
      shiny::validate(
        need(expr = (input$PlBeOn > best), message = "Not enough solutions found.")
      )
    } else {
      best <- input$PlBeOn
    }
    
    GRID <- GridFilter(shape = Polygon1, resol= (input$fcrr_SHID*input$Roto_SHID), 
                       prop = input$Propo_SHID, plotGrid = F)
    plotResult(result = TestResult, Polygon1 =  Polygon1, plotEn = plotEn,best = best,
               topographie = F, Grid = GRID[[2]])
  })
  output$ResGA1 <- renderPlot({
    ResGAPrint()
  })
  output$downloadResG <- downloadHandler(
    filename = function() {
                                         paste0("ResultGA-", Sys.Date(), ".csv", sep="")
                                       },
    content = function(file) {
                                         DownLoadData <- ResGAPrint();
                                         write.csv(x = DownLoadData,file =  file)
                                       }
  )
  
  

  output$Evo <- renderPlot({
    TestResult <- GARes()
    shiny::validate(
      need(expr = nrow(TestResult)>=4, message = "Too few generations to plot values")
    )
    if (input$EvoAsk=="Energy"){ask=2}
    else if (input$EvoAsk=="Efficiency"){ask=1}
    plotEvolution(result = TestResult, ask = ask, 
                  spar = input$EvoSp)
  })
  
  output$OvGAP <- renderPlot({
    TestResult <- GARes()
    plotparkfitness(result = TestResult)
    
  })
  output$DifGAP <- renderPlot({
    TestResult <- GARes()
    shiny::validate(
      need(expr = (nrow(TestResult)>=2), message = "Too few generations to plot values")
    )
    plotfitnessevolution(result = TestResult)
  })
  output$ClGAP <- renderPlot({
    TestResult <- GARes()

    shiny::validate(
      need(expr = (nrow(TestResult)>=4), message = "Too few generations to plot values")
    )
    plotCloud(result = TestResult,TRUE)
  })
  
  
  ## Corine and Cover Raster
  options(shiny.maxRequestSize=80*1024^2)  
  CCLr <- reactive({
    req(input$CLC_TIF)
    req(!is.null(input$CLC_TIF))
    CCLra <- input$CLC_TIF
    ccl <- raster::raster(CCLra[1,4])
    Polygon1 <- shape()
    Polygon1 <- spTransform(Polygon1, CRSobj = CRS(ProjLaea))
    cclPoly <- raster::crop(ccl,Polygon1)
    cclPoly1 <- raster::mask(cclPoly,Polygon1)
    
    req(input$CLC_Leg)
    rauhigkeitz <- CCLLeg()
    cclRaster <- raster::reclassify(cclPoly1, matrix(c(rauhigkeitz$GRID_CODE,rauhigkeitz$Rauhigkeit_z),ncol = 2))
  })
  CCLLeg <- reactive({
    inFile <- input$CLC_Leg
    if (is.null(inFile)){return()}
    shiny::validate(
      need(expr = grepl(pattern = ".csv",x = inFile), message = "The file must be given in .csv")
    )
    rauhigkeitz <- read.table(inFile$datapath, header=T, sep=";", quote='"', dec = ".")
  })
  output$CCL <- renderPlot({
    req(input$CLC_TIF)
    req(input$CLC_Leg)
    cclRaster <- CCLr()
    plot(cclRaster, main="Surface Roughness from Corine Land Cover")
  })
  
  ## Plot Terrain Effects
  output$TerrEff <- renderPlot({
    Polygon1 <- shape()
    TestResult <- GARes()
    if (is.na(proj4string(Polygon1))){
      proj4string(Polygon1) <- CRS(input$SpRefSys)
      Polygon1 <- spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    } else if (proj4string(shape())!=ProjLaea) {
      Polygon1 <- spTransform(Polygon1,CRSobj = CRS(ProjLaea))
    }
    
    if (input$PlBeOn1 > nrow(TestResult)){
      best <- nrow(TestResult)
    } else {
      best <- input$PlBeOn1
    }
    
    PlotTerrain(Polygon1 = Polygon1,Energy =  input$TerrPl, Result = TestResult,inputResol =  (input$fcrr_SHID*input$Roto_SHID),
                inputRotHei = input$Height_SHID, best = best)
  })
  
  
  
  ## Summary of Inputs
  output$summary_TERR <- renderPrint({
    if (input$Topo_SHID==TRUE){
      req(input$CLC_TIF)
      req(input$CLC_Leg)
      if (is.null(input$CLC_TIF)){return()}
      shiny::validate(
        need(expr = grepl(pattern = ".tif",x = input$CLC_TIF), message = "The file must be given in .tif")
      )
      inFile <- input$CLC_Leg
      if (is.null(inFile)){return()}
      shiny::validate(
        need(expr = grepl(pattern = ".csv",x = inFile), message = "The file must be given in .csv")
      )
      
      CCLra <- input$CLC_TIF
      ccl <- CCLra[1,4]
      CCLRough <- inFile$datapath
    }
    if (input$Topo_SHID==TRUE){
      cat(c(
        "CORINE LAND COVER  INPUTS",
        "\nCORINE LAND COVER Raster: ", ccl,
        "\nCORINE LAND COVER Legend: ", CCLRough
      ))
    }
  })
  output$summary_SHID <- renderPrint({
    cat(c("WIND INPUTS ",
          "\nReference Height: ",input$RefHeight_SHID,
          
          "\n\nTURBINE INPUTS",
          "\nTurbines: ", input$nTurb_SHID,
          "\nHub_Height: ", input$Height_SHID,
          "\nRotor_Radius: ", input$Roto_SHID,
          
          "\n\nGRID SPACING INPUTS INPUTS",
          "\nFactor_Of_Radius: ", input$fcrr_SHID,
          "\nProportionality: ", input$Propo_SHID,
          
          "\n\nGENETIC ALGORITHM INPUTS",
          "\nIterations: ", input$Ite_SHID,
          "\nSelection_Method: ", input$Selec_SHID,
          "\nCrossover_Method: ", input$Cross_SHID,
          "\nElitism: ", input$Elit_SHID,
          "\nElitism_Size: ", input$nElit_SHID,
          "\nAdjustment_Method: ", input$Trim_SHID,
          "\nMutationrate: ", input$Mutr_SHID,
          
          "\n\nTERRAIN INPUTS",
          "\nSurface_Roughness: ", input$Surface_SHID,
          "\nTopographic_Effects: ", input$Topo_SHID
    ))

    

  })
  
}

