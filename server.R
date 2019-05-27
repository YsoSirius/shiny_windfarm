
## SOURCING / INPUTS #################
source("www/R/TestLayouts.R", local = T)
source("www/R/plotResults.R", local = T)
source("www/R/plotparkfitness.R", local = T)
source("www/R/leafletPlot.R", local = T)
source("www/R/leafletPlotSing.R", local = T)



#################

server <- function(input, output, session) {
  v <- reactiveValues(valueButton = 0)
  ## Run Example, whenever Button is Clicked!  ##################
  observeEvent(input$runExample, {
    shinyjs::reset(id = "SpRefSys")
    toggleCssClass(id = "runExample", class = "red")
    v$valueButton = v$valueButton + 1
  })
  output$testbutton <- renderPrint({
    load(file = paste0(getwd(),"/www/extdata/TestPolygon.rds"))
    # str(Polygon1)
    v$valueButton
  })

  observe({
    if(v$valueButton==2) {
      v$valueButton = 0
    }
  })
  
  addTooltip(session, id = "RefHeight_SHID", title = "This is an input.",
             placement = "bottom", trigger = "hover")
  
  ## Navigation ##################
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
    shinyjs::reset(id="ResGA")
    shinyjs::show(id="ResGA",  anim = T,animType = "fade",time=0.1)
  })
  observeEvent(input$Reset, {
    shinyjs::reset(id="PlotZent")
    # shinyjs::hide(id="WiPlSHi")
    shinyjs::reset(id="ResGA")
    shinyjs::hide(id="ResGA", anim = T,animType = "fade",time=0)
    # shinyjs::reset(id="WiPlSHi")
    shinyjs::hide(id="WindButDiv",  anim = T,animType = "fade",time=0)
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
    shinyjs::onclick("runExample",shinyjs::show("WiPlSHi", anim = TRUE, animType = "fade", time = 0))
    shinyjs::onclick("Wind_SHID", shinyjs::show("WiPlSHi", anim = TRUE, animType = "fade", time = 0.2))
    
    # shinyjs::onclick("runExample", shinyjs::hide("WiPlSHi", anim = TRUE, animType = "fade", time = 0.2))
      if (v$valueButton == 1) {
        shinyjs::onclick("runExample",shinyjs::show("WiPlSHi", anim = TRUE, animType = "fade", time = 0))
      } else {
          if (!is.null(input$Wind_SHID)){
            shinyjs::onclick("Wind_SHID", shinyjs::show("WiPlSHi", anim = TRUE, animType = "fade", time = 0.2))
          } else {
            shinyjs::hide("WiPlSHi", anim = TRUE, animType = "fade", time = 0)
          }
      }
  })
  observe({
    shinyjs::toggle(id = "DtPoTst", condition = input$TestMethod== 'Data')
  })
  observeEvent(input$ResProjDef, {
    shinyjs::reset(id="SpRefSys")
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
    shinyjs::toggle(id = "RGUI3", anim = T, animType = "slide", time = 0.4)
  })
  observeEvent(input$RGUI4_AB, {
    shinyjs::toggle(id = "RGUI4", anim = T,animType = "slide",time=0.4)
  })
  observeEvent(input$RGUI5_AB, {
    shinyjs::toggle(id = "RGUI5", anim = T,animType = "slide",time=0.4)
  })
  
  
  # observe({
  #   condi <- input$Topo_SHID==T
  #   shinyjs::toggleState(id = "RGUITERR_AB", condition = condi)
  # })
  observeEvent(input$RGUITERR_AB, {
    shinyjs::toggle(id = "RGUITERR", anim = T, animType = "slide", time = 0.4)
  })
  
  
  ## Shape Read Reactive ##################
  shape <- reactive({
    req(input$Shape_SHID)
    req(!is.null(input$Shape_SHID))
    myshape <- input$Shape_SHID
    dir <- dirname(myshape[1,4])
    for (i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
    }
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    getshx <- list.files(dir, pattern="*.shx", full.names=TRUE)
    getdbf <- list.files(dir, pattern="*.dbf", full.names=TRUE)
    req(getshp)
    req(getshx)
    req(getdbf)
    
    if (suppressWarnings(!is.na(as.numeric(input$SpRefSys)))) {
      proj2num <- as.numeric(input$SpRefSys)
      projUser <- as.character(proj2num); 
      proj2url <- paste0("http://spatialreference.org/ref/epsg/",projUser, "/proj4/"); 
      proj2use <- suppressWarnings(try(readLines(proj2url, warn = F), silent = T)); 
      if (class(proj2use)=="try-error") {
        print("Please add a correct EPSG code.")
        shp <- NULL
      } else {
        shp <- suppressWarnings(maptools::readShapePoly(getshp, proj4string = CRS(proj2use)));
      }
    } else {
      proj4check <- suppressWarnings(try(CRS(input$SpRefSys, doCheckCRSArgs = T), silent=T))
      if (class(proj4check)=="try-error") {
        print("Please add a correct Proj4 text.")     
        shp <- NULL
      } else {
        # shpnoext <- sub(".+\\/","",getshp); shpnoext
        # shpnoext <- sub(".shp","",shpnoext);shpnoext
        # getproj <- list.files(dir, pattern="*.prj", full.names=TRUE)
        shp <- suppressWarnings(maptools::readShapePoly(getshp, proj4string = CRS(as.character(input$SpRefSys))));
        # rgdal::readOGR(dsn = dir, layer = shpnoext, p4s = (as.character(input$SpRefSys)), verbose = F);
      }
    }
    shp
  })
  
  output$prntwhateveritis <- renderPrint({
    class(shape())
  })
  
  shape1 <- reactive({
    req(input$runExample)
    load(file = paste0(getwd(),"/www/extdata/TestPolygon.rds"))
    Polygon1 <- Polygon1
    Polygon1
  })
  
  
  ## Real World Map - Static ##################
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
    if (v$valueButton == 1) {
      shape2 <- shape1()
    } else {
      shape2 <- shape()
    }
    req(!is.null(shape2))
    
    validate(
      need(!is.na(proj4string(shape2)), "Projection is missing.")
    )
    if (proj4string(shape2) != "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"){
      shape2 <- suppressWarnings(try(spTransform(shape2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),silent = T))
    }
    
    if (class(shape2)=="try-error") {
      message("Please add a correct EPSG code.")
    } else {
      popup <- paste0("<strong>Windfarm Area</strong>")
      input$update
      pal <- colorpal()
      opa <- opacRL()
      isolate(leaflet() %>%
                addProviderTiles(input$bmap, options = providerTileOptions(noWrap = TRUE)) %>%
                addPolygons(data=shape2, weight = 3, popup=popup,
                            color=pal, fillColor = pal,opacity = opa, fillOpacity = opa)
      )
    }
    
    
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
    if (v$valueButton == 1) {
      Poly <- shape1()
    } else {
      Poly <- shape()
    }
    validate(
      need(!is.null(Poly), "There must be an error with the projection.")
    )
    
    req(!is.null(GARes()))
    bSo <- GARes()
    opaP <- opacRL1()
    pal1 <- colorpal1()
    bord <- bord()
    if (nrow(bSo) == 1) {
      shinyjs::hide(id = "PlBeOn2", anim = F,time = 0.01, animType = "fade")
      shinyjs::hide(id = "RadiWa", anim = F,time = 0.01, animType = "fade")
    }
    
    
    a <- sapply(bSo[,2], "[", "EnergyOverall")
    b <- data.frame(sapply(a, function(x) x[1]))
    order1 <- order(b, decreasing = T)
    result <- bSo[,2][order1]
    
    rectid <- lapply(result, function(x) x[,8])
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)
    
    
    if (input$PlBeOn2 > ndif) {
      best <- ndif
      shinyjs::show(id = "prntwhateveritis2", animType = "fade", time = 0.1, anim = T)
      output$prntwhateveritis2 <- renderPrint({
        cat(paste("There are just ", best, "best unique layouts available."))
      })
    } else {
      shinyjs::hide(id = "prntwhateveritis2", animType = "fade", time = 0, anim = T)
      best <- input$PlBeOn2
    }
    
    leafletPlot(besteSolution = bSo, Polygon1 = Poly, 
                best = best, RadiWak = input$RadiWa, 
                IconW = input$TurbIco, opaP = opaP, pal1 = pal1, 
                bord = bord, iconcol = input$IcoCol1)
  })
  
  output$TstLeaf <- renderLeaflet({
    # Poly <- shape()
    if (v$valueButton == 1) {
      Poly <- shape1()
    } else {
      Poly <- shape()
    }
    validate(
      need(!is.null(Poly), "There must be an error with the projection.")
    )
    validate(
      need(!is.null(TSTLAY()), "There must be an error with the projection.")
    )
    validate(
      need(!is.na(proj4string(Poly)), "Projection is missing.")
    )
    
    bSo <- TSTLAY()
    # req(bSo)
    leafletPlotSing(besteSolution = bSo, Polygon1 = Poly,RadiWak=input$RadiWa1,IconW=input$Ico1, 
                    opaC = input$opacityRL2, colpa=input$colorsRL2, bord1=input$bord1, iconcol= input$IcoCol)
  })
  
  
  
  ## Print Polygon Input Files ##################
  output$PlotWhatNeed <- renderPrint({
    if (v$valueButton == 1) {
      namesInp <- shape1()
    } else {
      req(input$Shape_SHID)
      req(!is.null(input$Shape_SHID))
      namesInp <- input$Shape_SHID[,1]
      shiny::validate(
        need(expr = length(grep("shp", namesInp, perl=T, value=F))>0, message = "A (*.shp)-File is missing."),
        need(expr = length(grep("dbf", namesInp, perl=T, value=F))>0, message = "A (*dbf)-File is missing."),
        need(expr = length(grep("shx", namesInp, perl=T, value=F))>0, message = "A (*shx)-File is missing.")
        # need(expr = length(grep("prj", namesInp, perl=T, value=F))>0, message = "A (*prj)-File is missing.")
      )
    }
    namesInp
  })
  ## Plot Polygons ##################
  output$SHPplot <-   output$SHPplot1 <-  renderPlot({
    if (v$valueButton == 1) {
      shape2 <- shape1()
    } else {
      req(input$Shape_SHID)
      req(!is.null(input$Shape_SHID))
      namesInp <- input$Shape_SHID[,1]
      shiny::validate(
        need(expr = length(grep("shp", namesInp, perl=T, value=F))>0, message = "A (*.shp)-File is missing."),
        need(expr = length(grep("dbf", namesInp, perl=T, value=F))>0, message = "A (*dbf)-File is missing."),
        need(expr = length(grep("shx", namesInp, perl=T, value=F))>0, message = "A (*shx)-File is missing.")
        # need(expr = length(grep("prj", namesInp, perl=T, value=F))>0, message = "A (*prj)-File is missing.")
      )
      shape2 <- shape()
    }
    req(!is.null(shape2))
    
    par(bg="gray96")
    plot(shape2, col="orange")
  })
  ## Plot Text about transformed Polygon ##################
  observeEvent(input$GridButSum, {
    if (v$valueButton == 1) {
      shape2 <- shape1()
    } else {
      shape2 <- shape()
    }
    

    # if (proj4string(shape2)!=ProjLaea){
    #   shape2 <- spTransform(shape2,CRSobj = CRS(ProjLaea))
    # }

    output$GridTable <- renderPrint({"INPUT VALUES: "
      cat("############################ GRID SPACING INPUT VALUES ###########################\n")
      cat("Resolution: ", (input$fcrr_SHID * input$Roto_SHID), "\nProportionality: ", input$Propo_SHID,
          "\nFcr of Rotor: ", input$fcrr_SHID, "\nRotorradius: ", input$Roto_SHID)
      cat("\n\n############################ SUMMARY OF OUTPUT POLYGON #######################\n")
      # req(!is.null(shape2))
      summary(shape2)
    })
  })
  ## Plot Grid at ActionButton ##################
  observeEvent(input$GridBut, {
    output$GRIDplot <- renderPlot({
      req(input$SpRefSys)
      req(!is.null(input$SpRefSys))
      
      if (v$valueButton == 1) {
        shape2 <- shape1()
      } else {
        shape2 <- shape()
      }
      req(!is.null(shape2))
      
      shape3 <- shape2
      
      if (suppressWarnings(!is.na(as.numeric(input$SpRefSys)))) {
        proj2num <- as.numeric(input$SpRefSys)
        projUser <- as.character(proj2num); 
        proj2url <- paste0("http://spatialreference.org/ref/epsg/",projUser, "/proj4/"); 
        proj2use <- suppressWarnings(try(readLines(proj2url, warn = F), silent = T)); 
        
        if (class(proj2use)=="try-error") {
          message("Please add a correct EPSG code.")
        } else {
          if (proj4string(shape2) != as.character(proj2use)) {
            shape2 <- suppressWarnings(try(spTransform(shape2, CRSobj = CRS(proj2use)),silent = T))
          } else {
            raster::plot(shape3, col="blue")
            title("There seems to be an error with the Projection.", col.main="red")
          }
        }
        
      } else {
        proj4check <- suppressWarnings(try(CRS(input$SpRefSys, doCheckCRSArgs = TRUE), silent=TRUE))
        if (class(proj4check)=="try-error") {
          message("Please add a correct Proj4 text.")          
        } else {
          if (proj4string(shape2) != as.character(input$SpRefSys)) {
            shape2 <- suppressWarnings(try(spTransform(shape2, CRSobj = CRS(input$SpRefSys)),silent = TRUE))
          }
        }
        
      }
      

      # if (class(shape2)!="try-error") {
        # shape2 <- suppressWarnings(try(spTransform(shape2, CRSobj = CRS(ProjLaea)), silent = T))
        shape2 <- suppressMessages(suppressWarnings(try(sp::spTransform(shape2, CRSobj = CRS(ProjLaea)), silent = T)))
        
      # }
      
      par(bg="gray96")
      if (class(shape2)=="try-error") {
        raster::plot(shape3, col="yellow")
        title("There seems to be an error with the Projection.", col.main="red")
      } else {   
        if (class(shape2) == "SpatialPolygonsDataFrame") {
          acor <- suppressWarnings(try(GridFilter(shape = shape2,resol = (input$fcrr_SHID*input$Roto_SHID),
                     prop = input$Propo_SHID, plotGrid = TRUE), silent = TRUE))
          if (class(acor)=="try-error") {
            raster::plot(shape3, col="green")
            title("There seems to be an error with the Projection.", col.main="red")
          }
        } else {
          raster::plot(shape3, col="orange")
          title("There seems to be an error with the Projection.", col.main="red")
        }
      }
    })
  })
  ## CSV-Read and Plot ##################
  CsvReact <- reactive({
    if (v$valueButton == 1) {
      winddataframe <- as.data.frame(cbind(ws=sample(1:25,3), wd = sample(1:260,3)))
      # winddataframe
    } else {
      # req(input$Wind_SHID)
      inFile <- input$Wind_SHID
      if (is.null(inFile)){return()}
      shiny::validate(
        need(expr = grepl(pattern = ".csv",x = inFile), message = "The file must be given in .csv")
      )
      winddataframe <- read.table(inFile$datapath, header=T, sep=input$DtSep, quote='"', dec = input$DtDec)
      
      validate(
        need(any(colnames(winddataframe)=="wd"), message = "No column named 'wd' (Wind Directions). "),
          need(any(colnames(winddataframe)=="ws"), message = "No column named 'ws' (Wind Speeds).")%then%
          need(is.numeric((winddataframe$wd)), message = "The Wind Directions are not numeric.")%then%
          need(is.numeric((winddataframe$ws)), message = "The Wind Speeds are not numeric.")%then%
          need(min(winddataframe$wd)>=0, message = "The Wind Directions should go from 0 to 360.")%then%
          need(max(winddataframe$wd)<=360, message = "The Wind Directions should go from 0 to 360.")
        # need(any(colnames(winddataframe)=="wd"), message = "No column named 'wd' (Wind Directions). "),
        #   need(any(colnames(winddataframe)=="ws"), message = "No column named 'ws' (Wind Speeds)."),
        #   need(is.numeric((winddataframe$wd)), message = "The Wind Directions are not numeric."),
        #   need(is.numeric((winddataframe$ws)), message = "The Wind Speeds are not numeric."),
        #   need(min(winddataframe$wd)>=0, message = "The Wind Directions should go from 0 to 360."),
        #   need(max(winddataframe$wd)<=360, message = "The Wind Directions should go from 0 to 360.")
      )

      winddataframe$wd <- as.integer(winddataframe$wd);
      # winddataframe
    }
    winddataframe
  })
  ## Plot Windrose ##################
  output$PlotContent <- renderPlot({
    # if (is.null(CsvReact())){return()}
    WindInCSV <- CsvReact()
    shiny::validate(
      need(WindInCSV != "", "Please select a data set") %then%
        need(expr = (class(WindInCSV$ws)=="integer"|(class(WindInCSV$ws)=="numeric")), message = "Wind speeds are not numeric.")
    )
    # shiny::validate(
    #   need(any(colnames(WindInCSV)=="ws"),"The column with wind speeds must be named 'ws'"),
    #   need(any(colnames(WindInCSV)=="wd"),"The column with wind directions must be named 'wd'"),
    #   need(class(WindInCSV$ws)=="numeric"|class(WindInCSV$ws)=="integer", 
    #        "Wind Speeds must be integer or numeric. Floating point values should be separated by '.' instead of ','. \nPlease consider the tab 'Input File Specification'") %then%
    #     need(class(WindInCSV$wd)=="numeric"|class(WindInCSV$wd)=="integer",
    #          "Wind Directions must be integer or numeric. Floating point values should be separated by '.' instead of ','. \nPlease consider the tab 'Input File Specification'")
    # )
    
    # par(bg="gray96")
    plotWindrose(data = WindInCSV, spd = WindInCSV$ws,  dir = WindInCSV$wd, 
                 spdres = input$Windrose_SpBin, dirres = input$Windrose_DirBin,
                 palette= input$ColPAlwin)
  }, bg="transparent")
  ## Print Wind Data Table ##################
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
  
  
  ## Data Points to Test! ##################
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
  ## Print the Data Points ##################
  output$TstPoiunRs <- renderPrint({
    cat("Input Point Locations:\n")
    str(TestPointIn())
    cat("Wind Data:\n")
    str(CsvReact())
  })
  ## Test a Layout  ##################
  TSTLAY <- eventReactive(input$TeLaAct, {
    
    if (v$valueButton == 1) {
      shptest <- shape1()
    } else {
      shptest <- input$Shape_SHID
    }
    shiny::validate(
      need(expr = !is.null(shptest), message = "The shapefile is missing.") %then%
        need(expr = !is.null(CsvReact()), message = "The wind file is missing.")
    )

    if (input$TestMethod == 'Data'){
      shiny::validate(
        need(expr = input$TestLaySh, message = "The input point file is missing.")
      )
    }
    
    shinyjs::show(id="TstResShw", anim=T,animType = "fade", time=0.3)
    shinyjs::show(id="TstResShw1", anim=T,animType = "fade", time=0.3)
    shinyjs::show(id="TstRlM", anim=T,animType = "fade", time=0.3)
    
    
    if (v$valueButton == 1) {
      shape2 <- shape1()
    } else {
      shape2 <- shape()
    }
    shiny::validate(
      need(expr = req(!is.null(shape2)), message = "Projection is missing.")
    )
    
    
    Polygon1 <- shape2
    data.in <- CsvReact()
    
    
    shiny::validate(
      need(expr = !is.na(proj4string(Polygon1)), message = "Projection is missing.")
    )
    # if (proj4string(shape2) != as.character(input$SpRefSys)) {
    #   shape2 <- spTransform(shape2, CRSobj = CRS(input$SpRefSys))
    # }
    # 
    
    if (proj4string(Polygon1)!=ProjLaea) {
      Polygon3 <- suppressWarnings(try(spTransform(Polygon1, CRSobj = CRS(ProjLaea)), silent = T))
    } else {
      Polygon3 <- Polygon1
    }
    
    # par(bg="gray96")
    if (class(Polygon3) == "SpatialPolygonsDataFrame") {
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating Energy Outputs", value = 0)
      on.exit(progress$close())
      nWindDi <- length(unique(round((data.in$wd/10),0)*10))
      updateProgress <- function(detail = NULL) {
        progress$inc(amount = 1/nWindDi, detail = detail)
      }
      
      DtPoTst <- TestPointIn()
      
      a <- testLayoutNoRest(Polygon1 = Polygon3,
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
    } else {
      # raster::plot(shape2, col="red")
      # stop("There seems to be an error with the Projection.")
      a <- NULL
    }
    a
  })

  
  ## Print all the Inputs for the Test ##################
  output$InVarTest <- renderPrint({
    
    if (v$valueButton == 1) {
      shape2 <- shape1()
    } else {
      req(input$Shape_SHID)
      req(!is.null(input$Shape_SHID))
      namesInp <- input$Shape_SHID[,1]
      shiny::validate(
        need(expr = length(grep("shp", namesInp, perl=T, value=F))>0, message = "A (*.shp)-File is missing."),
        need(expr = length(grep("dbf", namesInp, perl=T, value=F))>0, message = "A (*dbf)-File is missing."),
        need(expr = length(grep("shx", namesInp, perl=T, value=F))>0, message = "A (*shx)-File is missing.")
        # need(expr = length(grep("prj", namesInp, perl=T, value=F))>0, message = "A (*prj)-File is missing.")
      )
      shape2 <- shape()
    }
    req(!is.null(shape2))
    
    
    # if (proj4string(shape2) != as.character(input$SpRefSys)) {
    #   shape2 <- spTransform(shape2, CRSobj = CRS(input$SpRefSys))
    # }
    shiny::validate(
      need(expr = !is.na(proj4string(shape2)), message = "Projection is missing.")
    )
    if (proj4string(shape2)!=ProjLaea) {
      shape2 <- suppressWarnings(try(spTransform(shape2, CRSobj = CRS(ProjLaea)), silent = T))
    } else {
      shape2 <- shape2
    }

    if (class(shape2) == "SpatialPolygonsDataFrame") {
      # Polygon3 <- spTransform(shape2,CRSobj = CRS(ProjLaea))
      data.in <- CsvReact()
      INPUTSTEST <- list(shape = shape2,
                         method = input$TestMethod,
                         ranmethod = input$RanMethTe,
                         dataInput = input$TestLaySh,
                         RotorR = input$Roto_SHID,
                         SurfaceRoughness = input$Surface_SHID,
                         referenceHeight = input$RefHeight_SHID,
                         RotorHeight = input$Height_SHID,
                         n = input$NRanTeLa, 
                         windata = data.in,
                         Projection = input$SpRefSys)
      INPUTSTEST
    } else {
      cat("There seems to be an error with the Projection.")
    }
    
  })
  
  
  ## Print the Results from the Test ##################
  output$ResLayDat <- renderPrint({
    validate(
      need(!is.null(TSTLAY()), label= "Correct Projection")
    )
    
    TestResult <- TSTLAY()
    colnames(TestResult)[1] <- "X"
    colnames(TestResult)[2] <- "Y"
    TestResult
  })
  ## Plots the Results of the Test ##################
  observeEvent(input$TeLaAct,{
    output$PltTest <- renderPlot({
      if (v$valueButton == 1) {
        PolyGon <- shape1()
      } else {
        PolyGon <- shape()
      }
      
      validate(
        need(!is.null(TSTLAY()), "There must be an error with the projection.")%then%
          need(!is.null(PolyGon), "There must be an error with the projection.")
      )
      
      # PolyGon <- shape()
      # req(!is.null(PolyGon))
      
      TestResult <- TSTLAY()
      colnames(TestResult)[1] <- "X"
      colnames(TestResult)[2] <- "Y"
      
      shpe3save <- PolyGon
      
      shiny::validate(
        need(expr = !is.na(proj4string(PolyGon)), message = "Projection is missing.")
      )
      if (proj4string(PolyGon)!=ProjLaea) {
        PolyGon <- suppressWarnings(try(spTransform(PolyGon, CRSobj = CRS(ProjLaea)), silent = T))
      } else {
        PolyGon <- PolyGon
      }
      
      if (class(PolyGon) == "SpatialPolygonsDataFrame") {
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
      } else {
        par(bg="gray96")
        raster::plot(shpe3save, col="red")
        title("There seems to be an error with the Projection.", col.main="red")
      }
      # PolyGon <- spTransform(PolyGon, CRSobj = CRS(ProjLaea));
    })
  })
  
  
  ## Download the Test Results! ##################
  output$downloadTD <- downloadHandler(
    filename = function(){
      paste("ResultTS_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      DownLoadData <- TSTLAY()
      colnames(DownLoadData)[1] <- "X"
      colnames(DownLoadData)[2] <- "Y"
      
      write.table(x = DownLoadData, file = file, sep = ";", quote = FALSE)
    }
  )
  
  
  ## SRTM Does it work when deployed?? ##################
  Srtm <- eventReactive(input$Srtm_PLot,{
    Polygon1 <- shape()
    req(!is.null(Polygon1))
    
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
  
  
  ## Run a GA-Optimization ##################
  GARes <- eventReactive(input$Submit, {
    if (v$valueButton == 1) {
      Polygon1 <- shape1()
    } else {
      Polygon1 <- shape()
    }
    
    shiny::validate(
      need(expr = Polygon1, message = "The shapefile is missing.") %then%
        need(expr = CsvReact(), message = "The wind file is missing.")
    )
    validate(
      need(!is.na(proj4string(Polygon1)), "Projection is missing.")
    )
    
    data.in <- CsvReact()
    
    
    ##______________________________________________________________________
    if (suppressWarnings(!is.na(as.numeric(input$SpRefSys)))) {
      proj2num <- as.numeric(input$SpRefSys)
      projUser <- as.character(proj2num); 
      proj2url <- paste0("http://spatialreference.org/ref/epsg/",projUser, "/proj4/"); 
      proj2use <- suppressWarnings(try(readLines(proj2url, warn = F), silent = T)); 
      
      if (class(proj2use)=="try-error") {
        cat("Please add a correct EPSG code. GARes")
        Polygon1 <- NULL
      } else {
        if (proj4string(Polygon1) != as.character(proj2use)) {
          Polygon1 <- spTransform(Polygon1, CRSobj = CRS(proj2use))
          Polygon1 <- sp::spTransform(Polygon1,CRSobj = CRS(ProjLaea))
        }
      }
      
    } else {
      proj4check <-  suppressWarnings(try(CRS(input$SpRefSys, doCheckCRSArgs = T), silent=T))
      # proj4check <-  try(CRS(input$SpRefSys, doCheckCRSArgs = T), silent=T)
      
      if (class(proj4check)=="try-error") {
        print("Please add a correct Proj4 text. GARes")      
        Polygon1 <- NULL
      } else {
        if (proj4string(Polygon1) != as.character(input$SpRefSys)) {
          Polygon1 <- suppressWarnings(try(sp::spTransform(Polygon1, CRSobj = CRS(input$SpRefSys)), silent=T))
          # Polygon1 <- sp::spTransform(Polygon1, CRSobj = CRS(input$SpRefSys))
          
          projPossib <-  suppressWarnings(try(sp::spTransform(Polygon1,CRSobj = CRS(ProjLaea)), silent=T))
          # projPossib <-  sp::spTransform(Polygon1,CRSobj = CRS(ProjLaea))
          
          if (class(projPossib)=="try-error" | class(Polygon1)=="try-error") {
            print("Error: Something is wrong with the Projection. Check if the original Projection is correct.  GARes")      
            Polygon1 <- NULL
          } else {
            Polygon1 <- projPossib
          }
        }
      }
    }
    ##______________________________________________________________________
    
    
    Gridtest <- suppressWarnings(try(
      GridFilter(shape = Polygon1, resol = input$fcrr_SHID * input$Roto_SHID, 
                         prop = input$Propo_SHID, plotGrid = F), 
      silent = T
    ))
    
    if (class(Gridtest) == "try-error") {
      print("No Grid could be drawn. Check the original Projection. GARes")   
      GA <- NULL

    } else {
      
      Startgatest <- suppressWarnings(try(
        StartGA(Gridtest[[1]], input$nTurb_SHID, nStart = 2), 
        silent = T
      ))
      if (class(Startgatest)=="try-error") {
        cat(paste("The amount of Grid-cells should at least be double the amount of turbines requested.",
                  "\nDecrease the resolution, the number of turbines or the rotor radius.\n"))  
        GA <- NULL
      } else {
        if (is.null(Polygon1)) {
          print("Something went wrong.")
          GA <- NULL
        } else {
          nIte <- as.numeric(input$Ite_SHID)
          progress1 <- shiny::Progress$new()
          progress1$set(message = "Computing Data", value = 0.1)
          on.exit(progress1$close())
          updateProgress1 <- function(detail = NULL) {
            progress1$inc(amount = 1/nIte, detail = detail)
          }
          
          
          # TopoIn <- as.logical(input$Topo_SHID)
          ElitFT <- as.logical(input$Elit_SHID)
          if (input$Trim_SHID == "Probabilistic") {
            TrimToGA <- TRUE
          } else {
            TrimToGA <- FALSE
          }
          
          # if (input$Topo_SHID==TRUE){
          #   req(input$CLC_TIF)
          #   if (is.null(input$CLC_TIF)){return()}
          #   shiny::validate(
          #     need(expr = grepl(pattern = ".tif",x = input$CLC_TIF), message = "The file must be given in .tif")
          #   )
          #   CCLra <- input$CLC_TIF
          #   ccl <- CCLra[1,4]
          #   inFile <- input$CLC_Leg
          #   if (is.null(inFile)){return()}
          #   shiny::validate(
          #     need(expr = grepl(pattern = ".csv",x = inFile), message = "The file must be given in .csv")
          #   )
          #   CCLRough <- inFile$datapath
          # } else {
          ccl = ""
          CCLRough = ""
          # }
          TopoIn = FALSE
          
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
                        sourceCCLRoughness = CCLRough
                        # ,updateProgress1
          )
        }
      }
      
      
    }
    # GA
  })
  
  observe({
    TestResult <- GARes()
    if (nrow(TestResult) == 1) {
      shinyjs::hide(id = "PlBeOn", anim = F,time = 0.01, animType = "fade")
      shinyjs::disable(id = "RGUI5_AB")
    } else {
      shinyjs::show(id = "PlBeOn", anim = F,time = 0.01, animType = "fade")
      shinyjs::enable(id = "RGUI5_AB")
    }
    
    if (nrow(TestResult) >= 4) {
      shinyjs::enable(id = "RGUI2_AB")
      shinyjs::enable(id = "RGUI4_AB")
      shinyjs::show(id = "EvoSp", anim = F,time = 0.01, animType = "fade")
      shinyjs::show(id = "EvoAsk", anim = F,time = 0.01, animType = "fade")
    } else {
      shinyjs::disable(id = "RGUI2_AB")
      shinyjs::disable(id = "RGUI4_AB")
      shinyjs::hide(id = "EvoSp", anim = F,time = 0.01, animType = "fade")
      shinyjs::hide(id = "EvoAsk", anim = F,time = 0.01, animType = "fade")
    }
  })
  
  output$ResGA <- renderPrint({
    result <- GARes()
    ## GA Inputs
    gainp <- do.call("rbind",result[1,10])
    
    ## Wind Inputs
    windinp <- do.call("rbind",result[1,11])
    colnames(windinp) <- c("Wind Speed", "Wind Direction")
    
    ## allvalues
    allval <- do.call("rbind",result[,1])
    colnames(allval) <- c("Maximum Park Fitness","Mean Park Fitness",
                          "Minimum Park Fitness", "Maximal Energy Output", 
                          "Mean Energy Output", "Minimum Energy Output", 
                          "Maximal Efficiency Rate", "Mean Efficiency Rate", "Minimum Efficiency Rate")
    rownames(allval) <- paste("Generation", 1:nrow(allval))
    
    ## N Indivs
    nindivre <- do.call("rbind",result[,6])
    colnames(nindivre) <- c("Number of Individuals in Fitness", 
                            "Number of Individuals in Selection", 
                            "Number of Individuals in Crossover",
                            "Number of Individuals in Mutation")
    rownames(nindivre) <- paste("Generation", 1:nrow(nindivre))
    
    ## Cross & Select & Mutat
    crosel <- do.call("rbind",result[,8])
    mutr <- do.call("rbind",result[,12])
    crosel <- cbind(crosel, mutr)
    colnames(crosel) <- c("Crossover Value", "Selection Divisor", "Mutation Rate")
    rownames(crosel) <- paste("Generation", 1:nrow(crosel))
    
    list(GA_Inputs = gainp, Wind_Inputs = windinp, 
         Result_Overview = allval, N_Individuals = nindivre, 
         GA_Parameters = crosel)
    
  })
  
  ResGAPrint <- reactive({
    if (v$valueButton == 1) {
      Polygon1 <- shape1()
    } else {
      Polygon1 <- shape()
    }
    req(!is.null(Polygon1))
    req(!is.null(GARes()))
    TestResult <- GARes()

    if (input$EneEff == "Energy") {
      plotEn = 1
      plwhat = "EnergyOverall"
    } else {
      plotEn = 2
      plwhat = "EfficAllDir"
    }

    if (is.na(proj4string(Polygon1))) {
      proj4string(Polygon1) <- CRS(input$SpRefSys)
      Polygon1 <- spTransform(Polygon1, CRSobj = CRS(ProjLaea))
    } else if (proj4string(Polygon1) != ProjLaea) {
      Polygon1 <- spTransform(Polygon1, CRSobj = CRS(ProjLaea))
    }


    a <- sapply(TestResult[,2], "[", plwhat)
    b <- data.frame(sapply(a, function(x) x[1]))
    order1 <- order(b, decreasing = T)
    result <- TestResult[,2][order1]
    rectid <- lapply(result, function(x) x[,8])
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)
    
    if (input$PlBeOn > ndif) {
      best <- ndif
      shinyjs::show(id = "prntwhateveritis1", animType = "fade", time = 0.1, anim = T)
      output$prntwhateveritis1 <- renderPrint({
        cat(paste("There are just ", best, "best unique layouts available."))
      })
    } else {
      shinyjs::hide(id = "prntwhateveritis1", animType = "fade", time = 0, anim = T)
      best <- input$PlBeOn
    }
    
    GRID <- GridFilter(shape = Polygon1, resol = input$fcrr_SHID * input$Roto_SHID,
                       prop = input$Propo_SHID, plotGrid = F)
    
    plotResult(result = TestResult, Polygon1 = Polygon1, 
               plotEn = plotEn, best = best,
               topographie = F, Grid = GRID[[2]])
  })
  
  output$ResGA1 <- renderPlot({
    ResGAPrint()
  })
  output$Evo <- renderPlot({
    req(!is.null(GARes()))
    TestResult <- GARes()
    shiny::validate(
      need(expr = nrow(TestResult) >= 4, 
           message = "Too few generations to plot values - Evo")
    )
    # if (input$EvoAsk == "Energy") {
    #   ask = 2
    # } else if (input$EvoAsk == "Efficiency") {
    #   ask = 1
    # }
    plotEvolution(result = TestResult, ask = F, spar = input$EvoSp)
  })
  output$OvGAP <- renderPlot({
    req(!is.null(GARes()))
    TestResult <- GARes()
    plotparkfitness(result = TestResult)
    
  })
  output$DifGAP <- renderPlot({
    req(!is.null(GARes()))
    TestResult <- GARes()
    shiny::validate(
      need(expr = (nrow(TestResult) >= 2),
           message = "Too few generations to plot values - DifGAP")
    )
    plotfitnessevolution(result = TestResult)
  })
  output$ClGAP <- renderPlot({
    req(!is.null(GARes()))
    TestResult <- GARes()
    
    shiny::validate(
      need(expr = (nrow(TestResult) >= 4),
           message = "Too few generations to plot values - ClGAP")
    )
    plotCloud(result = TestResult, TRUE)
  })
  
  output$downloadResG <- downloadHandler(
    filename = function() {
      paste0("ResultGA_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      DownLoadData <- ResGAPrint();
      write.table(x = DownLoadData, file = file, sep = ";", quote = FALSE)
    }
  )
  
  ## Corine and Cover Raster ##################
  options(shiny.maxRequestSize=80*1024^2)  
  CCLr <- reactive({
    req(input$CLC_TIF)
    req(!is.null(input$CLC_TIF))
    CCLra <- input$CLC_TIF
    ccl <- raster::raster(CCLra[1,4])
    
    if (v$valueButton == 1) {
      Polygon1 <- shape1()
    } else {
      Polygon1 <- shape()
    }
    req(!is.null(Polygon1))
    
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
  
  ## Plot Terrain Effects ##################
  output$TerrEff <- renderPlot({
    if (v$valueButton == 1) {
      Polygon1 <- shape1()
    } else {
      Polygon1 <- shape()
    }
    req(!is.null(Polygon1))
    req(!is.null(GARes()))
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
  # output$summary_TERR <- renderPrint({
  #   if (input$Topo_SHID==TRUE){
  #     req(input$CLC_TIF)
  #     req(input$CLC_Leg)
  #     if (is.null(input$CLC_TIF)){return()}
  #     shiny::validate(
  #       need(expr = grepl(pattern = ".tif",x = input$CLC_TIF), message = "The file must be given in .tif")
  #     )
  #     inFile <- input$CLC_Leg
  #     if (is.null(inFile)){return()}
  #     shiny::validate(
  #       need(expr = grepl(pattern = ".csv",x = inFile), message = "The file must be given in .csv")
  #     )
  #     
  #     CCLra <- input$CLC_TIF
  #     ccl <- CCLra[1,4]
  #     CCLRough <- inFile$datapath
  #   }
  #   if (input$Topo_SHID==TRUE){
  #     cat(c(
  #       "CORINE LAND COVER  INPUTS",
  #       "\nCORINE LAND COVER Raster: ", ccl,
  #       "\nCORINE LAND COVER Legend: ", CCLRough
  #     ))
  #   }
  # })
  output$summary_SHID <- renderPrint({
    cat(c("WIND INPUTS ",
          "\nReference Height: ",input$RefHeight_SHID,
          
          "\n\nTURBINE INPUTS",
          "\nTurbines: ", input$nTurb_SHID,
          "\nHub_Height: ", input$Height_SHID,
          "\nRotor_Radius: ", input$Roto_SHID,
          
          "\n\nPROJECTION INPUTS",
          "\nOriginal Projection: ", input$SpRefSys,
          "\nAlgorithm Projection: ", ProjLaea,
          
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
          "\nSurface_Roughness: ", input$Surface_SHID
          # "\nTopographic_Effects: ", input$Topo_SHID
    ))
    
    
    
  })
  
}
