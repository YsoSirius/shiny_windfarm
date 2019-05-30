genAlgo           <- function(Polygon1, GridMethod, Rotor, n, fcrR, referenceHeight,
                              RotorHeight, SurfaceRoughness, Proportionality,
                              iteration, mutr, vdirspe, topograp, elitism, nelit,
                              selstate, crossPart1, trimForce, Projection,
                              sourceCCL, sourceCCLRoughness, weibull, weibullsrc,
                              Parallel, numCluster, verbose = FALSE, plotit = FALSE,
							  updateProgress1=NULL){

  ## set Graphic Params ###############
  if (plotit) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(par(oldpar))
    plot.new()
    graphics::par(ask = FALSE)
  }

  ## MISSING ARGUMENTS ###############
  if (missing(fcrR)) {
    fcrR <- 5
  }
  if (missing(topograp)) {
    topograp <- FALSE
  }
  if (missing(GridMethod)) {
    GridMethod <- "Rectangular"
  }
  if (missing(Parallel)) {
    Parallel <- FALSE
  }
  if (missing(numCluster)) {
    numCluster <- 1
  }
  if (missing(weibull)) {
    weibull <- FALSE
  }
  if (missing(selstate)) {
    selstate <- "FIX"
  }
  if (missing(crossPart1)) {
    crossPart1 <- "EQU"
  }
  if (missing(SurfaceRoughness)) {
    SurfaceRoughness <- 0.3
  }
  if (missing(Proportionality)) {
    Proportionality <- 1
  }
  if (missing(mutr)) {
    mutr <- 0.008
  }
  if (missing(elitism)) {
    elitism <- TRUE
    if (missing(nelit)) {
      nelit <- 7
    }
  }
  if (missing(trimForce)) {
    trimForce <- FALSE
  }
  if (missing(referenceHeight)) {
    referenceHeight <- RotorHeight
  }
  if (missing(iteration)) {
    iteration <- 20
  }
  if (missing(Projection)) {
    ProjLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  } else {
    ProjLAEA <- Projection
  }
  if (missing(vdirspe)) {
    stop("No Winddata is given.")
  }
  if (missing(n)) {
    stop("The varieble 'n' is not defined. Assign the number of turbines to 'n'.")
  }
  if (missing(Rotor)) {
    stop("The varieble 'Rotor' is not defined. Assign the rotor radius to 'Rotor'.")
  }
  if (missing(RotorHeight)) {
    stop("The varieble 'RotorHeight' is not defined. Assign the turbine heights to 'RotorHeight'.")
  }


  ## INIT VARIABLES 1 #################
  selstate <- toupper(selstate)
  crossPart1 <- toupper(crossPart1)

  ## Is the Polygon Spatial / SF / coordinates - It will transform to SpatialPolygon
  Polygon1 <- isSpatial(Polygon1, ProjLAEA)

  ## Grid size calculation
  resol2 <- fcrR * Rotor

  ## Max Amount of individuals in the Crossover-Method
  CrossUpLimit <- 300

  ## Start Parallel Cluster ###############
  ## Is Parallel processing activated? Check the max number of cores and set to max-1 if value exceeds.
  if (Parallel) {
    ## TODO - test on Linux
    max_cores <- as.integer(Sys.getenv("NUMBER_OF_PROCESSORS"))
    if (numCluster > max_cores) {
      numCluster <- max_cores - 1
    }
    type_cluster <- "PSOCK"
    cl <- parallel::makeCluster(numCluster, type = type_cluster)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  ## WEIBULL ###############
  ## Is Weibull activated? If no source is given, take values from package
  if (weibull) {
    if (verbose) {
      cat("\nWeibull Distribution is used.")
    }
    if (missing(weibullsrc)) {
      if (verbose) {
        cat("\nWeibull Informations from package will be used.\n")
      }
      path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
      k_weibull <- ""
      a_weibull <- ""
      # load(file = paste0(path, "k_weibull.rda"))
      # load(file = paste0(path, "a_weibull.rda"))
      # weibullsrc = list(k_param, a_param)
      k_weibull <- readRDS(file = paste0(path, "k_weibull.RDS"))
      a_weibull <- readRDS(file = paste0(path, "a_weibull.RDS"))
      ## Project Shapefile to raster proj, Crop/Mask and project raster back
      shape_project <- sp::spTransform(Polygon1,
                                       CRSobj = sp::proj4string(a_weibull))
      k_par_crop <- raster::crop(x = k_weibull,
                                 y = raster::extent(shape_project))
      a_par_crop <- raster::crop(x = a_weibull,
                                 y = raster::extent(shape_project))
      weibl_k <- raster::mask(x = k_par_crop, mask = shape_project)
      weibl_a <- raster::mask(x = a_par_crop, mask = shape_project)
      estim_speed_raster <- weibl_a * (gamma(1 + (1 / weibl_k)))
      estim_speed_raster <- raster::projectRaster(estim_speed_raster,
                                              crs = sp::proj4string(Polygon1))
    } else {
      if (verbose) {
        cat("\nWeibull Informations are given.\n")
      }
      weibullsrc <- weibullsrc
      ## Project Shapefile to raster, Crop/Mask and project raster back
      shape_project <- sp::spTransform(Polygon1,
                                    CRSobj = sp::proj4string(weibullsrc[[1]]))
      k_par_crop <- raster::crop(x = weibullsrc[[1]],
                                 y = raster::extent(shape_project))
      a_par_crop <- raster::crop(x = weibullsrc[[2]],
                                 y = raster::extent(shape_project))
      weibl_k <- raster::mask(x = k_par_crop, mask = shape_project)
      weibl_a <- raster::mask(x = a_par_crop, mask = shape_project)
      estim_speed_raster <- weibl_a * (gamma(1 + (1 / weibl_k)))
      estim_speed_raster <- raster::projectRaster(estim_speed_raster,
                                                  crs = proj4string(Polygon1))
    }
  } else {
    estim_speed_raster <- FALSE
  }

  ## CHECK INPUTS ###############
  ## Check if Input Data is correct and prints it out.
  if  (crossPart1 != "EQU" & crossPart1 != "RAN") {
    crossPart1 <- readinteger()
  }
  if  (selstate != "FIX" & selstate != "VAR") {
    selstate <- readintegerSel()
  }
  inputData <- list(
    Input_Data = rbind("Rotorradius" = Rotor,
                       "Number of turbines" = n,
                       "Grid Shape Factor" = fcrR,
                       "Iterations" = iteration,
                       "Mutation Rate" = mutr,
                       "Percentage of Polygon" = Proportionality,
                       "Topographie" = topograp,
                       "Elitarism" = elitism,
                       "Selection Method" = selstate,
                       "Trim Force Method Used" = trimForce,
                       "Crossover Method Used" = crossPart1,
                       "Reference Height" = referenceHeight,
                       "Rotor Height" = RotorHeight,
                       "Resolution" = resol2,
                       "Parallel Processing" = Parallel,
                       "Number Clusters" = numCluster,
                       "Active Weibull" = weibull,
                       "Grid Method" = GridMethod,
                       "Projection" = ProjLAEA))

  inputWind <- list(Windspeed_Data = vdirspe)
  if (verbose) {
    print(inputData)
    print(inputWind)
  }

  ## Winddata Formatting #######################
  winddata <- windata_format(vdirspe)
  
  #######################
  ## Project Polygon ###############
  if (as.character(raster::crs(Polygon1)) != ProjLAEA) {
    Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjLAEA)
  }

  ## GRIDFILTER ###############
  ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
  GridMethod <- toupper(GridMethod)
  ## Decide if the space division should be rectangular or in hexagons.
  if (GridMethod != "HEXAGON" & GridMethod != "H") {
    # Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
    Grid1 <- GridFilter(shape = Polygon1, resol = resol2, prop = Proportionality)
    Grid <- Grid1[[1]]
    grid_filtered <- Grid1[[2]]
  } else {
    # Calculate a Grid with hexagonal grid cells
    Grid1 <- HexaTex(Polygon1, resol2 / 2)
    Grid <- Grid1[[1]]
    sp::proj4string(Grid1[[2]]) <- sp::proj4string(Polygon1)
    grid_filtered <- Grid1[[2]]
  }

  n_gridcells <- nrow(Grid)


  ## INIT VARIABLES 2 ###############
  ## Determine the amount of initial individuals and create initial population.
  nStart <- (n_gridcells * n) / iteration
  if (nStart < 100) {
    nStart <- 100
  }
  if (nStart > 300) {
    nStart <- 300
  }
  nStart <- ceiling(nStart)
  startsel <- StartGA(Grid, n, nStart)
  ## Initialize all needed variables as list.
  maxParkwirkungsg <- 0
  allparkcoeff <- vector("list", iteration)
  bestPaEn <- vector("list", iteration)
  bestPaEf <- vector("list", iteration)
  fuzzycontr <- vector("list", iteration)
  fitnessValues <- vector("list", iteration)
  nindiv <- vector("list", iteration)
  clouddata <- vector("list", iteration)
  selcross <- vector("list", iteration)
  beorwor <- vector("list", iteration)
  mut_rate <- vector("list", iteration)
  allCoords <- vector("list", iteration)

  ## TERRAIN EFFECT MODEL ###############
  ## Checks if terrain effect model is activated, and makes necessary caluclations.
  if (!topograp) {
    if (verbose) {
      cat("Topography and orography are not taken into account.")
    }
    srtm_crop <- ""
    cclRaster <- ""
  } else {
    if (verbose) {
      cat("Topography and orography are taken into account.")
    }

    if (plotit) {
      par(mfrow = c(3, 1))
    }

    if (missing(sourceCCL)) {
      message("No land cover raster ('sourceCCL') was given. It will be downloaded from ",
              "the EEA-website.")
      # readline(prompt = "Press [enter] to continue or Escpae to exit.")
      if (!file.exists("g100_06.tif")) {
        ## download an zip CCL-tif
        ccl_raster_url <-
          "https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-3/clc-2006-100m/g100_06.zip/at_download/file"
        temp <- tempfile()
        download.file(ccl_raster_url, temp, method = "libcurl", mode = "wb")
        unzip(temp, "g100_06.tif")
        unlink(temp)
      }
      ccl <- raster::raster("g100_06.tif")
    } else {
      ccl <- raster::raster(sourceCCL)
    }

    ## SRTM Daten
    Polygon1 <-  sp::spTransform(Polygon1,
                                 CRSobj = raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    extpol <- round(Polygon1@bbox,0)[, 2]
    srtm <- raster::getData("SRTM", lon = extpol[1], lat = extpol[2])
    if (missing(srtm)) {
      stop("\nCould not download SRTM for the given Polygon.",
           "Check the Projection of the Polygon.\n", call. = FALSE)
    }
    srtm_crop <- raster::crop(srtm, Polygon1)
    srtm_crop <- raster::mask(srtm_crop, Polygon1)

    Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLAEA))
    srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLAEA))
    if (plotit) {
      plot(srtm_crop, main = "Elevation from SRTM")
      plot(Polygon1, add = TRUE)
      plot(grid_filtered, add = TRUE)
    }

    srtm_crop <- list(
      strm_crop = srtm_crop,
      orogr1 = raster::calc(srtm_crop, function(x) {
        x / (raster::cellStats(srtm_crop, mean, na.rm = TRUE))
      }),
      raster::terrain(srtm_crop, "roughness")
    )

    # Include Corine Land Cover Raster to get an estimation of Surface Roughness
    if (missing(sourceCCLRoughness)) {
      path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
      sourceCCLRoughness <- paste0(path, "clc_legend.csv")
    } else {
      if (verbose) {
        print("You are using your own Corine Land Cover legend.")
        # readLines(prompt = "\nPress <ENTER> if you want to continue")
      }
      sourceCCLRoughness <- sourceCCLRoughness
    }

    cclPoly <- raster::crop(ccl, Polygon1)
    cclPoly1 <- raster::mask(cclPoly, Polygon1)
    rauhigkeitz <- utils::read.csv(sourceCCLRoughness,
                                   header = TRUE, sep = ";")
    cclRaster <- raster::reclassify(cclPoly1,
                                    matrix(c(rauhigkeitz$GRID_CODE,
                                             rauhigkeitz$Rauhigkeit_z),
                                           ncol = 2))
    if (plotit) {
      plot(cclRaster, main = "Surface Roughness from Corine Land Cover")
    }
  }


  ## GENETIC ALGORITHM #################
  if (verbose) {cat("\nStart Genetic Algorithm ...")}
  rbPal <- grDevices::colorRampPalette(c("red", "green"))
  i <- 1
  while (i <= iteration) {
    if (!verbose) {
      cat(".")
    }
    ## FITNESS (+getRectV) ###############
    if (i == 1) {
      fit <- fitness(selection = startsel, referenceHeight = referenceHeight,
                     RotorHeight = RotorHeight,
                     SurfaceRoughness = SurfaceRoughness,
                     Polygon = Polygon1, resol1 = resol2, rot = Rotor,
                     dirspeed = winddata, srtm_crop = srtm_crop,
                     topograp = topograp, cclRaster = cclRaster,
                     weibull = estim_speed_raster,
                     Parallel = Parallel, numCluster = numCluster)
    } else {
      getRectV <- getRects(mut1, Grid)
      fit <- fitness(selection = getRectV, referenceHeight = referenceHeight,
                     RotorHeight = RotorHeight,
                     SurfaceRoughness = SurfaceRoughness,
                     Polygon = Polygon1, resol1 = resol2, rot = Rotor,
                     dirspeed = winddata, srtm_crop = srtm_crop,
                     topograp = topograp, cclRaster = cclRaster,
                     weibull = estim_speed_raster,
                     Parallel = Parallel, numCluster = numCluster)
    }

    ## Fitness Result Processing ###############
    allparks <- do.call("rbind", fit)
    allparksUni <- subset.matrix(allparks,
                                 subset = !duplicated(allparks[, "Run"]))

    allCoords[[i]] <- allparks
    maxparkfitness <-  round(max(allparksUni[, "Parkfitness"]), 4)
    meanparkfitness <- round(mean(allparksUni[, "Parkfitness"]), 3)
    minparkfitness <- round(min(allparksUni[, "Parkfitness"]), 3)
    MaxEnergyRedu <-  round(max(allparksUni[, "EnergyOverall"]), 2)
    MeanEnergyRedu <- round(mean(allparksUni[, "EnergyOverall"]), 2)
    MinEnergyRedu <- round(min(allparksUni[, "EnergyOverall"]), 2)
    maxParkwirkungsg <- round(max(allparksUni[, "EfficAllDir"]), 2)
    meanParkwirkungsg <- round(mean(allparksUni[, "EfficAllDir"]), 2)
    minParkwirkungsg <- round(min(allparksUni[, "EfficAllDir"]), 2)
    allparkcoeff[[i]] <- cbind(
      maxparkfitness, meanparkfitness, minparkfitness,
      MaxEnergyRedu, MeanEnergyRedu, MinEnergyRedu,
      maxParkwirkungsg, meanParkwirkungsg, minParkwirkungsg)

    clouddata[[i]] <- subset.matrix(allparksUni,
                                    select = c("EfficAllDir",
                                               "EnergyOverall",
                                               "Parkfitness"))

    if (verbose) {
      cat(c("\n\n", i, ": Round with coefficients ", allparkcoeff[[i]], "\n"))
    }

    ## Highest Energy Output
    xd <- max(allparks[, "EnergyOverall"])
    ind <- allparks[, "EnergyOverall"] == xd
    bestPaEn[[i]] <- allparks[ind, ][1:n, ]
    ## Highest Efficiency
    xd1 <- max(allparks[, "EfficAllDir"])
    ind1 <- allparks[, "EfficAllDir"] == xd1
    bestPaEf[[i]] <- allparks[ind1, ][1:n, ]

    # Print out most relevant information on Generation i
    afvs <- allparks[allparks[, "EnergyOverall"] == max(
      allparks[, "EnergyOverall"]), ]
    if (verbose) {
      cat(paste("How many individuals exist: ",  length(fit) ), "\n")
      cat(paste("How many parks are in local Optimum: ",
                (length(afvs[, 1]) / n) ), "\n")
    }
    nindivfit <- length(fit)

    if (plotit) {
      lebre <- length(unique(bestPaEn[[i]][, "AbschGesamt"]))
      if (lebre < 2) {
        Col <- "green"
      } else {
        Col <- rbPal(lebre)[as.numeric(cut(-bestPaEn[[i]][, "AbschGesamt"],
                                           breaks = lebre))]
      }
      lebre2 <- length(unique(bestPaEf[[i]][, "AbschGesamt"]))
      if (lebre2 < 2) {
        Col1 <- "green"
      } else {
        Col1 <- rbPal(lebre2)[as.numeric(cut(-bestPaEf[[i]][, "AbschGesamt"],
                                             breaks = lebre2))]
      }
    }

    x <- round(bestPaEn[[i]][, "EnergyOverall"][[1]], 2)
    y <- round(bestPaEn[[i]][, "EfficAllDir"][[1]], 2)
    e <- bestPaEn[[i]][, "EfficAllDir"]
    x1 <- round(bestPaEf[[i]][, "EnergyOverall"][[1]], 2)
    y1 <- round(bestPaEf[[i]][, "EfficAllDir"][[1]], 2)
    e1 <- bestPaEf[[i]][, "EfficAllDir"]

    allparksNewplot <- subset.matrix(allparks,
                                     select = c("Rect_ID",
                                                "AbschGesamt",
                                                "Parkfitness"))

    allparksNewplot <- aggregate(allparksNewplot,
                                 list(allparksNewplot[, "Rect_ID"]), mean)
    allparksNewplot <- allparksNewplot[, -1]

    if (any(allparksNewplot[, "Rect_ID"] %in% Grid[, "ID"] == FALSE)) {
      stop("Index of Grid not correct. Bigger than maximum Grid? Fix BUG")
    }
    ##################

    if (plotit) {
      graphics::par(mfrow = c(1, 2))
      plot(Polygon1,
           main = paste(i, "Round \n Best Energy Output: ", x,
                        "W/h \n Efficiency: ", y, "%"),
           sub = paste("\n Number of turbines: ", length(e)))
      plot(grid_filtered, add = TRUE)
      graphics::points(bestPaEn[[i]][, "X"], bestPaEn[[i]][, "Y"],
                       col = Col, pch = 20, cex = 1.5)
      plot(Polygon1, main = paste(i, "Round \n Best Efficiency Output: ",
                                  x1, "W/h \n Efficiency: ", y1, "%"),
           sub = paste("\n Number of turbines: ", length(e1)))
      plot(grid_filtered, add = TRUE)
      graphics::points(bestPaEf[[i]][, "X"], bestPaEf[[i]][, "Y"],
                       col = Col1, pch = 20, cex = 1.5)
    }

    ## Fuzzy Control ###############
    if (i > 20) {
      besPE <- do.call("rbind", lapply(bestPaEn[1:i], function(x) {
        max(x[, "EnergyOverall"])
      }))
      maxBisher <- max(besPE)
      WhichMaxBs <- which(besPE == max(besPE))

      if (length(WhichMaxBs) >= 2) {
        BestForNo <- bestPaEn[sample(WhichMaxBs, 2)]
        BestForNo[[1]][, "Run"] <- length(fit) + 1
        BestForNo[[2]][, "Run"] <- length(fit) + 2
      } else {
        BestForNo <- bestPaEn[WhichMaxBs]
        BestForNo <- append(BestForNo, BestForNo)
        BestForNo[[1]][, "Run"] <- length(fit) + 1
        BestForNo[[2]][, "Run"] <- length(fit) + 2
      }

      last7 <- besPE[i:(i - 5)]
      if (!any(last7 == maxBisher)) {
        if (verbose) {
          cat(paste("Park with highest Fitness level to date ",
                    "is replaced in the list.", "\n\n"))
        }
        fit <- append(fit, BestForNo)
      }
    }
    if (i == 1) {
      ## TODO I do have such a matrix already with that info or??
      t0 <- subset.matrix(allparks, !duplicated(allparks[, "Run"]))
      t0 <- t0[, "Parkfitness"]
      fitnessValues[[i]] <- t0
      rangeFitnessVt0 <- range(t0)
      maxt0 <- max(t0)
      meant0 <- mean(t0)
      allcoef0 <- c(rangeFitnessVt0, meant0)
      fuzzycontr[[i]] <- rbind(allcoef0)
      colnames(fuzzycontr[[i]]) <- c("Min", "Max", "Mean")
      teil <- 2
      if (selstate == "VAR") {
        teil <- 1.35
      }
      u <- 1.1
      beorwor[[i]] <- cbind(0, 0)
    }
    if (i >= 2 && i <= iteration) {
      t0 <- subset.matrix(allparks, !duplicated(allparks[, "Run"]))
      t0 <- t0[, "Parkfitness"]
      fitnessValues[[i]] <- t0
      rangeFitnessVt0 <- range(t0)
      maxt0 <- max(t0)
      meant0 <- mean(t0)
      mint0 <- min(t0)
      t1 <- fitnessValues[[i - 1]]
      rangeFitnessVt1 <- range(t1)
      maxt1 <- max(t1)
      meant1 <- mean(t1)
      mint1 <- min(t1)
      maxDif <- maxt0 - maxt1
      meanDif <- meant0 - meant1
      minDif <- mint0 - mint1
      WeightDif <- c(0.80, 0.2, 0.0)
      maxunt <- (maxDif * WeightDif[1]) +
        (meanDif * WeightDif[2]) + (minDif * WeightDif[3])
      allcoef1 <- c(rangeFitnessVt0, meant0)
      allcoef2 <- c(rangeFitnessVt1, meant1)
      fuzzycontr[[i]] <- rbind(allcoef1, allcoef2)
      colnames(fuzzycontr[[i]]) <- c("Min", "Max", "Mean")

      if (maxunt < 0) {
        pri <- "deteriorated"
        teil <- teil - 0.02
        u <- u - 0.06
      } else if (maxunt == 0) {
        pri <- "not changed"
        teil <- teil; u <- u
      } else {
        pri <- "improved"
        teil <- teil + 0.017
        u <- u + 0.03
      }

      if (teil > 5) {
        teil <- 5
        u <- u + 0.09
        if (verbose) {
          cat("Min 20% Selected")
          cat(paste("CPR is increased! CPR:", u, "SP: ", teil, "\n"))
        }
      }
      if (trunc(u) < 0) {
        u <- 0.5
        teil <- teil - 0.4
        if (verbose) {
          cat(paste("Min 1 CrossPoints. Selection decreased. CPR:",
                    u, "SP: ", teil, "\n"))
        }
      }
      if (u >= 4) {
        u <- 4
        teil <- 4
        if (verbose) {
          cat(paste("Max 5 CrossPoints. Select fittest 25%. SP: ", teil, "\n"))
        }
      }
      if (teil <= 4 / 3) {
        teil <- 4 / 3
        if (verbose) {
          cat(paste("Max 75% selected. SP: ", teil, "\n"))
        }
      }
      if (length(fit) <= 20) {
        teil <- 1
        u <- u + 0.07
        if (verbose) {
          cat(paste("Less than 20 individuals. Select all and increase ",
                    "Crossover-point rate. CPR: ", u, "SP: ", teil, "\n"))
        }
      }
      if (length(fit) <= 10) {
        teil <- 1
        u <- u + 0.4
        if (verbose) {
          cat(paste("Less than 10 individuals. Select all and increase ",
                    "Crossover-point rate. CPR: ", u, "SP: ", teil, "\n"))
        }
      }
      if (teil > 5) {
        teil <- 5
        if (verbose) {
          cat(paste("Teil is bigger than 5. Set to max 5. SP:", teil, "\n"))
        }
      }

      u <- round(u, 2)
      teil <- round(teil, 3)

      if (verbose) {
        cat(paste("Fitness of this population (", i,
                  "), compared to the prior,", pri,
                  "by", round(maxunt, 2), "W/h \n"))
      }
      meanunt <- meant0 - meant1
      beorwor[[i]] <- cbind(maxunt, meanunt)
    }

    ## SELECTION #################
    if (selstate == "FIX") {
      if (teil == 1) {
        teil <- 1
      } else {
        teil <- 2
      }
    }
    if (crossPart1 == "EQU") {
      u <- round(u, 2)
    }

    ## How many are selected and how much crossover points are used?
    selcross[[i]] <- cbind(cross = trunc(u + 1), teil)
    selec6best <- selection1(fit = fit, Grid = Grid, teil = teil,
                             elitism = elitism, nelit = nelit,
                             selstate = selstate, verbose = verbose)

    selec6best_bin <- selec6best[[1]]
    if (verbose) {
      cat(paste("Selection  -  Amount of Individuals: ",
                length(selec6best_bin[1, -1]), "\n"))
    }
    Trus1 <- colSums(selec6best_bin)[-1] == n
    if (any(Trus1 == FALSE)) {
      stop("Number of turbines is not as required. Trus1. Fix BUG")
    }
    nindivsel <- length(selec6best_bin[1, -1])

    ## CROSSOVER #################
    ## u determines the amount of crossover points,
    ## crossPart determines the method used (Equal/Random),
    ## uplimit is the maximum allowed permutations
    crossOut <- crossover1(se6 = selec6best, u = u, uplimit = CrossUpLimit,
                           crossPart = crossPart1,
                           verbose = verbose, seed = NULL)
    if (verbose) {
      cat(paste("Crossover  -  Amount of Individuals: ",
                length(crossOut[1, ])))
    }
    nindivcros <- length(crossOut[1, ])

    ## MUTATION #################
    ## Variable Mutation Rate is activated if more than 2 individuals
    ## represent the current best solution.
    loOp <- (length(afvs[, 1]) / n)
    if (loOp > 2) {
      mutrn <- round(runif(1, 0.03, 0.1), 2)
      t1 <- (loOp * 1.25) / 42
      mutrn <- mutrn * (1 + t1)
      mutrn <- round(mutrn + ((i) / (20 * iteration)), 5)
      mut <- mutation(a = crossOut, p = mutrn, seed = NULL)
      mut_rat <- mutrn
      if (verbose) {
        cat(paste("\nVariable Mutation Rate is", mutrn, "\n"))
      }
    } else {
      mut <- mutation(a = crossOut, p = mutr, seed = NULL)
      mut_rat <- mutr
    }
    mut_rate[[i]] <- mut_rat
    if (verbose) {
      cat(paste("\nMutation   -  Amount of Individuals: ", length(mut[1, ])))
    }
    nindivmut <- length(mut[1, ])

    ## TRIMTON #################
    ## After Crossover and Mutation, the amount of turbines in a windpark
    ## change and have to be corrected to the required amount of turbines.
    mut1 <- trimton(mut = mut, nturb = n, allparks = allparks,
                    nGrids = n_gridcells, trimForce = trimForce,
                    seed = NULL)

    if (verbose) {
      cat(paste("\nTrimToN    -  Amount of Individuals: ",
                length(mut1[1, ])))
    }
    Trus3 <- colSums(mut1) == n
    if (any(Trus3 == FALSE)) {
      stop("Number of turbines is not as required. Trus3. Fix Bug.")
    }
    
	if (is.function(updateProgress1)) {
      detail = paste("for iteration: ",i)
      updateProgress1(detail)
    }
	
    nindiv[[i]] <- cbind(nindivfit, nindivsel, nindivcros, nindivmut)
    if (maxParkwirkungsg == 100) {
      i <- iteration + 1
    } else {
      i <- i + 1
    }
  }

  ## Remove Parallel Cluster ###############
  if (Parallel) {
  try(rm(cl), silent = TRUE)
  }

  ## Reduce list, if algorithm didnt run all iterations. (Found Optimum) #################
  mut_rate <- mut_rate[lapply(mut_rate, length) != 0]
  beorwor <- beorwor[lapply(beorwor, length) != 0]
  selcross <- selcross[lapply(selcross, length) != 0]
  clouddata <- clouddata[lapply(clouddata, length) != 0]
  allparkcoeff <- allparkcoeff[lapply(allparkcoeff, length) != 0]
  bestPaEn <- bestPaEn[lapply(bestPaEn, length) != 0]
  bestPaEf <- bestPaEf[lapply(bestPaEf, length) != 0]
  fuzzycontr <- fuzzycontr[lapply(fuzzycontr, length) != 0]
  fitnessValues <- fitnessValues[lapply(fitnessValues, length) != 0]
  nindiv <- nindiv[lapply(nindiv, length) != 0]
  allCoords <- allCoords[lapply(allCoords, length) != 0]

  ## Bind the results together and Output them. #################
  alldata <- cbind(allparkcoeff, bestPaEn, bestPaEf,
                   fuzzycontr, fitnessValues, nindiv,
                   clouddata, selcross, beorwor,
                   inputData, inputWind, mut_rate, allCoords)

  return(alldata)
}

#' @title Transform to SpatialPolygons
#' @name isSpatial
#' @description Helper Function, which transforms SimpleFeatures or
#' coordinates in matrix/data.frame/data.table into a SpatialPolygon
#'
#' @export
#'
#' @importFrom sp proj4string Polygon Polygons SpatialPolygons
#' @importFrom methods as
#'
#' @param shape An area as SpatialPolygon, SimpleFeature Polygon
#' or coordinates as matrix/data.frame
#' @param proj Which Projection should be assigned to matrix / 
#' data.frame coordinates
#'
#' @return A SpatialPolygons object
#' 
#' @details If the columns are named, it will look for common abbreviation
#' to match x/y or long/lat columns. If the columns are not named, the first
#' 2 numeric columns are taken.
#'
#' @examples \donttest{
#' df <- rbind(c(4498482, 2668272), c(4498482, 2669343),
#'             c(4499991, 2669343), c(4499991, 2668272))
#' isSpatial(df)
#' 
#' Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2668272)))
#' Polygon1 <- Polygons(list(Polygon1), 1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' df_fort <- ggplot2::fortify(Polygon1)
#' isSpatial(df_fort, Projection)
#'}
#' @author Sebastian Gatscha
isSpatial <- function(shape, proj) {
  # shape = xy_matrix
  if (class(shape)[1] == "sf") {
    shape <- as(shape, "Spatial")
    ## This is needed for GridFilter. Attribute names must have same length
    shape$names <- "layer"
  } else if (class(shape)[1] == "data.frame" |
             class(shape)[1] == "matrix") {
    ## If coordinate names are found, take those columns, 
    ## otherwise take the first 2
    if (length(colnames(shape))) {
      accep_cols_x <- c("L*N", "X")
      accep_cols_y <- c("L*T", "Y", "BREITE")
      sum_col_match <- sum(sapply(c(accep_cols_x, accep_cols_y), grepl,
                                 toupper(colnames(shape)) ))
      if (sum_col_match >= 2) {
        x_col_match <- which(sapply(
          lapply(accep_cols_x, grepl, toupper(colnames(shape))),
          any))
        y_col_match <- which(sapply(
          lapply(accep_cols_y, grepl, toupper(colnames(shape))),
          any))

        x_col_index <- which(grepl(accep_cols_x[x_col_match],
                                   toupper(colnames(shape))))
        y_col_index <- which(grepl(accep_cols_y[y_col_match],
                                   toupper(colnames(shape))))

        pltm <- shape[, c(x_col_index[1], y_col_index[1])]
      } else {
        col_numeric <- which(sapply(shape[1, ], is.numeric))
        pltm <- shape[, col_numeric]
      }
    } else {
      col_numeric <- which(sapply(shape[1, ], is.numeric))
      pltm <- shape[, col_numeric]
    }

    pltm <- Polygon(pltm)
    pltm <- Polygons(list(pltm), 1)
    shape <- SpatialPolygons(list(pltm))
    if (!missing(proj)) {
      sp::proj4string(shape) <- proj
    }
  }
  return(shape)
}


windata_format <- function(df) {
  wind_df <- data.frame(df)
  if (!all(colnames(wind_df) %in% c("ws", "wd"))) {
      # Assume that we've been given a wind_df frame. 
      # Lets find the correct columns
      if (length(colnames(wind_df)) && 
          all(!colnames(wind_df) %in% c("X1", "X2", "X3")) ) {
        accep_speed <- c("SPEED", "GESCH", "V", "WS")
        accep_direc <- c("DIR", "RICHT", "WD")
        accep_proba <- c("PRO", "WAHR")
        sum_col_match <- sum(sapply(c(accep_speed, accep_direc, accep_proba),
                                    grepl, toupper(colnames(wind_df)) ))
        if (sum_col_match >= 2) {
          speed_match <- which(sapply(
            lapply(accep_speed, grepl, toupper(colnames(wind_df))),
            any))
          direc_match <- which(sapply(
            lapply(accep_direc, grepl, toupper(colnames(wind_df))),
            any))
          probab_match <- which(sapply(
            lapply(accep_proba, grepl, toupper(colnames(wind_df))),
            any))
          speed_index <- which(grepl(accep_speed[speed_match],
                                     toupper(colnames(wind_df))))
          direc_index <- which(grepl(accep_direc[direc_match],
                                     toupper(colnames(wind_df))))
          if (length(probab_match) != 0) {
            probab_index <- which(grepl(accep_proba[probab_match],
                                        toupper(colnames(wind_df))))
            wind_df[, c(speed_index[1], direc_index[1], probab_index[1])]
            colnames(wind_df) <- c("ws", "wd", "probab")            
          } else {
            wind_df[, c(speed_index[1], direc_index[1])]
            colnames(wind_df) <- c("ws", "wd")            
          }
        } else {
          col_numeric <- which(sapply(wind_df[1, ], is.numeric))
          wind_df <- wind_df[, col_numeric]
          colnames(wind_df) <- c("ws", "wd")
        }
      } else {
        col_numeric <- which(sapply(wind_df[1, ], is.numeric))
        wind_df <- wind_df[, col_numeric]
        if (length(colnames(wind_df)) == 2) {
          colnames(wind_df) <- c("ws", "wd")        
        } else {
          colnames(wind_df) <- c("ws", "wd", "probab")
        }
      }
  }
  wind_df$wd <- round(wind_df$wd, 0)
  wind_df$wd <-  round(wind_df$wd / 100, 1) * 100
  ## If no probabilites are given, assign uniform distributed ones.
  if (anyNA(colnames(wind_df))) {
    which(is.na(colnames(wind_df)))
    colnames(wind_df)[3] <- "probab"    
  }
  if (any(names(wind_df) == "probab") == FALSE) {
    wind_df$probab <- 100 / nrow(wind_df)
  }
  ## Checks if all the sum of possibility is  100
  if (sum(wind_df$probab) != 100) {
    wind_df$probab <- wind_df$probab * (100 / sum(wind_df$probab))
  }
  ## Checks if duplicated wind directions are at hand
  if  (any(duplicated(wind_df$wd))) {
    for (i in 1:length(wind_df[duplicated(wind_df$wd) == FALSE, 1]) ) {
      ## Get duplicated direction rows
      temp <- wind_df[wind_df$wd ==  wind_df[duplicated(
        wind_df$wd) == FALSE, ][i, "wd"], ]
      ## Sum up speed and probability 
      temp$ws <- sum(temp$ws * (temp$probab / sum(temp$probab)))
      temp$probab <- sum(temp$probab * (temp$probab / sum(temp$probab)))
      ## Assign new/uniwue windspeed and probablity per direction
      wind_df[wind_df$wd ==  wind_df[duplicated(
        wind_df$wd) == FALSE, ][i, "wd"], ]$ws <- round(temp$ws, 2)[1]
      wind_df[wind_df$wd ==  wind_df[duplicated(
        wind_df$wd) == FALSE, ][i, "wd"], ]$probab <- round(temp$probab, 2)[1]
    }
  }
  ## Delete duplicated direction rows
  wind_df <- wind_df[!duplicated(wind_df$wd) == TRUE, ]
  ## Order by direction
  wind_df <- wind_df[with(wind_df, order(wd)), ]
  ## Sum up probabilites to 100% again
  if (sum(wind_df$probab) != 100) {
    wind_df$probab <- wind_df$probab * (100 / sum(wind_df$probab))
  }
  probabDir <- wind_df$probab
  if (any(wind_df$wd > 360)) {
    wind_df[wind_df$wd > 360, "wd"] <- wind_df[wind_df$wd > 360, "wd"] - 360 
  }
  wind_df <- as.matrix(wind_df)
  winddata <- list(wind_df, probabDir)
  return(winddata)
}
