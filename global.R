
suppressMessages(library(shiny, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(windfarmGA, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(shinyjs, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(shinythemes, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(dplyr, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(DT, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(ggplot2, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(leaflet, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(rgdal, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(sp, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(raster, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(rgeos, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(maptools, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(RColorBrewer, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(data.table, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(calibrate, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(gtools, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(shinyBS, warn.conflicts = FALSE, quietly = TRUE))


source("server.R")
source( "ui.R")

`%then%` <- shiny:::`%OR%`
ProjLaea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

shinyApp(ui, server)

# library(rsconnect)
# deployApp()


