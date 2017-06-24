
library(shiny)
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
library(shinythemes)


source(paste0(getwd(), "/server.R"))
source(paste0(getwd(), "/ui.R"))

ProjLaea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

shinyApp(ui, server)

# library(rsconnect)
# deployApp()