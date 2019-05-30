
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),
  titlePanel("Optimize your Windpark", windowTitle = "windfarmGA"),
  
  includeCSS("www/slider.css"),
  tags$style(HTML("::selection {background: tomato}")),
  
  navbarPage(title="windfarmGA",
             tabPanel("Run an Optimization",
                      sidebarLayout(
                        fluid = TRUE,
                        div(id="ALLINPUTS",
                            sidebarPanel(
                              ########## SHAPEFILE ########## 
                              br(),
                              tags$div(id="POCSBO0",
                                       h4("DEMO FILES:"),
                                       p("Click the buttom bellow to include a Shapefile and some random wind data. 
                                         Run an Optimization, Test a Layout and explore this App."),
                                       inlineCSS(list(.red = "background: red;")),
                                       
                                       div(id="runExCol", style="margin:20px;",
                                           actionButton(inputId = "runExample", label = "Include Demo Files", width = "100%")
                                       )  
                                       ),
                              bsTooltip("runExample", title = "Click me and some random data will be added, so you can explore this app.",
                                        placement ="right", options = list(container = "body"), trigger="hover"),
                              br(),
                              
                              tags$div(id="POCSBO",
                                       h4("POLYGON INPUT:"),
                                       fileInput(inputId="Shape_SHID", label = "Upload all files of a Polygon representing the area, 
                                                 where the wind farm should be optimized.",
                                                 multiple = TRUE,accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"))
                              ),
                              ########## WIND DATA INPUTS ##########
                              br(),
                              tags$div(id="POCSBO1",
                                       h4("WIND DATA INPUTS:"),
                                       fileInput(inputId="Wind_SHID", label = "Upload a Data Table of the wind speeds",multiple = FALSE,
                                                 accept = c(".csv")),
                                       sliderInput(inputId="RefHeight_SHID", label = "What is the reference height of the wind data?",
                                                   value=10,min=1, max=200,step = 0.5)
                              ),
                              shinyBS::bsTooltip(id = "RefHeight_SHID", title = "Where are youu",
                                                 placement ="bottom", trigger = "hover"),
                              
                              ########## TURBINE INPUTS ##########
                              br(),
                              tags$div(id="POCSBO2",
                                       h4("TURBINE INPUTS:"),
                                       sliderInput(inputId="nTurb_SHID", label ="Select the number of desired turbines",
                                                   value=10, min=1,max = 20,step = 1),
                                       sliderInput(inputId="Height_SHID", label = "What are the hub heights of the turbines?",
                                                   value=100,min=10, max=200, step=1),
                                       sliderInput(inputId="Roto_SHID", label = "Determine the radius of the turbines",
                                                   value=20,min=2, max=100, step=1)
                              ),
                              ########## GENETIC ALGORITHM METHOD INPUTS ##########
                              br(),
                              tags$div(id="POCSBO3",       
                                       h4("GENETIC ALGORITHM INPUTS:"),
                                       sliderInput(inputId="Ite_SHID", label = "Choose the number of iterations",
                                                   value=10,min=1, max=30, step=1),
                                       selectInput(inputId="Selec_SHID", label = "Pick a Selection Method:",
                                                   choices = c("FIX","VAR"),selected = "FIX",multiple = FALSE),
                                       selectInput(inputId="Cross_SHID", label = "Pick a Crossover Method:",
                                                   choices = c("EQU","RAN"),selected = "EQU",multiple = FALSE),
                                       selectInput(inputId="Elit_SHID", label = "Choose if Elitism should be added:",
                                                   choices = c(TRUE,FALSE),selected = TRUE,multiple = FALSE),
                                       conditionalPanel(
                                         condition = "input.Elit_SHID == 'TRUE'",
                                         sliderInput(inputId="nElit_SHID", label = "How much individuals should be in the elite group?",
                                                     value=5,min=1, max=15)
                                       ),
                                       selectInput(inputId="Trim_SHID", label = "Should turbine-count-adjustment be random or probabilistic?",
                                                   choices = c("Random","Probabilistic"),selected = "Probabilistic",multiple = FALSE),
                                       sliderInput(inputId="Mutr_SHID", label = "What should the mutation percentage be?",
                                                   value=0.008,min=0.001, max=0.2, step = 0.001)
                              ),
                              ########## TERRAIN INPUTS ##########
                              br(),
                              tags$div(id="POCSBO4",
                                       h4("TERRAIN INPUTS:"),
                                       sliderInput(inputId="Surface_SHID", label = "What is the default surface roughness of the area?",
                                                   value=0.3,min=0.0001,max=5,step = 0.05),
                                       # shinyBS::bsTooltip(id = "Surface_SHID", title = "What was the height of the wind data measurements.",
                                       #                    placement ="right", trigger="click"),
                                       
                                       
                                       
                                       # selectInput(inputId="Topo_SHID", label = "Should terrain effects be taken into account, using geodata?",
                                       # choices = c(TRUE,FALSE),selected = FALSE, multiple = FALSE),
                                       conditionalPanel(
                                         condition = "input.Topo_SHID == 'TRUE'",
                                         fileInput(inputId="CLC_TIF", 
                                                   label = "Please upload the Image File (.tif) of the Corine Land Cover Raster",
                                                   multiple = F, accept = ".tif")
                                       ),
                                       conditionalPanel(
                                         condition = "input.Topo_SHID == 'TRUE'",
                                         fileInput(inputId="CLC_Leg", 
                                                   label = "Please upload the processed Legend (.csv) of the Corine Land Cover Raster",
                                                   multiple = F, accept = ".csv")
                                       )
                              )
                              )
                            ),
                        
                        mainPanel(
                          tabsetPanel(
                            
                            tabPanel("Polygon Output", 
                                     ## Plot the Polygon
                                     wellPanel(
                                       ##-----------------------------------------------
                                       # verbatimTextOutput("testbutton"),
                                       
                                       
                                       
                                       h4(strong("Polygon:")),
                                       verbatimTextOutput("shpprint"),
                                       div(id="PlotZent",
                                           plotOutput("SHPplot", width="100%",hover="Considered Area")
                                       ),
                                       
                                       h4(strong("Input Polygon Files:")),
                                       verbatimTextOutput("prntwhateveritis"),
                                       
                                       verbatimTextOutput("PlotWhatNeed")
                                       
                                       
                                       # actionButton("Srtm_PLot", label = "Plot Elevation"),
                                       # plotOutput("srtmpl")
                                       
                                     ),
                                     ## Calculate Grid and Plot AB
                                     actionButton("GridBut","Draw a Grid over the Area"),
                                     shinyjs::hidden(
                                       div(id="HidGridDiv", 
                                           wellPanel(
                                             br(),
                                             h4("Adjust the Grid Spacing Inputs including the rotor radius:"), 
                                             
                                             tags$div(id="GSFL",
                                                      sliderInput(inputId="fcrr_SHID", label = "By which factor should the rotor radius be multiplied, to 
                                                                  model a grid cell?", value=10,min=2, max=15,step=1, width='70%'),
                                                      sliderInput(inputId="Propo_SHID", label = "Determine a proportionality factor for grid calculation", 
                                                                  value=1, min=0.01, max=1,step=0.01, width='70%')
                                                      ),
                                             
                                             plotOutput("GRIDplot", width="100%",hover="Gridded Area")
                                       )
                                           )
                                       ),
                                     ## Transform Polygon AB
                                     actionButton("TransPol", "Set Original Projection"),
                                     shinyjs::hidden(
                                       div(id="HidTransPol", 
                                           wellPanel(
                                             textInput("SpRefSys", label = c("Original Spatial Reference System of the uploaded Shapefile"), 
                                                       value = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                                                       placeholder = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
                                             ),
                                             helpText("If the original reference system differs from the one, given above, please use the text field to",
                                                      "insert your own reference system. Make sure that it is given in .proj4-format.",
                                                      "Alternatively, you can also assign the desired EPSG code of the projection.",
                                                      "The algorithm transforms the shapefile to the European reference system [EPSG:3035] to",
                                                      "calculate distances and areas correctly.",
                                                      br(),"The default original reference system is [EPSG:4326]:",br(),"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                             actionButton("ResProjDef", label = "Reset Default Value"),
                                             tags$p("For further reference systems, please visit:"),
                                             tags$a(href = "http://spatialreference.org/ref/epsg/3035/", "spatialreference.org")
                                           )
                                       )                  
                                     ),
                                     ## Print out Summary of Polygon. AB
                                     actionButton("GridButSum","Summary of Polygon"),
                                     shinyjs::hidden(
                                       div(id = "HidTextDiv", verbatimTextOutput("GridTable"))
                                     )
                            ),
                            
                            tabPanel("Real Map",
                                     leaflet::leafletOutput("mymap", height = 500),
                                     
                                     absolutePanel(
                                       top = 200, right = 20, width = 190, draggable = TRUE,fixed = T,cursor = "move",
                                       div(id="RLSLF",
                                           sliderInput("opacityRL", "Opacity", min = 0, max = 1, value = 0.2, step = 0.1)
                                       ),
                                       selectInput("colorsRL", "Color",choices = c("white","black","blue","red","yellow"),
                                                   selected = "blue"),
                                       selectInput("bmap", "Base maps", choices = c("Esri.WorldImagery","Thunderforest.Outdoors"), 
                                                   selected = "Thunderforest.Outdoors"),
                                       actionButton("update", "Update Map!")
                                     )
                            ),
                            
                            tabPanel("Wind Data Output",
                                     div(id="ALLINPUTS1",
                                         tabsetPanel(
                                           tabPanel("Plotting Settings",
                                                    
                                                    tags$div(id="FARBANDER",
                                                             sliderInput(inputId="Windrose_SpBin", label = "Select the bin size of wind speeds?",
                                                                         width = "70%",value=3,min=1,max=9,step = 1),
                                                             sliderInput(inputId="Windrose_DirBin", label = "Select the bin size of wind directions?",
                                                                         width = "70%",value=10,min=5,max=90,step = 5)
                                                    ),
                                                    selectInput(inputId="ColPAlwin", label="Color Palette", 
                                                                choices = c("YlGnBu","Spectral", "RdYlBu","Blues","GnBu"))
                                           ),
                                           tabPanel("Input File Specifications",
                                                    selectInput(inputId="DtDec",label = "Decimal separator", choices = c(",","."),selected = "."),
                                                    selectInput(inputId="DtSep",label = "Column separator", choices = c(";",","),selected = ";")
                                           )
                                         )
                                     ),
                                     wellPanel(
                                       shinyjs::hidden(
                                         div(id = "WiPlSHi", 
                                             plotOutput("PlotContent")
                                         )
                                       )
                                     ),
                                     wellPanel(
                                       p(
                                         actionButton("WindButSum","Show Wind Data Table"),
                                         style= "padding-left: 35%;", 
                                         br()
                                       ),
                                       
                                       shinyjs::hidden(
                                         div(id = "WindButDiv", 
                                             DT::dataTableOutput("tableContent", width = "70%"))
                                       )
                                     )
                            ),
                            
                            
                            tabPanel("Controll & Run", 
                                     verbatimTextOutput("summary_SHID"),
                                     verbatimTextOutput("summary_TERR"),
                                     
                                     
                                     checkboxInput(inputId="CheckGA_SHID", 
                                                   label = "My Inputs are ok!",
                                                   value = FALSE),
                                     actionButton("Submit",label = "Let's run an Optimization!"),
                                     actionButton("Reset", label = "Stop and Reset"),
                                     br(),
                                     verbatimTextOutput("ResGA")
                            ),
                            tabPanel("Plot the Optimization Results",
                                     br(),
                                     fluidRow(
                                       
                                       actionButton("RGUI1_AB", label = "Best Found Solution"),
                                       # actionButton("RGUITERR_AB", label = "Plot Terrain Effects"),  
                                       actionButton("RGUI2_AB", label = "Evolution of Values"),
                                       actionButton("RGUI3_AB", label = "Overview of Genetic Algorithm"),
                                       actionButton("RGUI4_AB", label = "Plot all Values"),
                                       actionButton("RGUI5_AB", label = "Differences to Previous Generation"),
                                       
                                       shinyjs::hidden(
                                         div(id="RGUI1",
                                             h4("Best Solution:"),
                                             
                                             div(style="display:inline-block; margin-right:30px; float: left;",
                                                 radioButtons(inputId="EneEff", label="Plot the best Efficiency or Energy solution",
                                                              choices = c("Energy","Efficiency"), selected = "Energy", inline = T)
                                             ),
                                             div(style="display:inline-block; margin-right:5px; float: left;",
                                                 numericInput("PlBeOn", label = "Which best solution should be plotted", min = 1, max = 5,
                                                              value = 1,step = 1)),
                                             verbatimTextOutput("prntwhateveritis1"),
                                             plotOutput("ResGA1"),
                                             tags$div(id="DnlCsc1", class="HoverBut",
                                                      downloadButton("downloadResG", "Download Result")
                                             ),
                                             br(),
                                             div(style="display:inline-block; margin-right:5px",
                                                 numericInput("PlBeOn2", label = "Which best solution should be plotted", min = 1, max = 5,
                                                              value = 1,step = 1)
                                             ),
                                             verbatimTextOutput("prntwhateveritis2"),
                                             # div(style="display:inline-block; margin-right:5px;",
                                             #     numericInput("RadiWa", label = "Size of Wake Influence", min = 1, max = 10,
                                             #                  value = 2,step = 1)
                                             # ),
                                             # div(style="display:inline-block; margin-right:5px",
                                             #     sliderInput("TurbIco", label = "Size of Turbine Icon", min = 1, max = 100,
                                             #                 value = 40,step = 1)
                                             # ),
                                             
                                             
                                             leaflet::leafletOutput("mymapresult", height = 500),
                                             div(id="MoreSetting1",style="display:inline-block;",
                                                 actionButton("absoPBU", label = "More Settings", width = "90%")
                                             ),
                                             shinyjs::hidden(
                                               tags$div(id = "absoPanel",
                                                        absolutePanel(
                                                          top = 300, right = 20, width = 230, draggable = TRUE,
                                                          fixed = T, cursor = "move",
                                                          div(id = "RLSLF1",
                                                              sliderInput("opacityRL1", "Opacity", min = 0, max = 1, 
                                                                          value = 0.2, step = 0.1),
                                                              sliderInput("TurbIco", label = "Turbine Icon Size", min = 1, max = 100,
                                                                          value = 40,step = 1),
                                                              radioButtons("IcoCol1", label = "Turbine Icon Color", choiceValues = list(1,2),
                                                                           choiceNames = list("Bright Icon", "Dark Icon")),
                                                              sliderInput("RadiWa", label = "Wake Influence Circle", min = 0, max = 5,
                                                                          value = 1.5,step = 0.1),
                                                              sliderInput("bord", "Border Size", min = 0, max = 20, 
                                                                          value = 1.2, step = 0.1)
                                                              
                                                          ),
                                                          selectInput("colorsRL1", "Color",choices =
                                                                        c("white","black","blue","red","yellow"),
                                                                      selected = "blue")
                                                        )
                                               )
                                             )
                                             
                                             
                                             
                                         )
                                       ),
                                       
                                       
                                       ####################################################
                                       shinyjs::hidden(
                                         div(id="RGUITERR",
                                             h4("Terrain Effects:"),
                                             
                                             div(style="display:inline-block; margin-right:5px",
                                                 radioButtons(inputId = "TerrPl",
                                                              label = "Plot Terrain Effects for the best Efficiency or Energy solution",
                                                              choices = c("Energy","Efficiency"), selected = "Energy", inline = T)
                                             ),
                                             div(style = "display:inline-block; margin-right:5px",
                                                 numericInput("PlBeOn1", label = "Which best solution should be plotted", 
                                                              min = 1, max = 5,
                                                              value = 1,step = 1)
                                             ),
                                             # plotOutput("CCL"),
                                             plotOutput("TerrEff")
                                             
                                         )
                                       ),
                                       
                                       shinyjs::hidden(
                                         div(id = "RGUI2",
                                             h4("Evolution of Energy or Efficiency:"),
                                             # div(style="display:inline-block; margin-right:5px",
                                             #     radioButtons(inputId="EvoAsk", label="Plot the best Efficiency or Energy solution",
                                             #                  choices = c("Energy","Efficiency"), selected = "Energy", inline = T)
                                             # ),
                                             div(style = "display:inline-block; margin-right:5px",
                                                 sliderInput("EvoSp", label = "Set the smoothing factor",
                                                             min = 0.1, max = 1, value = 0.5, step = 0.1)
                                             ),
                                             plotOutput("Evo")
                                         )
                                       ),
                                       shinyjs::hidden(
                                         div(id = "RGUI3",
                                             h4("Mechanisms of Genetic Algortihm:"),
                                             plotOutput("OvGAP")
                                         )
                                       ),
                                       shinyjs::hidden(
                                         div(id = "RGUI4",
                                             h4("Plot all Individual Values:"),
                                             plotOutput("ClGAP")
                                         )
                                       ),
                                       shinyjs::hidden(
                                         div(id = "RGUI5",
                                             h4("Differences to previous Generation:"),
                                             plotOutput("DifGAP")
                                         )
                                       )
                                     )
                            )
                          )
                          )
                        )
                      ),
             
             
             
             
             tabPanel("Test a Layout",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          tags$div(id = "POCSBO5",
                                   radioButtons(inputId = "TestMethod", label="Point Inputs", 
                                                choices=c("Data","Random"),selected = NULL),
                                   
                                   conditionalPanel(
                                     condition = "input.TestMethod == 'Data'",
                                     fileInput(inputId="TestLaySh", 
                                               label = "Please upload a data table with the desired coordinates",
                                               multiple = F, accept = ".csv"),
                                     p("The .csv-file must consist of an indexed data frame with the 
                                       header ('ID';'X';'Y') and the coordinates must be projected in 'EPSG:3035'.")
                                     ),
                                   
                                   conditionalPanel(
                                     condition = "input.TestMethod == 'Random'",
                                     sliderInput(inputId = "NRanTeLa",
                                                 label = "How many turbines are required?", 
                                                 min = 2, max = 50,
                                                 value = 10, step = 1),
                                     selectInput(inputId = "RanMethTe",
                                                 label = "Which random method should be used?",
                                                 choices = c("random","regular","stratified","nonaligned","hexagonal",
                                                             "clustered"),
                                                 selected = "regular", multiple = F)
                                   )
                                   ),
                          
                          br(),
                          actionButton(inputId="TeLaAct", label = "Test the Layout", width = "100%")
                        ),
                        
                        
                        
                        mainPanel(
                          fluidRow(
                            column(5, 
                                   h4(strong("Input Polygon:")),
                                   plotOutput("SHPplot1", width="100%", inline = F)       
                            ),
                            column(7,
                                   shinyjs::hidden(
                                     div(id="TstResShw",
                                         h4(strong("Resulting Layout:")),
                                         plotOutput("PltTest", width="100%", inline = F)
                                     )
                                   )
                            )
                          ),
                          fluidRow(
                            column(12,
                                   shinyjs::hidden(
                                     div(id="TstRlM",
                                         h4(strong("Resulting Layout on Real Map:")),
                                         leaflet::leafletOutput("TstLeaf", height = 500, width = "100%"),
                                         div(id="MoreSetting", style="display:inline-block;",
                                             actionButton("absoPBU1", label = "More Settings", width = "90%")
                                         )
                                     )
                                   ),
                                   shinyjs::hidden(
                                     tags$div(id="absoPanel1",
                                              absolutePanel(
                                                top = 370, right = 40, width = 170, draggable = TRUE,
                                                fixed = T,cursor = "move",
                                                div(id="RLSLF2",
                                                    sliderInput("opacityRL2", "Opacity", min = 0, max = 1, 
                                                                value = 0.2, step = 0.1),
                                                    sliderInput("Ico1", "Turbine Icon Size", min = 10, max = 150, 
                                                                value = 40, step = 1),
                                                    radioButtons("IcoCol", label = "Turbine Icon Color",
                                                                 choiceValues = list(1,2),
                                                                 choiceNames = list("Bright Icon", "Dark Icon")),
                                                    sliderInput("RadiWa1", "Wake Influence Circle", min = 0, max = 5, 
                                                                value = 1.5, step = 0.1),
                                                    sliderInput("bord1", "Border Size", min = 0, max = 15, 
                                                                value = 1.2, step = 0.1)
                                                ),
                                                selectInput("colorsRL2", "Color",choices =
                                                              c("white","black","blue","red","yellow"),
                                                            selected = "blue")
                                              )
                                     )
                                   )
                            )
                          ),
                          
                          fluidRow(
                            column(5,
                                   h4(strong("Input Variables:")),
                                   shinyjs::hidden(
                                     div(id="DtPoTst", 
                                         verbatimTextOutput("TstPoiunRs")
                                     )
                                   ),
                                   verbatimTextOutput("InVarTest")
                            ),
                            column(7,
                                   div(id="TstResShw1",
                                       h4(strong("Resulting Layout Data:")),
                                       verbatimTextOutput("ResLayDat"),
                                       tags$div(id="DnlCsc", class="HoverBut",
                                                downloadButton("downloadTD", "Download Result")
                                       )
                                   )
                            )
                          )
                        )
             )
             )
             )
  )
