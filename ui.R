
library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(lubridate)
library(dygraphs)
library(threadr)
library(tidyr)
library(readr)
library(DT)


library(ggplot2)
library(stringr)
library(tidyr)
library(readr)
library(broom)
library(threadr)
library(dplyr)
library(dygraphs)
library(ggpmisc)
library(plotly)
library(GGally)
library(htmlwidgets)
library(htmltools)
library(webshot)
library(ggrepel)
library(openair)
library(widgetframe)
library(grid)
library(gridExtra)
library(pander)
library(varhandle)
options(scipen=5)
options(warn=-1)
library(RPostgreSQL)
options(scipen=5)


####---- library for icons ---- ###########################
## https://getbootstrap.com/docs/3.4/components/#glyphicons


# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

jscode <- "shinyjs.refresh = function() { history.go(0); }"

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "FCD data from Viasat"),

                    dashboardSidebar(
                      width = 290,
                      paste("Time:",Sys.time()),
                      sidebarMenu(
                        
                        
                      selectInput("database", "database",
                                    c("DB Roma" = "HAIG_ROMA",
                                      "DB Salerno" = "HAIG_SALERNO",
                                      "DB Catania" = "HAIG_CATANIA",
                                      "DB Brescia" = "HAIG_BRESCIA"), selected = "HAIG_ROMA"),
                      
                      
                      menuItem("vehicles & route data", tabName = "route_data", icon = icon("th-list")),
                      menuItem("offset positions", tabName = "offset", icon = icon("stats", lib = "glyphicon")),
                      menuItem("Speeds", tabName = "speeds", icon = icon("picture", lib = "glyphicon")),
                      menuItem("Stop times", tabName = "stop_times", icon = icon("picture", lib = "glyphicon")),
                      menuItem("Trip distances", tabName = "trip_distance", icon =icon("picture", lib = "glyphicon")),
                      menuItem("Trip times", tabName = "trip_times", icon = icon("picture", lib = "glyphicon")),
                      menuItem("Traffic flow (mapmatching)", tabName = "traffic_flow", icon = icon("picture", lib = "glyphicon"))
                      
                      
                      )
                      ),
                    
                    
                  
                      dashboardBody(
                            
                            tags$head(
                              tags$style(
                                "body{
                          min-height: auto;
                          height: auto;
                          max-width: auto;
                          margin: auto;
                          }"),
                              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                            ),
                            
                            
                            
                            tabItems(
                              
                              # First tab content
                              
                              
                              
                              tabItem(tabName = "route_data",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("Num.vehicles by type"),
                                          tabPanel(
                                            tags$b("Num.vehicles by type"), 
                                            DT::dataTableOutput('table_1', width = "100%"),
                                            # withSpinner(plotOutput("plot_offset_position", height = "300px", width = "800px"), type=6),
                                            # withSpinner(plotlyOutput("bar_offset_position", height = "300px", width = "800px"), type=6)
                                       
                                          ),
                                          
                                          
                                          tabPanel(
                                            tags$b("route data"),
                                            icon = icon("th"), 
                                            withSpinner(DT::dataTableOutput('table_2'), type = 6)
                                            
                                          )
                                        )
                                      )),
                              
                  
                              
                              
                              tabItem(tabName = "offset",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("bias between END & START of new trip"),
                                          tabPanel(
                                            tags$b("bias between END & START of new trip"), 
                                            # DT::dataTableOutput('table_1', width = "100%"),
                                            withSpinner(plotOutput("plot_offset_position", height = "300px", width = "800px"), type=6),
                                            withSpinner(plotlyOutput("bar_offset_position", height = "300px", width = "800px"), type=6)
                                          )
        
                                        )
                                      )),
                              
                              
                              tabItem(tabName = "speeds",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("mean speed"),
                                          tabPanel(
                                            tags$b("mean speed"), 
                                            withSpinner(plotOutput("plot_average_speed", height = "300px", width = "800px"), type=6)
                                          )
                                        
                                        )
                                      )),
                              
                              tabItem(tabName = "stop_times",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("stop times"),
                                          tabPanel(
                                            tags$b("stop times"), 
                                            withSpinner(plotOutput("plot_stop_time", height = "300px", width = "800px"), type=6)
                                          )
                                          
                                        )
                                      )),
                              
                              tabItem(tabName = "trip_distance",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("trip distance"),
                                          tabPanel(
                                            tags$b("trip distance"), 
                                            withSpinner(plotOutput("plot_trip_distance", height = "300px", width = "800px"), type=6)
                                          )
                                          
                                        )
                                      )),
                              
                              tabItem(tabName = "trip_times",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("trip times"),
                                          tabPanel(
                                            tags$b("trip times"), 
                                            withSpinner(plotOutput("plot_trip_time", height = "300px", width = "800px"), type=6)
                                          )
                                          
                                        )
                                      )),
                              
                              tabItem(tabName = "traffic_flow",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("Roma"),
                                          tabPanel(
                                            tags$b("Roma"), 
                                            tags$iframe(src = "March_trip_CARS_only_UNIBS.html",
                                                        height = "400px", width = "800px")
                                          ),
                                          
                                          tabPanel(
                                            tags$b("Salerno"),
                                            tags$iframe(src = "March_trip_CARS_only_UNIBS.html",
                                                        height = "400px", width = "800px")
                                            
                                          ),
                                          
                                          tabPanel(
                                            tags$b("Catania"),
                                            tags$iframe(src = "SUN_24_Feb_2019_CARS_traffic_counts_all_EDGES_all_Catania.html",
                                                        height = "400px", width = "800px")
                                            
                                          ),
                                          
                                          tabPanel(
                                            tags$b("Brescia"),
                                            tags$iframe(src = "March_trip_CARS_only_UNIBS.html",
                                                        height = "400px", width = "800px")
                                            
                                          )
                                          
                                        )
                                      ))
                            
                              
                        ))
                    )

