
library(shiny)
# library(rgdal)
# library(raster)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(lubridate)
# library(dygraphs)
library(threadr)
library(tidyr)
library(readr)
library(DT)
library(ggplot2)
library(stringr)
# library(broom)
library(dygraphs)
# library(ggpmisc)
# library(plotly)
# library(GGally)
# library(htmlwidgets)
# library(htmltools)
# library(webshot)
# library(ggrepel)
# library(openair)
# library(widgetframe)
# library(grid)
# library(gridExtra)
# library(pander)
# library(varhandle)
options(scipen=5)
options(warn=-1)
library(RPostgreSQL)
library(shinybusy)
library(sf)
# library(sp)
library(hms)



####---- library for icons ---- ###########################
## https://getbootstrap.com/docs/3.4/components/#glyphicons


# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "GPS data from vehicles", titleWidth = 450),
                    

                    dashboardSidebar(
                      width = 290,
                      paste("Time:",Sys.time()),
                      sidebarMenu(
                        # helpText("Elaboration of Floating Car Data (GPS data) with \n
                        #          information about trips and travelled route"),
                        
                        
                      # selectInput("database", "Database",
                      #               c("DB Roma" = "HAIG_ROMA",
                      #                 "DB Salerno" = "HAIG_SALERNO",
                      #                 "DB Catania" = "HAIG_CATANIA",
                      #                 "DB Brescia" = "HAIG_BRESCIA"), selected = "HAIG_ROMA"),
                      # https://mastering-shiny.org/basic-ui.html
                        
                        
                      # fileInput('route_BRESCIA', 'load data for BRESCIA',
                      #           accept = c(".rds")),
                      # 
                      # fileInput('route_ROMA', 'load data for ROMA',
                      #           accept = c(".rds")),
                      
                      radioButtons("database", "Database", c("DB Roma" = "HAIG_ROMA",
                                                             "DB Salerno" = "HAIG_SALERNO",
                                                             # "DB Catania" = "HAIG_CATANIA",
                                                             "DB Brescia" = "HAIG_BRESCIA"), selected = "HAIG_ROMA"),
                   
                      
                      
                      # conditionalPanel(
                      #   condition = "input.database == 'HAIG_ROMA'",
                      #     selectInput(
                      #       "type", "Veh. Type:",
                      #       c("Cars" = "1", "Fleet" = "2"), selected = "2")),
                      
                      
                      
                     
                      menuItem("vehicles & route data", tabName = "route_data", icon = icon("th-list")),
                      menuItem("profiles", tabName = "profiles", icon = icon("stats", lib = "glyphicon")),
                      menuItem("offset positions", tabName = "offset", icon = icon("stats", lib = "glyphicon")),
                      menuItem("Speeds", tabName = "speeds", icon = icon("picture", lib = "glyphicon")),
                      menuItem("Stop times", tabName = "stop_times", icon = icon("picture", lib = "glyphicon")),
                      menuItem("Trip distances", tabName = "trip_distance", icon =icon("picture", lib = "glyphicon")),
                      menuItem("Trip times", tabName = "trip_times", icon = icon("picture", lib = "glyphicon")),
                      # menuItem("Traffic flow (mapmatching)", tabName = "traffic_flow", icon = icon("picture", lib = "glyphicon")),
                      
                    
                      # fileInput('matched_routes_ROMA', 'load ROMA matched routes',
                      #           accept = c(".rds")),
                      
                      
                      # radioButtons("type", "Veh. Type", c("Cars" = "1",
                      #                                           "Fleet" = "2"), selected = "2"),    #checkboxGroupInput
                      
                  
                      conditionalPanel(
                        condition = "input.database == 'HAIG_ROMA'",
                        selectInput("type", "Veh. Type:",
                                c("Cars" = "1", "Fleet" = "2"), selected = "2")),
                      
                      conditionalPanel(
                        condition = "input.database == 'HAIG_SALERNO'",
                        selectInput("type", "Veh. Type:",
                                    c("Cars" = "1", "Fleet" = "2"), selected = "2")),
                      
                      conditionalPanel(
                        condition = "input.database == 'HAIG_CATANIA'",
                        selectInput("type", "Veh. Type:",
                                    c("Cars" = "1", "Fleet" = "2"), selected = "2")),
                      
                      
                      conditionalPanel(
                        condition = "input.database == 'HAIG_BRESCIA'",
                        selectInput("type", "Veh. Type:",
                                    c("Cars" = "1"), selected = "1")),
                      
                      
                      # dateInput("start", "Start Day",
                      #           value = "2019-10-09", format = "yyyy-mm-dd"),
                      # 
                      # dateInput("end", "End Day",
                      #           value = "2019-10-09", format = "yyyy-mm-dd"),
                      
                      sliderInput("range_hours", 
                                  label = "time interval:",
                                  value = c(8, 9), step =1,
                                  min = 7, max = 14),  ## 16-->19
                      
                      menuItem("Matched Routes", tabName = "MAP", icon = icon("th"))
                      
                      
                      )
                      ),
                    
                     
                  
                      dashboardBody(
                        
                        
                        ####################################################################
                        ##### ----------ADD TEXT in the dashboard header----------##########
                        ####################################################################
                        
                        tags$head(tags$style(HTML(
                          '.myClass { 
                              font-size: 20px;
                              line-height: 50px;
                              text-align: left;
                              font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                              padding: 0 15px;
                              overflow: hidden;
                              color: white;
                            }
                          '))),
                                              tags$script(HTML('
                            $(document).ready(function() {
                              $("header").find("nav").append(\'<span class="myClass"> Demonstrator for on-line FCD data elaboration </span>\');
                            })
                           ')),
                      
                        ####################################################################
                        ##### --------------------------------------------- ################
                        ####################################################################
                        
                            
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
                                            textOutput("text_table1"),
                                            br(),
                                            br(),
                                            withSpinner(DT::dataTableOutput('table_1', width = "100%"), color="#0dc5c1")
                                            # withSpinner(plotOutput("plot_offset_position", height = "300px", width = "800px"), type=6),
                                            # withSpinner(plotlyOutput("bar_offset_position", height = "300px", width = "800px"), type=6)
                                          ),
                                          
                                          
                                          tabPanel(
                                            tags$b("route data"),
                                            icon = icon("th"), 
                                            textOutput("text_table2"),
                                            br(),
                                            br(),
                                            withSpinner(DT::dataTableOutput('table_2'), color="#0dc5c1")
                                          )
                                        )
                                      )),
                              
                  
                              
                              tabItem(tabName = "profiles",
                                      fluidRow(
                                        tabBox(
                                          height = 1000, width = 950, selected = tags$b("travelled distance profiles"),
                                          
                                          tabPanel(
                                            tags$b("travelled distance profiles"), 
                                            withSpinner(plotOutput("plot_hourly_distance_profile", height = "400px", width = "800px"), type=6),
                                            # withSpinner(plotOutput("plot_hourly_trips_profile", height = "400px", width = "800px"), type=6)
                                          ),
                                          
                                          tabPanel(
                                            tags$b("trips profiles"),
                                            withSpinner(plotOutput("plot_hourly_trips_profile", height = "400px", width = "800px"), type=6)
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
                                            # withSpinner(plotOutput("bar_offset_position", height = "300px", width = "800px"), type=6)
                                            # withSpinner(plotlyOutput("bar_offset_position", height = "300px", width = "800px"), type=6)
                                          ),
                                          
                                          tabPanel(
                                            tags$b("...bar plot"),
                                            withSpinner(plotOutput("bar_offset_position", height = "300px", width = "800px"), type=6)
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
                                          ),
                                          
                                          tabPanel(
                                            tags$b("trip percentage"),
                                            withSpinner(plotOutput("plot_trips_speed", height = "300px", width = "800px"), type=6)
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
                              
                              
                              
                              
                              # First tab content
                              tabItem(tabName = "MAP",
                                      fluidRow(
                                        tabBox(
                                          height = 750, width = 950, selected = tags$b("Traffic Loading"),
                                          tabPanel(
                                            tags$b("Traffic Loading"), 
                                            withSpinner(leafletOutput('myMap', height = 650, width = 750), type = 8)
                                          )
                                        )
                                      ))
                              
                              ## make iterative map with leafleft.....using map-matching data.......
                              # tabItem(tabName = "traffic_flow",
                              #         fluidRow(
                              #           tabBox(
                              #             height = 1000, width = 950, selected = tags$b("Brescia (06 March 2019, cars & fleet)"),
                              #             tabPanel(
                              #               tags$b("Roma (09 October 2019),fleet"), 
                              #               tags$iframe(src = "WED_09_October_2019_HEAVY_Roma.html",
                              #                           height = "400px", width = "800px")
                              #             ),
                              #             
                              #             tabPanel(
                              #               tags$b("Salerno (02 Sep 2019, fleet)"),
                              #               tags$iframe(src = "MON_02_September_2019_Traffic_HEAVY_SALERNO.html",
                              #                           height = "400px", width = "800px")
                              #               
                              #             ),
                              #             
                              #             tabPanel(
                              #               tags$b("Catania (fleet)"),
                              #               tags$iframe(src = "HEAVY_traffic_loads_all_EDGES_all_Catania.html",
                              #                           height = "400px", width = "800px")
                              #               
                              #             ),
                              #             
                              #             tabPanel(
                              #               tags$b("Brescia (06 March 2019, cars & fleet)"),
                              #               tags$iframe(src = "06_March_trip_all_vehicles_Brescia.html",
                              #                           height = "400px", width = "800px")
                              #               
                              #             )
                              #             
                              #           )
                              #         ))
                            
                              
                        ))
                    )



