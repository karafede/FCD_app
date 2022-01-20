
rm(list = ls())


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
library(shinyMobile)




ui <- f7Page(
  title = "My app",
  # f7SingleLayout(
  f7TabLayout(
    
      panels = tagList(
        f7Panel(title = "routes", side = "left", theme = "light", "",

                f7Select("database", "Select City", c("Roma 2019" = "HAIG_ROMA",
                                                      "Salerno 2019" = "HAIG_SALERNO",
                                                      "Catania 2019" = "HAIG_CATANIA",
                                                      "Brescia 2019" = "HAIG_BRESCIA"), selected = "HAIG_BRESCIA"),

                f7PanelMenu(
                  id = "menu",
                  f7PanelItem(tabName = "profiles", title = "Route profiles", icon = f7Icon("email"), active = TRUE),
                  f7PanelItem(tabName = "offsets", title = "Trip offsets", icon = f7Icon("email"), active = FALSE),
                  f7PanelItem(tabName = "speeds", title = "Speeds", icon = f7Icon("email"), active = FALSE),
                  f7PanelItem(tabName = "stop_times", title = "Stop times", icon = f7Icon("email"), active = FALSE),
                  # f7PanelItem(tabName = "trip_distance", title = "Trip distance", icon = f7Icon("email"), active = FALSE),
                  # f7PanelItem(tabName = "trip_times", title = "Trip times", icon = f7Icon("email"), active = FALSE),
                  f7PanelItem(tabName = "route_data", title = "vehicles & route data", icon = f7Icon("email"), active = FALSE)
                ),

                effect = "reveal"
        ),

        f7Panel(title = "map-matching", side = "right", theme = "dark", " ",
                
                # f7Select(
                #   inputId = "type",
                #   label = "Veh. Type:",
                #   selected = "1",
                #   choices = c("Cars" = "1", "Fleet" = "2")
                # ),

                f7Slider(
                  inputId = "range_hours",
                  label = "time interval:",
                  max = 21,
                  min = 6,
                  value = c(8, 9),
                  scaleSteps = 1,
                  scaleSubSteps = 10,
                  scale = TRUE,
                  color = "orange",
                  labels = tagList(
                    f7Icon("circle"),
                    f7Icon("circle_fill")
                  )
                ),


                f7PanelMenu(
                  id = "menu",
                  f7PanelItem(tabName = "MAP", title = "Matched Routes", icon = f7Icon("th"), active = FALSE)
                ),

                effect = "cover")



        ),

        navbar = f7Navbar(
          title = "Online FCD elaboration",
          hairline = FALSE,
          shadow = TRUE,
          leftPanel = TRUE,
          rightPanel = TRUE
        ),

        f7Items(

          f7Item(
            tabName = "route_data",
            br(),
            textOutput("text_table1"),
            br(),
            tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: white !important;}')),
            tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: yellow !important;}')),
            tags$style(HTML('table.dataTable th {background-color: white !important;}')),
            withSpinner(DT::dataTableOutput('table_1', width = "100%"), color="#0dc5c1")
            # br(),
            # textOutput("text_table2"),
            # br(),
            # withSpinner(DT::dataTableOutput('table_2', width = "100%"), color="#0dc5c1")
          ),
          f7Item(
            tabName = "profiles",
            br(),
            br(),
            withSpinner(plotOutput("plot_hourly_distance_profile"), color="#0dc5c1"),
            withSpinner(plotOutput("plot_hourly_trips_profile"),color="#0dc5c1"),
            withSpinner(plotOutput("plot_daily_distance_profile"),color="#0dc5c1"),
            textOutput("author")
          ),
          f7Item(
            tabName = "offsets",
            br(),
            br(),
            withSpinner(plotOutput("plot_offset_position"), color="#0dc5c1"),
            withSpinner(plotOutput("bar_offset_position"), color="#0dc5c1")
          ),
          f7Item(
            tabName = "speeds",
            br(),
            br(),
            withSpinner(plotOutput("plot_average_speed"), color="#0dc5c1"),
            withSpinner(plotOutput("plot_trips_speed"),color="#0dc5c1")
          ),
          f7Item(
            tabName = "stop_times",
            br(),
            br(),
            withSpinner(plotOutput("plot_stop_time"), color="#0dc5c1")
          ),
          # f7Item(
          #   tabName = "trip_times",
          #   br(),
          #   br(),
          #   withSpinner(plotOutput("plot_trip_time"), color="#0dc5c1")
          # ),
          # f7Item(
          #   tabName = "trip_distance",
          #   br(),
          #   br(),
          #   withSpinner(plotOutput("plot_trip_distance"), color="#0dc5c1")
          # ),
          f7Item(
            tabName = "MAP",
            br(),
            textOutput("text_maps"),
            br(),
            withSpinner(leafletOutput('myMap', height = 650, width = 750), type = 8)
          )

      )
    )
  )






# ui <- f7Page(
#   title = "My app",
#   f7SingleLayout(
#     navbar = f7Navbar(
#       title = "Demonstrator for on-line FCD data elaboration",
#       hairline = FALSE,
#       shadow = TRUE
#     ),
#     
#     toolbar = f7Toolbar(
#       position = "bottom",
#       f7Link(label = "Link 1", href = "https://www.google.com"),
#       f7Link(label = "Link 2", href = "https://www.google.com")
#     ),
#     f7Tabs(
#       id = "tabs",
#       style = "strong", animated = FALSE, swipeable = TRUE,
#       f7Tab(
#         tabName = "Profiles",
#         icon = f7Icon("today"),
#         active = TRUE,
#         f7Shadow(
#           intensity = 10,
#           hover = FALSE,
#           f7Card(
#             title = "hourly profiles",
#             f7Select("database", "Select City", c("Roma 2019" = "HAIG_ROMA",
#                                               "Salerno 2019" = "HAIG_SALERNO",
#                                               # "DB Catania" = "HAIG_CATANIA",
#                                               "Brescia 2019" = "HAIG_BRESCIA"), selected = "HAIG_ROMA"),
#             br(),
#             br(),
#             withSpinner(plotOutput("plot_hourly_distance_profile"), color="#0dc5c1"),
#             withSpinner(plotOutput("plot_hourly_trips_profile"),color="#0dc5c1")
#           ))),
# 
#       # f7Tab(
#       #   tabName = "Trip prof.",
#       #   icon = f7Icon("house"),
#       #   active = FALSE,
#       #   f7Shadow(
#       #     intensity = 10,
#       #     hover = TRUE,
#       #     f7Card(
#       #       title = "hourly profiles",
#       #       f7Select("database_1", "Select City", c("Roma 2019" = "HAIG_ROMA",
#       #                                         "Salerno 2019" = "HAIG_SALERNO",
#       #                                         # "DB Catania" = "HAIG_CATANIA",
#       #                                         "Brescia 2019" = "HAIG_BRESCIA")),
#       #       br(),
#       #       br(),
#       #       withSpinner(plotOutput("plot_hourly_trips_profile"),color="#0dc5c1") 
#       #     ))),
#       
#       f7Tab(
#         tabName = "offset",
#         icon = f7Icon("house"),
#         active = FALSE,
#         f7Shadow(
#           intensity = 10,
#           hover = TRUE,
#           f7Card(
#             title = "trips offset",
#             f7Select("database_2", "Select City", c("Roma 2019" = "HAIG_ROMA",
#                                              "Salerno 2019" = "HAIG_SALERNO",
#                                              # "DB Catania" = "HAIG_CATANIA",
#                                              "Brescia 2019" = "HAIG_BRESCIA")),
#             br(),
#             br(),
#             withSpinner(plotOutput("plot_offset_position"), color="#0dc5c1"),
#             withSpinner(plotOutput("bar_offset_position"), color="#0dc5c1")
#             
#           ))),
#       f7Tab(
#         tabName = "vehicles & route data",
#         icon = f7Icon("house"),
#         active = FALSE,
#         f7Shadow(
#           intensity = 10,
#           hover = TRUE,
#           f7Card(
#             title = "vehicles & route data",
#             f7Select("database_3", "Select City", c("Roma 2019" = "HAIG_ROMA",
#                                                     "Salerno 2019" = "HAIG_SALERNO",
#                                                     # "DB Catania" = "HAIG_CATANIA",
#                                                     "Brescia 2019" = "HAIG_BRESCIA")),
#             br(),
#             br(),
#             tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: pink !important;}')),
#             tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: yellow !important;}')),
#             tags$style(HTML('table.dataTable th {background-color: white !important;}')), 
#             withSpinner(DT::dataTableOutput('table_1', width = "100%"), color="#0dc5c1")
#             
#           )))
#       
#     
#      
#     )
#   )
# )



    
    
    
    
    
    

 
# ui <- f7Page(
#   title = "Split layout",
#   f7SplitLayout(
#     sidebar = f7Panel(
#       id = "sidebar",
#       title = "Sidebar",
#       side = "left",
#       theme = "dark",
#       f7PanelMenu(
#         id = "menu",
#         f7PanelItem(tabName = "tab1", title = "Travelled distance profile", icon = f7Icon("today"), active = TRUE),
#         f7PanelItem(tabName = "tab2", title = "Trips profile", icon = f7Icon("house"), active = TRUE)
#       ),
#       uiOutput("selected_tab")
#     ),
#     navbar = f7Navbar(
#       title = "Split Layout",
#       hairline = FALSE,
#       shadow = TRUE
#     ),
#    # main content
#     f7Items(
#       f7Item(
#         tabName = "tab1",
#         f7Select("database", "City", c("DB Roma" = "HAIG_ROMA",
#                                       "DB Salerno" = "HAIG_SALERNO",
#                                       # "DB Catania" = "HAIG_CATANIA",
#                                       "DB Brescia" = "HAIG_BRESCIA")),
#         plotOutput("plot_hourly_distance_profile")
#       ),
#       # f7Item(tabName = "tab2", "Tab 2 content"),
#       f7Item(
#         tabName = "tab2",
#         f7Select("database_a", "City", c("DB Roma" = "HAIG_ROMA",
#                                        "DB Salerno" = "HAIG_SALERNO",
#                                        # "DB Catania" = "HAIG_CATANIA",
#                                        "DB Brescia" = "HAIG_BRESCIA")),
#         plotOutput("plot_hourly_trips_profile")
#       ),
# 
#     )
#   )
# )




