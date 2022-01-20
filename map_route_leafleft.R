


## load packages

rm(list = ls())

library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(shinydashboard)
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
library(dbplyr)
library(shinybusy)
library(sf)
library(sp)

setwd("D:\\Federico\\FCDapp")

#https://rviews.rstudio.com/2017/05/17/databases-using-r/


# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")



DB <- "HAIG_Viasat_RM_2019"
conn_HAIG <- dbConnect(drv, dbname = DB,           
                       host = "10.0.0.1", port = 5432,
                       user = "postgres", password = "superuser")


matched_routes = dbGetQuery(conn_HAIG,"
                                    SELECT *
                                    FROM public.matched_routes")


## you can directly load the .csv file with LINESTRING set as geom....

# matched_routes$geometry = dbGetQuery(conn_HAIG, "SELECT ST_AsText(geom) 
#                               FROM public.matched_routes
#                               ST_AsText")


matched_routes <- read_csv("matched_routes_2019_10_09.csv")[-1]

# geom = matched_routes$geometry
# names(geom) <- "geometry"
# df <- cbind(matched_routes$counts, geom)
# names(df) <- c("counts", "geometry")


df <- matched_routes %>%
  select(hour_date,
         count,
         u,v,
         vehtype,
         name,
         `load(%)`,
         geometry)
### normalize counts to 1
df$count <- df$count / max(df$count)


## convert dataframe into Spatial Object
sf <- sf::st_as_sf(df, wkt = "geometry" )
# plot(sf)

## convert spatial object into SpatialLines
edges <- as_Spatial(sf$geometry)
# plot(edges)




### iteractive plot
map <- leaflet() %>%
  # setView(-5.35, 36.150, 16) %>%
  addTiles(group = "OSM (default)") %>%
  addPolylines(data = edges, 
               color='blue', 
               group='edge',
               weight = df$counts) %>%
  addLayersControl(
    overlayGroups = c("edge"),
    options = layersControlOptions(collapsed = TRUE))

map






###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

  
# wkb = structure(list( matched_routes[12, c("geom")]), class = "WKB")
# edge <- st_as_sfc(wkb,EWKB = TRUE) %>% st_coordinates()
# edge <- as.data.frame(edge)
# # make a spatial dataframe with edge data 
# sp_edge <- SpatialPointsDataFrame(edge[,c("X", "Y")], edge,            # lat, lon
#                                      proj4string=CRS("+init=epsg:4326")) 
# plot(sp_edge)
# # make lines
# p1 = Line(edge[,c("X", "Y")]) #lon & lat
# # make Polygon class
# p2 = Lines(list(p1), ID = "edge")
# 
# 
# wkb = structure(list( matched_routes[13, c("geom")]), class = "WKB")
# edge <- st_as_sfc(wkb,EWKB = TRUE) %>% st_coordinates()
# edge <- as.data.frame(edge)
# # make a spatial dataframe with edge data 
# sp_edge <- SpatialPointsDataFrame(edge[,c("X", "Y")], edge,            # lat, lon
#                                   proj4string=CRS("+init=epsg:4326")) 
# plot(sp_edge)
# # make lines
# p3 = Line(edge[,c("X", "Y")]) #lon & lat
# # make Polygon class
# p4 = Lines(list(p3), ID = "edge_1")
# # make spatial polygons 
# 
# 
# edge_line = SpatialLines(list(p2, p4),proj4string=CRS("+init=epsg:4326"))
# plot(edge_line)
# 
# 
# c(as.numeric(slot(edge_line@lines, "values")))
# 
# 
# 
# ### iteractive plot
# map <- leaflet(data = matched_routes[12,]) %>%
#   # setView(-5.35, 36.150, 16) %>%
#   addTiles(group = "OSM (default)") %>%
#   addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
#   addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
#   addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
#   addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
#   addPolylines(data = edge_line, color='blue', group='edge',
#                label = as.character(matched_routes[12,]$counts),
#                labelOptions = labelOptions(noHide = F)) %>%
#   addLayersControl(
#     baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
#     overlayGroups = c( "edge"),
#     options = layersControlOptions(collapsed = TRUE))
# 
# map

