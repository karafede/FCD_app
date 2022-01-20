

## load packages

rm(list = ls())


# library(lobstr)
# mem_used()

library(shiny)
# library(rgdal)
# library(raster)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(lubridate)
# library(dygraphs)
library(threadr)
library(tidyr)
library(readr)
library(DT)
library(ggplot2)
library(stringr)
# library(broom)
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
library(rdrop2)


cleanMem <- function(n=2) { for (i in 1:n) gc() }
## set timezone on the remote server when the application will be deployed 
Sys.setenv(TZ="Europe/Paris")

### connect to Dropbox account "karafede"
# token <- readRDS("D:\\Federico\\FCDapp\\droptoken.rds")
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)


# setwd("D:\\Federico\\FCDapp")
file.remove(c("route_ROMA.rds", "route_BRESCIA.rds", 
              "route_CATANIA.rds", "route_SALERNO.rds",
              "matched_routes_ROMA_2019_10_09.rds",
              "matched_routes_CATANIA_2019_02_14.rds",
              "matched_routes_SALERNO_2019_09_19.rds",
              "matched_routes_BRESCIA_UNIBS_2019_03_15.rds"))

# outputDir <- "FCD_data/input_files"
# filesInfo <- drop_dir(outputDir)



#https://rviews.rstudio.com/2017/05/17/databases-using-r/
## https://stackoverflow.com/questions/46337029/show-a-loading-bar-while-in-r-shiny-while-sourcing-a-script
## https://community.rstudio.com/t/how-to-use-future-promises-to-read-rds-files-in-background-to-decrease-initial-loading-latency-in-ie11/12880


# loads the PostgreSQL driver
# drv <- dbDriver("PostgreSQL")
# setwd("D:\\Federico\\FCDapp")
options(shiny.maxRequestSize=270*1024^2)


  # route_BRESCIA <- read_csv("route_BRESCIA.csv")
  # saveRDS(route_BRESCIA, file = "route_BRESCIA.rds")
  # route_ROMA <- read_csv("route_ROMA.csv")
  # saveRDS(route_ROMA, file = "route_ROMA.rds")
  # route_CATANIA <- read_csv("route_CATANIA.csv")
  # saveRDS(route_CATANIA, file = "route_CATANIA.rds")
  # route_SALERNO <- read_csv("route_SALERNO.csv") 
  # saveRDS(route_SALERNO, file = "route_SALERNO.rds")
 
  
  # route_BRESCIA <- readRDS(file = "route_BRESCIA.rds")
  # route_BRESCIA <- route_BRESCIA %>%
  #   select(-day, - hour, -month, -speed_trip)
  # route_BRESCIA$deviation_pos_m <- round(route_BRESCIA$deviation_pos_m, digits = 0)
  # route_ROMA <- readRDS(file = "route_ROMA.rds")
  # route_ROMA$deviation_pos_m <- round(route_ROMA$deviation_pos_m, digits = 0)
  # route_CATANIA <- readRDS(file = "route_CATANIA.rds")
  # route_SALERNO <- readRDS(file = "route_SALERNO.rds")
  # mapmatching <- readRDS(file = "matched_routes_ROMA_2019_10_09.rds")
  # save(route_BRESCIA, route_ROMA, mapmatching, file = "routes_data.RData")

  # load("routes_data.RData")
# filePaths <- filesInfo$path_display
# data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
# route_BRESCIA <- as.data.frame(data[[1]])
# route_ROMA <- as.data.frame(data[[2]])



## GET data from DROPBOX account "karafede"
drop_get(path = "/FCD_data/input_files/route_BRESCIA.rds",
         local_file = 'route_BRESCIA.rds', 
         dtoken = token, 
         overwrite = TRUE,
         progress = TRUE)
route_BRESCIA <- readRDS('route_BRESCIA.rds')
# route_BRESCIA <- readRDS('route_BRESCIA_random.rds')



drop_get(path = "/FCD_data/input_files/route_ROMA.rds",
         local_file = 'route_ROMA.rds',
         dtoken = token,
         overwrite = TRUE,
         progress = TRUE)
route_ROMA <- readRDS('route_ROMA.rds')


# drop_get(path = "/FCD_data/input_files/route_CATANIA.rds",
#          local_file = 'route_CATANIA.rds',
#          dtoken = token,
#          overwrite = TRUE,
#          progress = TRUE)
# route_CATANIA <- readRDS('route_CATANIA.rds')


drop_get(path = "/FCD_data/input_files/route_SALERNO.rds",
         local_file = 'route_SALERNO.rds',
         dtoken = token,
         overwrite = TRUE,
         progress = TRUE)
route_SALERNO <- readRDS('route_SALERNO.rds')



############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

shinyServer(function(input, output, session) {
  
  observe({
 
    
    # periodically collect garbage
    # invalidateLater(100000,session)
    cleanMem()
    
    rm(list = ls())
    gc()
    DB <- input$database
    VEHTYPE <- input$type
    START_TIME <- input$range_hours[1]
    END_TIME <- input$range_hours[2]
  
    # start_date <- '2019-10-09'
    # end_date <- '2019-10-09'
    # start_time <- '8'
    # end_time <- '9'
    
    # req(input$route_BRESCIA)
    # route_BRESCIA <- readRDS(input$route_BRESCIA$datapath)
    # req(input$route_ROMA)
    # route_ROMA <- readRDS(input$route_ROMA$datapath)
  

    # conn_HAIG <- dbConnect(drv, dbname = DB,           
    #                        host = "10.1.0.1", port = 5432,
    #                        user = "postgres", password = "superuser")
    
                    # ###------ Create a Progress object
                    # progress <- shiny::Progress$new()
                    # # Make sure it closes when we exit this reactive, even if there's an error
                    # on.exit(progress$close())
                    # progress$set(message = "Querying data...", value = 0.5)
 
                   # number of records
                   TABLE1 <- reactive({
                     
                    
                     # count_vehtype = dbGetQuery(conn_HAIG,"
                     #                SELECT vehtype, COUNT(*)
                     #                FROM public.idterm_portata 
                     #                group by vehtype ")
                     
                    gc()
                     if (DB == 'HAIG_BRESCIA') {
                       count_vehtype <- read.csv("count_vehtype_BRESCIA.csv")[-1]
                     } else if (DB == 'HAIG_ROMA') {
                       count_vehtype <- read.csv("count_vehtype_ROMA.csv")[-1]
                     } else if (DB == 'HAIG_CATANIA') {
                       count_vehtype <- read.csv("count_vehtype_CATANIA.csv")[-1]
                     } else if (DB == 'HAIG_SALERNO') {
                       count_vehtype <-  read.csv("count_vehtype_SALERNO.csv")[-1]
                     } else {print("no matched data available")}
              
                     ###---rename data
                     count_vehtype$vehtype <- gsub(1, "auto", (count_vehtype$vehtype))
                     count_vehtype$vehtype <- gsub(2, "veicoli commerciali", (count_vehtype$vehtype))
                     count_vehtype$percentage <- round(((count_vehtype$count)/sum(count_vehtype$count)*100), 2)
                     names(count_vehtype) <- c("Type of Vehicle", "number of vehicles", "percentage (%) by type")
                     # output table
                     return(count_vehtype) 
                   })
                   
                   
                   
                   
                   
                   TABLE2 <- reactive({
                     
                     gc()
                     # ###------ Create a Progress object
                     # progress <- shiny::Progress$new()
                     # # Make sure it closes when we exit this reactive, even if there's an error
                     # on.exit(progress$close())
                     # progress$set(message = "Querying data...", value = 0.5)
                     
                     
                      
                      ##----join "vehicle type" from the table "idterm_portata" (to get vehtype)
                       # route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
                       #                    route.idterm,
                       #                    route.tripdistance_m,
                       #                    route.timedate_o,
                       #                    route.breaktime_s,
                       #                    route.triptime_s,
                       #                    route.deviation_pos_m,
                       #                    idterm_portata.vehtype
                       #             FROM route
                       #             LEFT JOIN idterm_portata
                       #                  ON route.idterm = idterm_portata.idterm
                       #                  LIMIT 100000
                       #                    ")
                  
                     
                     ## load a .".csv" file... 
                     # if (DB == 'HAIG_BRESCIA') {
                     #   route <- read_csv("route_BRESCIA.csv")
                     # } else if (DB == 'HAIG_ROMA') {
                     #   route <- read_csv("route_ROMA.csv")
                     # } else if (DB == 'HAIG_CATANIA') {
                     #   route <- read_csv("route_CATANIA.csv")
                     # } else if (DB == 'HAIG_SALERNO') {
                     #   route <- read_csv("route_SALERNO.csv") 
                     # } else {print("no matched data available")}
                     
                     if (DB == 'HAIG_BRESCIA') {
                       route <- route_BRESCIA
                       # req(input$route_BRESCIA)
                       # route <- readRDS(input$route_BRESCIA$datapath)
                     } else if (DB == 'HAIG_ROMA') {
                       route <- route_ROMA
                       # req(input$route_ROMA)
                       # route <- readRDS(input$route_ROMA$datapath)
                     } else if (DB == 'HAIG_CATANIA') {
                       route <- route_CATANIA
                     } else if (DB == 'HAIG_SALERNO') {
                       route <- route_SALERNO
                     } else {print("no matched data available")}
                     
                     route$vehtype <- gsub(1, "auto", (route$vehtype))
                     route$vehtype <- gsub(2, "veicoli commerciali", (route$vehtype))
                     route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                    
                     route <- route %>%
                       filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                                triptime_s > 60 & triptime_s < 20500 & 
                                speed_trip < 150 & speed_trip > 15 &
                                breaktime_s > 60 & breaktime_s < 100000) 
                     
                       route$breaktime_m <- round((route$breaktime_s)/60, digits = 1)
                       ## convert triptime_s into minutes
                       route$triptime_m <- round((route$triptime_s)/60, digits = 1)
                       
                       route <- route %>%
                         dplyr::select(idtrajectory,
                                idterm,
                                tripdistance_m,
                                timedate_o,
                                breaktime_m,
                                triptime_m,
                                deviation_pos_m,
                                vehtype,
                                speed_trip)
                       
                       names(route) <- c("idtrajectory", "idterm", "trip distance (meter)", "timestamp",
                                               "stop time (min)", "trip duration (min)",
                                               "offset position (meters)", "type of vehicle", "speed (km/h)")
                       return(route[1:30,])
                   })
                   
                   
                   
        
                   ###------ make plots-------###################################
            
                    output$bar_offset_position <- renderPlot({   ## user "renderPlotly" fo make iterative plot
                      
                      gc()
                    # if (DB == 'HAIG_BRESCIA') {
                    #   route <- read_csv("route_BRESCIA.csv")
                    # } else {print("no matched data available")}
                      
                     ## ----- plot deviation (or offset) of the position
                     # route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
                     #                      route.deviation_pos_m
                     #                      FROM route ")
                     
                      # ## load a .".csv" file... 
                      # if (DB == 'HAIG_BRESCIA') {
                      #   route <- read_csv("route_BRESCIA.csv")
                      # } else if (DB == 'HAIG_ROMA') {
                      #   route <- read_csv("route_ROMA.csv")
                      # } else if (DB == 'HAIG_CATANIA') {
                      #   route <- read_csv("route_CATANIA.csv")
                      # } else if (DB == 'HAIG_SALERNO') {
                      #   route <- read_csv("route_SALERNO.csv") 
                      # } else {print("no matched data available")}
                      
                      if (DB == 'HAIG_BRESCIA') {
                        route <- route_BRESCIA
                        # req(input$route_BRESCIA)
                        # route <- readRDS(input$route_BRESCIA$datapath)
                      } else if (DB == 'HAIG_ROMA') {
                        route <- route_ROMA
                        # req(input$route_ROMA)
                        # route <- readRDS(input$route_ROMA$datapath)
                      } else if (DB == 'HAIG_CATANIA') {
                        route <- route_CATANIA
                      } else if (DB == 'HAIG_SALERNO') {
                        route <- route_SALERNO
                      } else {print("no matched data available")}
                      
                      route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                      route <- route %>%
                        filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                                 triptime_s > 60 & triptime_s < 20500 & 
                                 speed_trip < 150 & speed_trip > 15 &
                                 breaktime_s > 60 & breaktime_s < 100000) 
                     
                     BBB <- NULL
                     bin_dev = c(5,10,40,100,1000)  ## deviations bins in meters
                     
                     for (i in 1:length(bin_dev)){
                       AAA <- route %>%
                         filter(deviation_pos_m >= bin_dev[i]) %>%
                         summarise(counts = length(idtrajectory))
                       BBB <- rbind(BBB, AAA)
                     }
                     
                     
                     deviations <- as.data.frame(cbind(bin_dev, BBB))
                     deviations <- na.omit(deviations)
                     deviations$N_tot_trips <- sum(deviations$counts)
                     ## calculate percentage of trips by deviation distance
                     deviations$perc_count <- round((deviations$counts/deviations$N_tot_trips)*100, digits = 1)
                     
                     deviations$bin_dev <- as.factor(deviations$bin_dev)
                     rm(AAA, BBB, route)
                     gc()
                  
                     Figure_bar_offset_position <-  ggplot(data = deviations,
                                                       aes(bin_dev, perc_count, fill = bin_dev)) + guides(fill=FALSE) +
                                                       geom_bar(stat = "identity") + 
                                                       guides(fill=FALSE) +
                                                       # ylim(0, 90) +
                                                       theme_bw() +
                                                       theme( strip.text = element_text(size = 13)) +
                                                       theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
                                                       theme(axis.title.x = element_blank()) +                  # Remove x-axis label
                                                       xlab("distance (meters)") +
                                                       ylab("frequency (%)") +
                                                       theme(axis.title.y = element_text(face="bold", colour="black", size=14),
                                                             axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
                                                       theme(axis.title.x = element_text(face="bold", colour="black", size=14),
                                                             axis.text.x  = element_text(angle=0, vjust=0.5, size=14, hjust = 0.5)) + 
                                                       # scale_x_continuous(breaks=c(0, 5, 10, 15, 20)) +
                                                       ggtitle("Bias larger than 5, 10, 40, 100 and 1000 meters") + 
                                                       theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                  
                     return(Figure_bar_offset_position)
                     gc()
                   },  height = function() {
                     if (session$clientData$output_bar_offset_position_width <= 1000) {
                       (session$clientData$output_bar_offset_position_width)*(3/4)
                     } else { (session$clientData$output_bar_offset_position_width)*(7/16) }
                   })
                   
                   ## to also scale down the labels a little, 
                   ## add another parameter res = log(session$clientData$output_plot_width/30)*24 to the renderPlot function call
  
                   
                   
                ### ------------ ############ 
                ## bias position (end - start new trip)
                 output$plot_offset_position <- renderPlot({
               
                   rm(list = ls())
                   gc()
                     
                     ## ----- plot deviation (or offset) of the position
                     # route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
                     #                      route.deviation_pos_m
                     #                      FROM route ")
                     
                     
                     if (DB == 'HAIG_BRESCIA') {
                       route <- route_BRESCIA
                       # req(input$route_BRESCIA)
                       # route <- readRDS(input$route_BRESCIA$datapath)
                     } else if (DB == 'HAIG_ROMA') {
                       route <- route_ROMA
                       # req(input$route_ROMA)
                       # route <- readRDS(input$route_ROMA$datapath)
                     } else if (DB == 'HAIG_CATANIA') {
                       route <- route_CATANIA
                     } else if (DB == 'HAIG_SALERNO') {
                       route <- route_SALERNO
                     } else {print("no matched data available")}
                     
                     route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                     route <- route %>%
                       filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                                triptime_s > 60 & triptime_s < 20500 & 
                                speed_trip < 150 & speed_trip > 15 &
                                breaktime_s > 60 & breaktime_s < 100000) 
                     
                     ### distribution of the bias between the end position of a trip and the starting position o a new trip
                     ### plot a distribution
                     Figure_offset_position <- ggplot(route, aes(x =(deviation_pos_m))) +
                                         theme_bw() +
                                         geom_density(stat = 'bin') +
                                         scale_x_continuous(trans='log10', breaks=c(0, 5, 10, 50, 100, 1000, 5000)) +
                                         # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 30, 40, 50, 100, 500, 1000, 2000, 5000)) +
                                         theme(legend.title=element_blank()) + 
                                         aes(y=stat(count)/sum(stat(count))) + 
                                         scale_y_continuous(labels = scales::percent) +
                                         theme_bw() +
                                         theme( strip.text = element_text(size = 10)) +
                                         guides(fill=FALSE) +
                                         theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, size=13)) +
                                         theme(axis.text.x=element_text(size=13, colour = "black")) +
                                         theme(axis.title.x = element_text(face="bold", colour="black", size=13)) +
                                         xlab("distance (meters)") +
                                         ylab("frequency (%)") +
                                         # xlim(0,100)+
                                         geom_vline(xintercept = 5, col="black", lty=2, size=1) +
                                         geom_vline(xintercept = 10, col="red", lty=2, size=1) +
                                         geom_vline(xintercept = 100, col="blue", lty=2, size=1) +
                                         # geom_text(aes(x = 12 , y = 0.15 , label = "10 m"), size = 4) +
                                         # geom_text(aes(x = 100 , y = 0.15 , label = "150 m"), size = 4) +
                                         theme(axis.title.y = element_text(face="bold", colour="black", size=13),
                                               axis.text.y  = element_text(angle=0, vjust=0.5, size=15, colour="black")) +
                                         ggtitle("Bias between end-start of a new trip") +
                                         theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                     
                     return(Figure_offset_position)
                     gc()
                   }, height = function() {
                     if (session$clientData$output_plot_offset_position_width <= 1000) {
                       (session$clientData$output_plot_offset_position_width)*(3/4)
                     } else { (session$clientData$output_plot_offset_position_width)*(7/16) }
                   })
                   
                   
                   
                   
                   output$plot_hourly_trips_profile <- renderPlot({
                     
                     gc()
                     # ## load a .".csv" file... 
                     # if (DB == 'HAIG_BRESCIA') {
                     #   route <- read_csv("route_BRESCIA.csv")
                     # } else if (DB == 'HAIG_ROMA') {
                     #   route <- read_csv("route_ROMA.csv")
                     # } else if (DB == 'HAIG_CATANIA') {
                     #   route <- read_csv("route_CATANIA.csv")
                     # } else if (DB == 'HAIG_SALERNO') {
                     #   route <- read_csv("route_SALERNO.csv") 
                     # } else {print("no matched data available")}
                     
                     if (DB == 'HAIG_BRESCIA') {
                       route <- route_BRESCIA
                       # req(input$route_BRESCIA)
                       # route <- readRDS(input$route_BRESCIA$datapath)
                     } else if (DB == 'HAIG_ROMA') {
                       route <- route_ROMA
                       # req(input$route_ROMA)
                       # route <- readRDS(input$route_ROMA$datapath)
                     } else if (DB == 'HAIG_CATANIA') {
                       route <- route_CATANIA
                     } else if (DB == 'HAIG_SALERNO') {
                       route <- route_SALERNO
                     } else {print("no matched data available")}
                     
                     route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                     route$hour <- hour(route$timedate_o)
                     route$day <- weekdays(as.Date(route$timedate_o, tz = "CET"))
                     route$month <- month(as.Date(route$timedate_o, tz = "CET"))
                     # route$month <- month(as.Date(route$timedate_o))
                     
                     route <- route %>%
                       filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                                triptime_s > 60 & triptime_s < 20500 & 
                                speed_trip < 200 & speed_trip > 15 &
                                breaktime_s > 60 & breaktime_s < 100000)
                     
                     
                     ## summary statistics by DAY of the WEEK #############
                     ## assign name to month
                     route$month <- as.factor(route$month)
                     route$month <- gsub(2, "February", (route$month))
                     route$month <- gsub(3, "March", (route$month))
                     route$month <- gsub(5, "May", (route$month))
                     route$month <- gsub(8, "August", (route$month))
                     route$month <- gsub(9, "September", (route$month))
                     route$month <- gsub(10, "October", (route$month))
                     route$month <- gsub(11, "November", (route$month))
                     
                     ### summary statistics by HOUR
                     ### find the number trips for each hour and travelled distance
                     trips_hour <- route %>%
                       group_by(month, hour) %>%
                       summarise(count = length(idtrajectory),
                                 total_km = sum(tripdistance_m/1000)) 
                     
                    AAA <- trips_hour %>%
                      group_by(month) %>%
                      summarise(count = length(hour))
                    month_to_remove <- AAA$month[AAA$count < 24]
                    
                    trips_hour <- trips_hour %>%
                      filter(!month %in% month_to_remove)
                    
                    route <- route %>%
                      filter(!month %in% month_to_remove)
                     
                     
                     ## find the number of trip travelled each months and their distance
                     trips_month <- route %>%
                       group_by(month) %>%
                       summarise(N_trip_month = length(idtrajectory),
                                 total_km_month = sum(tripdistance_m/1000))
                     
                     
                     ## merge above data
                     trips_hour <- trips_hour %>%
                       left_join(trips_month, by = c("month"))
                     
                     
                     trips_hour <- trips_hour %>%
                       group_by(month, hour) %>%
                       summarise(perc_count = round( (count/N_trip_month)*100, digits = 1),
                                 perc_km = round( (total_km/total_km_month)*100, digits = 1)) 
                     
                     
                     trips_hour <- as.data.frame(trips_hour)
                     
                     
                     Figure_hourly_trips <- ggplot(data = trips_hour,
                                 aes(hour, perc_count, fill = hour)) + guides(fill=FALSE) +
                       geom_bar(stat = "identity") + 
                       facet_wrap( ~ month, scales = "free_y") +
                       guides(fill=FALSE) +
                       ylim(0, 10) +
                       theme_bw() +
                       theme( strip.text = element_text(size = 15)) +
                       theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
                       theme(axis.text.x=element_text(size=11,face="bold", colour = "black")) +
                       theme(axis.title.x = element_blank()) +                  # Remove x-axis label
                       ylab("(%)") +            # Set y-axis label
                       theme(axis.title.y = element_text(face="bold", colour="black", size=14),
                             axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
                       xlab("") +            # Set y-axis label
                       theme(axis.title.x = element_text(face="bold", colour="black", size=12),
                             axis.text.x  = element_text(angle=0, vjust=0.5, hjust=0.5, size=9)) +
                       scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
                       # scale_x_continuous(breaks=c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
                       # geom_text(aes(label = paste(count, sep = "")), size = 5, hjust = 0.5, vjust = -0.5) +
                       ggtitle("Hourly daily trip percentage") + 
                       theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                 
                     return(Figure_hourly_trips)
                   })
                   
                   
                   
                   output$plot_hourly_distance_profile <- renderPlot({
                     
                     gc()
                     if (DB == 'HAIG_BRESCIA') {
                       route <- route_BRESCIA
                       # req(input$route_BRESCIA)
                       # route <- readRDS(input$route_BRESCIA$datapath)
                     } else if (DB == 'HAIG_ROMA') {
                       route <- route_ROMA
                       # req(input$route_ROMA)
                       # route <- readRDS(input$route_ROMA$datapath)
                     } else if (DB == 'HAIG_CATANIA') {
                       route <- route_CATANIA
                     } else if (DB == 'HAIG_SALERNO') {
                       route <- route_SALERNO
                     } else {print("no matched data available")}
                     
                     route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                     route$hour <- hour(route$timedate_o)
                     route$day <- weekdays(as.Date(route$timedate_o, tz = "CET"))
                     route$month <- month(as.Date(route$timedate_o, tz = "CET"))
                     # route$month <- month(as.Date(route$timedate_o))
                     
                     route <- route %>%
                       filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                                triptime_s > 60 & triptime_s < 20500 & 
                                speed_trip < 200 & speed_trip > 15 &
                                breaktime_s > 60 & breaktime_s < 100000)
                     
                     
                     ## summary statistics by DAY of the WEEK #############
                     ## assign name to month
                     route$month <- as.factor(route$month)
                     route$month <- gsub(2, "February", (route$month))
                     route$month <- gsub(3, "March", (route$month))
                     route$month <- gsub(5, "May", (route$month))
                     route$month <- gsub(8, "August", (route$month))
                     route$month <- gsub(9, "September", (route$month))
                     route$month <- gsub(10, "October", (route$month))
                     route$month <- gsub(11, "November", (route$month))
                     
                     
                     ### summary statistics by HOUR
                     ### find the number trips for each hour and travelled distance
                     trips_hour <- route %>%
                       group_by(month, hour) %>%
                       summarise(count = length(idtrajectory),
                                 total_km = sum(tripdistance_m/1000)) 
                     
                     AAA <- trips_hour %>%
                       group_by(month) %>%
                       summarise(count = length(hour))
                     month_to_remove <- AAA$month[AAA$count < 24]
                     
                     trips_hour <- trips_hour %>%
                       filter(!month %in% month_to_remove)
                     
                     route <- route %>%
                       filter(!month %in% month_to_remove)
                     
                     
                     ## find the number of trip travelled each months and their distance
                     trips_month <- route %>%
                       group_by(month) %>%
                       summarise(N_trip_month = length(idtrajectory),
                                 total_km_month = sum(tripdistance_m/1000))
                     
                     
                     ## merge above data
                     trips_hour <- trips_hour %>%
                       left_join(trips_month, by = c("month"))
                     
                     
                     trips_hour <- trips_hour %>%
                       group_by(month, hour) %>%
                       summarise(perc_count = round( (count/N_trip_month)*100, digits = 1),
                                 perc_km = round( (total_km/total_km_month)*100, digits = 1)) 
                     
                     
                     trips_hour <- as.data.frame(trips_hour)
                     remove(trips_month)
                     
                     
                     Figure_hourly_distances <- ggplot(data = trips_hour,
                                                   aes(hour, perc_km, fill = hour)) + guides(fill=FALSE) +
                       geom_bar(stat = "identity") + 
                       facet_wrap( ~ month, scales = "free_y") +
                       guides(fill=FALSE) +
                       ylim(0, 10) +
                       theme_bw() +
                       theme( strip.text = element_text(size = 15)) +
                       theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
                       theme(axis.text.x=element_text(size=11,face="bold", colour = "black")) +
                       theme(axis.title.x = element_blank()) +                  # Remove x-axis label
                       ylab("(%)") +            # Set y-axis label
                       theme(axis.title.y = element_text(face="bold", colour="black", size=14),
                             axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
                       xlab("") +            # Set y-axis label
                       theme(axis.title.x = element_text(face="bold", colour="black", size=12),
                             axis.text.x  = element_text(angle=0, vjust=0.5, hjust=0.5, size=9)) +
                       scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
                       # scale_x_continuous(breaks=c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
                       # geom_text(aes(label = paste(count, sep = "")), size = 5, hjust = 0.5, vjust = -0.5) +
                       ggtitle("Hourly daily trip distance") + 
                       theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                     
                     return(Figure_hourly_distances)
                   })
                   
                   
                   
                 ### ------------ #####   
                 ### mean speed #######
                 output$plot_average_speed <- renderPlot({
                     
                   gc()
                     
                     ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                     # route = dbGetQuery(conn_HAIG, "SELECT route.tripdistance_m,
                     #                                       route.triptime_s
                     #                                       FROM route
                     #                                       ")
                     
                     if (DB == 'HAIG_BRESCIA') {
                       route <- route_BRESCIA
                     } else if (DB == 'HAIG_ROMA') {
                       route <- route_ROMA
                     } else if (DB == 'HAIG_CATANIA') {
                       route <- route_CATANIA
                     } else if (DB == 'HAIG_SALERNO') {
                       route <- route_SALERNO
                     } else {print("no matched data available")}
     
                     route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                     route <- route %>%
                       filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                                triptime_s > 60 & triptime_s < 20500 & 
                                breaktime_s > 60 & breaktime_s < 100000) 
                   
                     ### distribution of the bias between the end position of a trip and the starting position o a new trip
                     ### plot a distribution
                     
                     ## distribution of the speeds ###
                     ### plot a distribution
                     Figure_mean_speed <- ggplot(route, aes(x = speed_trip)) +
                               theme_bw() +
                               geom_density(stat = 'bin') +
                               scale_x_continuous(trans='log10', breaks=c(0, 10, 20, 30, 50, 100)) +
                               theme(legend.title=element_blank()) + 
                               aes(y=stat(count)/sum(stat(count))) + 
                               scale_y_continuous(labels = scales::percent) +
                               theme_bw() +
                               theme( strip.text = element_text(size = 10)) +
                               guides(fill=FALSE) +
                               theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, size=10)) +
                               theme(axis.text.x=element_text(size=10, colour = "black")) +
                               theme(axis.title.x = element_text(face="bold", colour="black", size=10)) +
                               geom_vline(xintercept = 20, col="black", lty=2, size=1) +
                               geom_vline(xintercept = 30, col="blue", lty=2, size=1) +
                               geom_vline(xintercept = 50, col="red", lty=2, size=1) +
                               xlab("speed (km/h)") +
                               ylab("frequency (%)") +
                               # geom_vline(xintercept = 30, col="red", lty=2, size=1) +
                               theme(axis.title.y = element_text(face="bold", colour="black", size=10),
                                     axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
                               ggtitle("distribution of mean speed during the trips") +
                               theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                             
                     return(Figure_mean_speed)
                   })

                   
                   
                   output$plot_trips_speed <- renderPlot({
                     
                     gc()
                     
                     if (DB == 'HAIG_BRESCIA') {
                       route <- route_BRESCIA
                     } else if (DB == 'HAIG_ROMA') {
                       route <- route_ROMA
                     } else if (DB == 'HAIG_CATANIA') {
                       route <- route_CATANIA
                     } else if (DB == 'HAIG_SALERNO') {
                       route <- route_SALERNO
                     } else {print("no matched data available")}
                     
                     route$vehtype <- gsub(1, "auto", (route$vehtype))
                     route$vehtype <- gsub(2, "veicoli commerciali", (route$vehtype))
                     route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                     route <- route %>%
                       filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                                triptime_s > 60 & triptime_s < 20500 & 
                                speed_trip < 220 & speed_trip > 15 &
                                breaktime_s > 60 & breaktime_s < 100000) 
                     
                     
                     ### auto ###
                     VIAGGI <- NULL
                     bin_speed = seq(from = 0, to = 140, by =10)
                     
                     for (i in 1:length(bin_speed)){
                       #i=5
                       if (i==1){
                         viaggi <- route %>%
                           filter(vehtype == "auto") %>%
                           filter(speed_trip <= bin_speed[i])  %>%
                           summarise(counts = length(idtrajectory))
                       }else{
                         viaggi <- route %>%
                           filter(vehtype == "auto") %>%
                           filter(speed_trip <= bin_speed[i] & speed_trip >= bin_speed[i-1]) %>%
                           summarise(counts = length(idtrajectory))
                         
                       }
                       VIAGGI<- rbind(VIAGGI, viaggi)
                    }
                     
                     viaggi_10kmh_auto <- as.data.frame(cbind(bin_speed, VIAGGI))
                     viaggi_10kmh_auto <- na.omit(viaggi_10kmh_auto)
                     viaggi_10kmh_auto$vehtype <- "auto"
                     viaggi_10kmh_auto$N_tot_trips <- sum(viaggi_10kmh_auto$counts)
                     
                     
                     ### veicoli commerciali ###
                     VIAGGI <- NULL
                     bin_speed = seq(from = 0, to = 140, by =10)
                     
                     for (i in 1:length(bin_speed)){
                       #i=5
                       if (i==1){
                         viaggi <- route %>%
                           filter(vehtype == "veicoli commerciali") %>%
                           filter(speed_trip <= bin_speed[i])  %>%
                           summarise(counts = length(idtrajectory))
                       }else{
                         viaggi <- route %>%
                           filter(vehtype == "veicoli commerciali") %>%
                           filter(speed_trip <= bin_speed[i] & speed_trip >= bin_speed[i-1]) %>%
                           summarise(counts = length(idtrajectory))
                         
                       }
                       VIAGGI<- rbind(VIAGGI, viaggi)
                      }
                     
                     viaggi_10kmh_commerciali <- as.data.frame(cbind(bin_speed, VIAGGI))
                     viaggi_10kmh_commerciali <- na.omit(viaggi_10kmh_commerciali)
                     viaggi_10kmh_commerciali$vehtype <- "veicoli commerciali"
                     viaggi_10kmh_commerciali$N_tot_trips <- sum(viaggi_10kmh_commerciali$counts)
                     
                     viaggi_10kmh <- rbind(viaggi_10kmh_auto,
                                           viaggi_10kmh_commerciali)
                     ## calculate percentage of trips by vehtype
                     viaggi_10kmh$perc_count <- round((viaggi_10kmh$counts/viaggi_10kmh$N_tot_trips)*100, digits = 0)
                     rm(VIAGGI, viaggi, viaggi_10kmh_auto, viaggi_10kmh_commerciali, route)
                     gc()
                         
                         
                         Figure_trips_speed <- ggplot(data = viaggi_10kmh,
                                                      aes(bin_speed, perc_count, fill = bin_speed)) + guides(fill=FALSE) +
                           geom_bar(stat = "identity") + 
                           guides(fill=FALSE) +
                           facet_wrap(~vehtype) +
                           theme_bw() +
                           theme( strip.text = element_text(size = 15)) +
                           theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
                           theme(axis.text.x=element_text(size=13,face="bold", colour = "black")) +
                           theme(axis.title.x = element_blank()) +                
                           xlab("speed (km/h)") +
                           ylab("percentage of trips") +
                           theme(axis.title.y = element_text(face="bold", colour="black", size=15),
                                 axis.text.y  = element_text(angle=0, vjust=0.5, size=15)) +
                           theme(axis.title.x = element_text(face="bold", colour="black", size=13),
                                 axis.text.x  = element_text(angle=0, vjust=0.5, size=10, hjust = 0.5)) + 
                           # scale_x_continuous(breaks=c(0, 5, 10, 15, 20)) +
                           ggtitle("Percentage of trips per mean speed trip") + 
                           theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                         
                         return(Figure_trips_speed)
                   })
                         
                 
                 ### stop time (breaktime_m)
                 output$plot_stop_time <- renderPlot({
                   
                   gc()
                   ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                   # route = dbGetQuery(conn_HAIG, "SELECT route.breaktime_s,
                   #                                        triptime_s,
                   #                                        tripdistance_m
                   #                                         FROM route
                   #                                         ")
                   
                   
                   # route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
                   #                        route.idterm,
                   #                        route.tripdistance_m,
                   #                        route.timedate_o,
                   #                        route.breaktime_s,
                   #                        route.triptime_s,
                   #                        route.deviation_pos_m,
                   #                        idterm_portata.vehtype
                   #                 FROM route
                   #                 LEFT JOIN idterm_portata
                   #                      ON route.idterm = idterm_portata.idterm
                   #                      ")
                   
                   
                   if (DB == 'HAIG_BRESCIA') {
                     route <- route_BRESCIA
                   } else if (DB == 'HAIG_ROMA') {
                     route <- route_ROMA
                   } else if (DB == 'HAIG_CATANIA') {
                     route <- route_CATANIA
                   } else if (DB == 'HAIG_SALERNO') {
                     route <- route_SALERNO
                   } else {print("no matched data available")}
                   
                   route$vehtype <- gsub(1, "auto", (route$vehtype))
                   route$vehtype <- gsub(2, "veicoli commerciali", (route$vehtype))
                   route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                   route <- route %>%
                     filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                              triptime_s > 60 & triptime_s < 20500 & 
                              breaktime_s > 60 & breaktime_s < 100000) 
                   
                   route <- route[complete.cases(route), ]
                   
                   route <- route %>%
                     filter(speed_trip > 0 & speed_trip < 220)
                   
                   
                   ##----- convert breaktime_s into minutes
                   route$breaktime_m <- (route$breaktime_s)/60
                
                   ## distribution of the stop times ###
                   ### plot a distribution
                   
                   Figure_stop_time <- ggplot() + 
                                 geom_density(data=route, aes(x = breaktime_m, group=vehtype, color=vehtype), adjust=2) +
                                 scale_x_continuous(trans='log10', breaks=c(3, 10, 50, 780)) +
                                 theme(legend.title=element_blank()) + 
                                 # aes(y=stat(count)/sum(stat(count))) + 
                                 scale_y_continuous(labels = scales::percent) +
                                 theme_bw() +
                                 theme( strip.text = element_text(size = 10)) +
                                 guides(fill=FALSE) +
                                 theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, size=10)) +
                                 theme(axis.text.x=element_text(size=10, colour = "black")) +
                                 theme(axis.title.x = element_text(face="bold", colour="black", size=10)) +
                                 geom_vline(xintercept = 3, col="gray", lty=2, size=0.5) +
                                 geom_vline(xintercept = 10, col="gray", lty=2, size=0.5) +
                                 geom_vline(xintercept = 70, col="gray", lty=2, size=0.5) +
                                 geom_vline(xintercept = 250, col="gray", lty=2, size=0.5) +
                                 geom_vline(xintercept = 730, col="gray", lty=2, size=0.5) +
                                 xlab("tempo di sosta (minuti)") +
                                 ylab("density") +
                                 theme(axis.title.y = element_text(face="bold", colour="black", size=10),
                                       axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
                                 ggtitle("distribuzione dei tempi di sosta") +
                                 theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                  
                   return(Figure_stop_time)
                 })
                 
                 
                 ##### --------------------------------------- #####
                 ### trip distance (tripdistance_m)  (in km) #######
                 output$plot_trip_distance <- renderPlot({
                   
                   gc()
                   ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                   # route = dbGetQuery(conn_HAIG, "SELECT triptime_s,
                   #                                        tripdistance_m
                   #                                         FROM route
                   #                                         ")
                   
                   if (DB == 'HAIG_BRESCIA') {
                     route <- route_BRESCIA
                   } else if (DB == 'HAIG_ROMA') {
                     route <- route_ROMA
                   } else if (DB == 'HAIG_CATANIA') {
                     route <- route_CATANIA
                   } else if (DB == 'HAIG_SALERNO') {
                     route <- route_SALERNO
                   } else {print("no matched data available")}
                   
                   route$vehtype <- gsub(1, "auto", (route$vehtype))
                   route$vehtype <- gsub(2, "veicoli commerciali", (route$vehtype))
                   route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 1)  ## km/h
                   
                   route <- route %>%
                     filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                              triptime_s > 60 & triptime_s < 20500 & 
                              # speed_trip < 150 & speed_trip > 15 &
                              breaktime_s > 60 & breaktime_s < 100000) 
                   
                   
                   route <- route[complete.cases(route), ]
                   ## speed
                   # route$speed_trip = (route$tripdistance_m/1000)/(route$triptime_s/3600)  ## km/h
                   route <- route %>%
                     filter(speed_trip > 0 & speed_trip < 240)
                   
                   ### transform travelled sitance into km
                   route$tripdistance_km <- round(route$tripdistance_m/1000, digits = 1)
                   
                   
                   ## distribution of the travelled distances ###
                   ### plot a distribution
                   Figure_trip_distance <- ggplot(route, aes(x = tripdistance_km)) +
                     theme_bw() +
                     geom_density(stat = 'bin') +
                     scale_x_continuous(trans='log10', breaks=c(1, 2, 5, 10, 20, 50)) +
                     theme(legend.title=element_blank()) + 
                     aes(y=stat(count)/sum(stat(count))) + 
                     scale_y_continuous(labels = scales::percent) +
                     theme_bw() +
                     theme( strip.text = element_text(size = 10)) +
                     guides(fill=FALSE) +
                     theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, size=10)) +
                     theme(axis.text.x=element_text(size=10, colour = "black")) +
                     theme(axis.title.x = element_text(face="bold", colour="black", size=10)) +
                     geom_vline(xintercept = 1, col="blue", lty=2, size=0.5) +
                     geom_vline(xintercept = 5, col="blue", lty=2, size=0.5) +
                     geom_vline(xintercept = 10, col="blue", lty=2, size=0.5) +
                     xlab("distanza (km)") +
                     ylab("frequenza (%)") +
                     theme(axis.title.y = element_text(face="bold", colour="black", size=10),
                           axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
                     ggtitle("distribuzione della distanza percorsa per viaggio") +
                     theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                 
                   return(Figure_trip_distance)
                 })
                 
                 
                 
                 #### ---------------------------------------##########
                 ### trip times (triptime_m) (in minutes) #############
                 output$plot_trip_time <- renderPlot({
                   
                   gc()
                   ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                   # route = dbGetQuery(conn_HAIG, "SELECT triptime_s,
                   #                                        tripdistance_m
                   #                                         FROM route
                   #                                         ")
                 
                   if (DB == 'HAIG_BRESCIA') {
                     route <- route_BRESCIA
                   } else if (DB == 'HAIG_ROMA') {
                     route <- route_ROMA
                   } else if (DB == 'HAIG_CATANIA') {
                     route <- route_CATANIA
                   } else if (DB == 'HAIG_SALERNO') {
                     route <- route_SALERNO
                   } else {print("no matched data available")}
                   
                   route$vehtype <- gsub(1, "auto", (route$vehtype))
                   route$vehtype <- gsub(2, "veicoli commerciali", (route$vehtype))
                   route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                   
                   route <- route %>%
                     filter(tripdistance_m > 0 & tripdistance_m < 150000 &
                              triptime_s > 60 & triptime_s < 20500 & 
                              # speed_trip < 150 & speed_trip > 15 &
                              breaktime_s > 60 & breaktime_s < 100000) 
                   
                   route <- route[complete.cases(route), ]
                   ## speed
                   route <- route %>%
                     filter(speed_trip > 0 & speed_trip < 240)
                   

                   ##---- convert triptime_s into minutes
                   route$triptime_m <- (route$triptime_s)/60
                   
                   
                   ## distribution of the trip time ###
                   ### plot a distribution
                   Figure_trip_time <- ggplot(route, aes(x = triptime_m)) +
                     theme_bw() +
                     geom_density(stat = 'bin') +
                     scale_x_continuous(trans='log10', breaks=c(5, 10, 15, 20, 30, 50)) +
                     theme(legend.title=element_blank()) + 
                     aes(y=stat(count)/sum(stat(count))) + 
                     scale_y_continuous(labels = scales::percent) +
                     theme_bw() +
                     theme( strip.text = element_text(size = 10)) +
                     guides(fill=FALSE) +
                     theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, size=10)) +
                     theme(axis.text.x=element_text(size=10, colour = "black")) +
                     theme(axis.title.x = element_text(face="bold", colour="black", size=10)) +
                     geom_vline(xintercept = 10, col="black", lty=2, size=0.5) +
                     geom_vline(xintercept = 15, col="blue", lty=2, size=0.5) +
                     geom_vline(xintercept = 30, col="red", lty=2, size=0.5) +
                     xlab("tempo di viaggio (minuti)") +
                     ylab("frequenza (%)") +
                     theme(axis.title.y = element_text(face="bold", colour="black", size=10),
                           axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
                     ggtitle("distribuzione dei tempi di viaggio") +
                     theme(plot.title = element_text(lineheight=.8, face="bold", size = 14))
                   
                   return(Figure_trip_time)
                 })
                 
                 
                 ## Map------------------------------------------------------------------------------------------   
                 
                 finalMap <- reactive({
                  
                   if (DB == 'HAIG_ROMA') {
                     
                     # mapmatching <- read_csv("matched_routes_ROMA_2019_10_09.csv")[-1]
                     # saveRDS(mapmatching, file = "matched_routes_ROMA_2019_10_09.rds")
                     
                     # mapmatching <- readRDS(file = "matched_routes_ROMA_2019_10_09.rds")
                     # req(input$matched_routes_ROMA)
                     # mapmatching <- readRDS(input$matched_routes_ROMA$datapath)
                     
                     drop_get(path = "/FCD_data/input_files/matched_routes_ROMA_2019_10_09.rds",
                              local_file = 'matched_routes_ROMA_2019_10_09.rds',
                              dtoken = token,
                              overwrite = TRUE,
                              progress = TRUE)
                     mapmatching_ROMA <- readRDS('matched_routes_ROMA_2019_10_09.rds')
                     mapmatching <- mapmatching_ROMA
                     
                   } else if (DB == 'HAIG_CATANIA') {
                     
                     drop_get(path = "/FCD_data/input_files/matched_routes_CATANIA_2019_02_14.rds",
                              local_file = 'matched_routes_CATANIA_2019_02_14.rds',
                              dtoken = token,
                              overwrite = TRUE,
                              progress = TRUE)
                     mapmatching_CATANIA <- readRDS('matched_routes_CATANIA_2019_02_14.rds')
                     mapmatching <- mapmatching_CATANIA
                     
                     
                   } else if (DB == 'HAIG_SALERNO') {
                     
                     drop_get(path = "/FCD_data/input_files/matched_routes_SALERNO_2019_09_19.rds",
                              local_file = 'matched_routes_SALERNO_2019_09_19.rds',
                              dtoken = token,
                              overwrite = TRUE,
                              progress = TRUE)
                     mapmatching_SALERNO <- readRDS('matched_routes_SALERNO_2019_09_19.rds')
                     mapmatching <- mapmatching_SALERNO
                     
                     
                     
                   } else if (DB == 'HAIG_BRESCIA') {
                     
                     drop_get(path = "/FCD_data/input_files/matched_routes_BRESCIA_UNIBS_2019_03_15.rds",
                              local_file = 'matched_routes_BRESCIA_UNIBS_2019_03_15.rds',
                              dtoken = token,
                              overwrite = TRUE,
                              progress = TRUE)
                     mapmatching_BRESCIA <- readRDS('matched_routes_BRESCIA_UNIBS_2019_03_15.rds')
                     mapmatching <- mapmatching_BRESCIA
                     VEHTYPE <- "1"
                     
                   } else {print("no matched data available")}
        
                   mapmatching <- mapmatching %>%
                     select(hr,
                            day,
                            counts,
                            u,v,
                            vehtype,
                            name,
                            `load(%)`,
                            geometry)
                   ### normalize counts to 1
                   # mapmatching$counts <- (mapmatching$counts / max(mapmatching$counts))*35
                   
                  
                   
                   ### get a column only for the date
                   # mapmatching <- mapmatching %>%
                   #   mutate(date = date(ymd_hms(hour_date)))
                   ### get a column only for the time
                   # mapmatching$time <- as_hms(mapmatching$hour_date)
                   # mapmatching$time <- format(as.POSIXct(mapmatching$hour_date), "%H:%M:%S")
                   
                   if (VEHTYPE == '1') {
                   df_mapmatching <- mapmatching %>%
                     # filter(date > format(input$start) & date < format(input$end)
                     # filter(day >= start_date & day <= end_date) %>%
                     filter(hr >= START_TIME & hr <= END_TIME) %>%
                     filter(vehtype == VEHTYPE) %>%
                     filter(`load(%)` >= 5)
                     df_mapmatching$counts <- (df_mapmatching$counts / max(df_mapmatching$counts))*2
                   
                     
                   } else if (VEHTYPE == '2') {
                     df_mapmatching <- mapmatching %>%
                       # filter(date > format(input$start) & date < format(input$end)
                       # filter(day >= start_date & day <= end_date) %>%
                       filter(hr >= START_TIME & hr <= END_TIME) %>%
                       filter(vehtype == VEHTYPE) 
                       df_mapmatching$counts <- (df_mapmatching$counts / max(df_mapmatching$counts))*3
                   } 
                   
                   df_mapmatching <- df_mapmatching %>%
                     group_by(u,v, name, geometry) %>%
                     summarise(counts = sum(counts),
                               `load(%)` = max(`load(%)`))
              
                   
                   ## convert dataframe into Spatial Object
                   sf <- sf::st_as_sf(df_mapmatching, wkt = "geometry" )
                   # plot(sf)
                   
                   ## convert spatial object into SpatialLines
                   edges <- as_Spatial(sf$geometry)
                   
                   # filter coordinates by date
                   
                   # if (input$name != "All") {
                   #   coord_HE <- filter(table, Name == input$name)
                   #   
                   # }
                   # if (input$postal != "All") {
                   #   coord_HE <- filter(table, Post_Code == input$postal)
                   # }
                   
                   
                   # popup_road_name <- paste0("<strong><i>",
                   #                           df_mapmatching$name, 
                   #                          "</i></strong><br>load(%): <strong> ", df_mapmatching$`load(%)`)
                   
                   popup_road_name <- paste0(df_mapmatching$name, "  ", 
                                             ", load(%): ", df_mapmatching$`load(%)`)
                   
                   
                   ### iteractive plot
                   map <- leaflet() %>%
                     # setView(-5.35, 36.150, 16) %>%
                     addTiles(group = "OSM (default)") %>%
                     addPolylines(data = edges, 
                                  color='blue', 
                                  group='edge',
                                  weight = df_mapmatching$counts,
                                  label = popup_road_name,
                                  labelOptions = labelOptions(noHide = F, direction = "top")) %>%
                     addLayersControl(
                       overlayGroups = c("edge"),
                       options = layersControlOptions(collapsed = TRUE))
                   
                   map
                   
                   
                 })
                 
                 
                 # Return to client
                 output$myMap = renderLeaflet(finalMap())
                 
                 # output$menu <- renderMenu({
                 #   
                 #   
                 #   if(input$Traffic_flow == "Roma")
                 #     my_list = "March_trip_CARS_only_UNIBS.html"
                 #   
                 #   sidebarMenu(my_list)
                 #   
                 # })
                 
                   
                   # tables to display
                 
                   output$text_table1 <- renderText({ 
                     paste("Number of analyzed vehicles based on their typology. 
                     Commercial vehicles represent a variable percentage in the
                           total number of vehicles", input$table_1)
                   })
                 
                   output$table_1 <- DT::renderDataTable(TABLE1())
                   
                   output$text_table2 <- renderText({ 
                     paste("Structure of 'routes' obtained from the elaboration of Floating Car Data (FCD)", input$table_1)
                   })
                   
                   output$table_2 <- DT::renderDataTable(TABLE2())
                   
                   
                   
                   ##---- close session and therefore PostgreSQL connection
                   # cancel.onSessionEnded <- session$onSessionEnded(function() {
                   #   dbDisconnect(conn_HAIG)
                   # })

  })
})
              
                 
                 



