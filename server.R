

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

#https://rviews.rstudio.com/2017/05/17/databases-using-r/


# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# DB <- "HAIG_ROMA"
# conn_HAIG <- dbConnect(drv, dbname = DB,             # input$database
#                        host = "10.1.0.1", port = 5432,
#                        user = "postgres", password = "superuser")
# 
# ### preview the DB...for now it is enough....
# route = tbl(conn_HAIG, "route")
# # AAA = as.data.frame()
# ## check size...
# tally(tbl(conn_HAIG, "route"))
# 
# 
# 
# ### querying "route.timedate_o" take a lot of time......
# start_time <- Sys.time()
# route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
#                                       route.idterm,
#                                       route.tripdistance_m,
#                                       /* route.timedate_o, */
#                                       route.breaktime_s,
#                                       route.triptime_s,
#                                       route.deviation_pos_m,
#                                       idterm_portata.vehtype
#                                FROM route
#                                LEFT JOIN idterm_portata
#                                     ON route.idterm = idterm_portata.idterm
#                                     /*WHERE route.id <= 1919047*/
#                                     /*LIMIT 100000*/
#                                       ")
# 
# end_time <- Sys.time()
# print(end_time - start_time)




############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

shinyServer(function(input, output, session) {
  
  observe({
    
    DB <- input$database
    conn_HAIG <- dbConnect(drv, dbname = DB,           
                           host = "10.1.0.1", port = 5432,
                           user = "postgres", password = "superuser")
 
                   # number of records
                   TABLE1 <- reactive({
                     
                     count_vehtype = dbGetQuery(conn_HAIG,"
                                    SELECT vehtype, COUNT(*)
                                    FROM public.idterm_portata 
                                    group by vehtype ")
                     ###---rename data
                     count_vehtype$vehtype <- gsub(1, "auto", (count_vehtype$vehtype))
                     count_vehtype$vehtype <- gsub(2, "veicoli commerciali", (count_vehtype$vehtype))
                     count_vehtype$percentage <- round(((count_vehtype$count)/sum(count_vehtype$count)*100), 2)
                     names(count_vehtype) <- c("Type of Vehicle", "number of vehicles", "percentage (%) by type")
                     # output table
                     return(count_vehtype) 
                   })
                   
                   
                   TABLE2 <- reactive({
                     
                     # ###------ Create a Progress object
                     progress <- shiny::Progress$new()
                     # Make sure it closes when we exit this reactive, even if there's an error
                     on.exit(progress$close())
                     progress$set(message = "Querying data...", value = 0.5)

                      ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                       route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
                                          route.idterm,
                                          route.tripdistance_m,
                                          route.timedate_o,
                                          route.breaktime_s,
                                          route.triptime_s,
                                          route.deviation_pos_m,
                                          idterm_portata.vehtype
                                   FROM route
                                   LEFT JOIN idterm_portata
                                        ON route.idterm = idterm_portata.idterm
                                        LIMIT 100000
                                          ")
                  
                       ### rename data
                       route$vehtype <- gsub(1, "auto", (route$vehtype))
                       route$vehtype <- gsub(2, "veicoli commerciali", (route$vehtype))
                       route$deviation_pos_m <- round(route$deviation_pos_m, digits = 1)
                       ## convert break (stop) time from seconds to minutes
                       route$breaktime_m <- round((route$breaktime_s)/60, digits = 1)
                       ## convert triptime_s into minutes
                       route$triptime_m <- round((route$triptime_s)/60, digits = 1)
                       
                       ### add SPEED registered along each TRIP
                       route <- route[complete.cases(route), ]
                       ## speed
                       route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                       route <- route %>%
                         filter(speed_trip > 0 & speed_trip < 240)
                       
                       route_table <- route %>%
                         dplyr::select(idtrajectory,
                                idterm,
                                tripdistance_m,
                                timedate_o,
                                breaktime_m,
                                triptime_m,
                                deviation_pos_m,
                                vehtype,
                                speed_trip)
                       
                       names(route_table) <- c("idtrajectory", "idterm", "trip distance (meter)", "timestamp",
                                               "stop time (min)", "trip duration (min)",
                                               "offset position (meters)", "type of vehicle", "speed (km/h)")
                       return(route_table)
            
                   })
                   
                   
                   
        
                   ###------ make plots-------###################################
            
                    output$bar_offset_position <- renderPlotly({
              
                      ###------ Create a Progress object
                      progress <- shiny::Progress$new()
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      progress$set(message = "Querying data...bar plot", value = 0.5)
                     
                     ## ----- plot deviation (or offset) of the position
                     route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
                                          route.deviation_pos_m
                                          FROM route ")
                     
                     
                     BBB <- NULL
                     bin_dev = c(5,10,40,100,1000)  ## deviations bins in meters
                     
                     for (i in 1:length(bin_dev)){
                       AAA <- route %>%
                         filter(deviation_pos_m >= bin_dev[i]) %>%
                         summarise(counts = length(idtrajectory))
                       BBB<- rbind(BBB, AAA)
                     }
                     
                     
                     deviations <- as.data.frame(cbind(bin_dev, BBB))
                     deviations <- na.omit(deviations)
                     deviations$N_tot_trips <- sum(deviations$counts)
                     ## calculate percentage of trips by deviation distance
                     deviations$perc_count <- round((deviations$counts/deviations$N_tot_trips)*100, digits = 1)
                     
                     deviations$bin_dev <- as.factor(deviations$bin_dev)
                  
                     Figure_bar_offset_position <-  ggplot(data = deviations,
                                                       aes(bin_dev, perc_count, fill = bin_dev)) + guides(fill=FALSE) +
                                                       geom_bar(stat = "identity") + 
                                                       guides(fill=FALSE) +
                                                       # ylim(0, 90) +
                                                       theme_bw() +
                                                       theme( strip.text = element_text(size = 10)) +
                                                       theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
                                                       theme(axis.title.x = element_blank()) +                  # Remove x-axis label
                                                       xlab("distance (meters)") +
                                                       ylab("frequency (%)") +
                                                       theme(axis.title.y = element_text(face="bold", colour="black", size=10),
                                                             axis.text.y  = element_text(angle=0, vjust=0.5, size=10)) +
                                                       theme(axis.title.x = element_text(face="bold", colour="black", size=10),
                                                             axis.text.x  = element_text(angle=0, vjust=0.5, size=10, hjust = 0.5)) + 
                                                       # scale_x_continuous(breaks=c(0, 5, 10, 15, 20)) +
                                                       ggtitle("Bias larger than 5, 10, 40, 100 and 1000 meters") + 
                                                       theme(plot.title = element_text(lineheight=.8, face="bold", size = 10))
                  
                     # return(ggplotly(Figure_bar_offset_position))
                     return(Figure_bar_offset_position)
                   })
                   
                   
                ### ------------ ############ 
                ## bias position (end - start new trip)
                 output$plot_offset_position <- renderPlot({
               
                     ###------ Create a Progress object
                     progress <- shiny::Progress$new()
                     # Make sure it closes when we exit this reactive, even if there's an error
                     on.exit(progress$close())
                     progress$set(message = "Querying data...chart plot", value = 0.5)
                     
                     ## ----- plot deviation (or offset) of the position
                     route = dbGetQuery(conn_HAIG, "SELECT route.idtrajectory,
                                          route.deviation_pos_m
                                          FROM route ")
                     
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
                                         theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, size=10)) +
                                         theme(axis.text.x=element_text(size=10, colour = "black")) +
                                         theme(axis.title.x = element_text(face="bold", colour="black", size=10)) +
                                         xlab("distance (meters)") +
                                         ylab("frequency (%)") +
                                         # xlim(0,100)+
                                         geom_vline(xintercept = 5, col="black", lty=2, size=1) +
                                         geom_vline(xintercept = 10, col="red", lty=2, size=1) +
                                         geom_vline(xintercept = 100, col="blue", lty=2, size=1) +
                                         # geom_text(aes(x = 12 , y = 0.15 , label = "10 m"), size = 4) +
                                         # geom_text(aes(x = 100 , y = 0.15 , label = "150 m"), size = 4) +
                                         theme(axis.title.y = element_text(face="bold", colour="black", size=10),
                                               axis.text.y  = element_text(angle=0, vjust=0.5, size=15, colour="black")) +
                                         ggtitle("Bias between end-start of new trip") +
                                         theme(plot.title = element_text(lineheight=.8, face="bold", size = 10))
                     
                     # return(ggplotly(Figure_offset_position))
                     return(Figure_offset_position)
                   })
                   
                   
                 ### ------------ #####   
                 ### mean speed #######
                 output$plot_average_speed <- renderPlot({
                     
                     ###------ Create a Progress object
                     progress <- shiny::Progress$new()
                     # Make sure it closes when we exit this reactive, even if there's an error
                     on.exit(progress$close())
                     progress$set(message = "Querying data...chart plot", value = 0.5)
                     
                     ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                     route = dbGetQuery(conn_HAIG, "SELECT route.tripdistance_m,
                                                           route.triptime_s
                                                           FROM route
                                                           ")
                   
                     ##---- convert triptime_s into minutes
                     route$triptime_m <- (route$triptime_s)/60
                     
                     ### add SPEED registered along each TRIP
                     route <- route[complete.cases(route), ]
                     ## speed
                     route$speed_trip = round((route$tripdistance_m/1000)/(route$triptime_s/3600), digits = 0)  ## km/h
                     route <- route %>%
                       filter(speed_trip > 0 & speed_trip < 220)
                     
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
                               theme(plot.title = element_text(lineheight=.8, face="bold", size = 10))
                             
                     
                     # return(ggplotly(Figure_mean_speed))
                     return(Figure_mean_speed)
                   })

                 
                 
                 ### stop time (breaktime_m)
                 output$plot_stop_time <- renderPlot({
                   
                   ###------ Create a Progress object
                   progress <- shiny::Progress$new()
                   # Make sure it closes when we exit this reactive, even if there's an error
                   on.exit(progress$close())
                   progress$set(message = "Querying data...chart plot", value = 0.5)
                   
                   ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                   route = dbGetQuery(conn_HAIG, "SELECT route.breaktime_s,
                                                          triptime_s,
                                                          tripdistance_m
                                                           FROM route
                                                           ")
                   
                   route <- route[complete.cases(route), ]
                   ## speed
                   route$speed_trip = (route$tripdistance_m/1000)/(route$triptime_s/3600)  ## km/h
                   route <- route %>%
                     filter(speed_trip > 0 & speed_trip < 220)
                   
                   
                   ##----- convert breaktime_s into minutes
                   route$breaktime_m <- (route$breaktime_s)/60
                
                   ## distribution of the stop times ###
                   ### plot a distribution
                   Figure_stop_time <- ggplot(route, aes(x = breaktime_m)) +
                                 theme_bw() +
                                 geom_density(stat = 'bin') +
                                 scale_x_continuous(trans='log10', breaks=c(3, 10, 50, 780)) +
                                 theme(legend.title=element_blank()) + 
                                 aes(y=stat(count)/sum(stat(count))) + 
                                 scale_y_continuous(labels = scales::percent) +
                                 theme_bw() +
                                 theme( strip.text = element_text(size = 10)) +
                                 guides(fill=FALSE) +
                                 theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, size=10)) +
                                 theme(axis.text.x=element_text(size=10, colour = "black")) +
                                 theme(axis.title.x = element_text(face="bold", colour="black", size=10)) +
                                 geom_vline(xintercept = 3, col="gray", lty=2, size=1) +
                                 geom_vline(xintercept = 10, col="red", lty=2, size=1) +
                                 geom_vline(xintercept = 50, col="blue", lty=2, size=1) +
                                 geom_vline(xintercept = 780, col="black", lty=2, size=1) +
                                 xlab("tempo di sosta (minuti)") +
                                 ylab("frequenza (%)") +
                                 theme(axis.title.y = element_text(face="bold", colour="black", size=10),
                                       axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
                                 ggtitle("distribuzione dei tempi di sosta") +
                                 theme(plot.title = element_text(lineheight=.8, face="bold", size = 10))
                   
                   # return(ggplotly(Figure_stop_time))
                   return(Figure_stop_time)
                 })
                 
                 
                 ##### --------------------------------------- #####
                 ### trip distance (tripdistance_m)  (in km) #######
                 output$plot_trip_distance <- renderPlot({
                   
                   ###------ Create a Progress object
                   progress <- shiny::Progress$new()
                   # Make sure it closes when we exit this reactive, even if there's an error
                   on.exit(progress$close())
                   progress$set(message = "Querying data...chart plot", value = 0.5)
                   
                   ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                   route = dbGetQuery(conn_HAIG, "SELECT triptime_s,
                                                          tripdistance_m
                                                           FROM route
                                                           ")
                   
                   route <- route[complete.cases(route), ]
                   ## speed
                   route$speed_trip = (route$tripdistance_m/1000)/(route$triptime_s/3600)  ## km/h
                   route <- route %>%
                     filter(speed_trip > 0 & speed_trip < 240)
                   
                   ### transform travelled sitance into km
                   route$tripdistance_km <- route$tripdistance_m/1000
                   
                   
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
                     theme(plot.title = element_text(lineheight=.8, face="bold", size = 10))
                 
                   return(Figure_trip_distance)
                 })
                 
                 
                 
                 #### ---------------------------------------##########
                 ### trip times (triptime_m) (in minutes) #############
                 output$plot_trip_time <- renderPlot({
                   
                   ###------ Create a Progress object
                   progress <- shiny::Progress$new()
                   # Make sure it closes when we exit this reactive, even if there's an error
                   on.exit(progress$close())
                   progress$set(message = "Querying data...chart plot", value = 0.5)
                   
                   ##----join "vehicle type" from the table "idterm_portata" (to get vehitype)
                   route = dbGetQuery(conn_HAIG, "SELECT triptime_s,
                                                          tripdistance_m
                                                           FROM route
                                                           ")
                   
                   route <- route[complete.cases(route), ]
                   ## speed
                   route$speed_trip = (route$tripdistance_m/1000)/(route$triptime_s/3600)  ## km/h
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
                     theme(plot.title = element_text(lineheight=.8, face="bold", size = 10))
                   
                   return(Figure_trip_time)
                 })
                 
               
                 
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
                   output$table_1 <- DT::renderDataTable(TABLE1())
                   output$table_2 <- DT::renderDataTable(TABLE2())
                   
                   
                   
                   ##---- close session and therefore PostgreSQL connection
                   cancel.onSessionEnded <- session$onSessionEnded(function() {
                     dbDisconnect(conn_HAIG)
                   })
                   
  })
})
              
                 
                 



