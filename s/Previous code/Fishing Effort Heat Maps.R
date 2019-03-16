setwd('/Users/tiffanyong/Documents/GitHub/gfw/s')
library(sf)
library(mregions)
library(DBI)
library(bigrquery)
library(ggsci)
library(tidyverse) #loads dplyr and friends
require(ggplot2) #there is no package called ggplot
library(hrbrthemes)
library(viridisLite)
library(rgdal)
library(dplyr)
library(viridis)
library(rgdal)  #for readOGR()
library(rasterVis) #for levelplot
library(RColorBrewer) #for levelplot
library(rgeos)
library(grid)
library(gridExtra)
library(rwdpa) #TO FIND SHAPEFILES POSSIBLY!!!!
library(raster) #for raster images
library(fields) # for image.plot
library(colorRamps) # for image.plot color scale
library(lattice) #for levelplot perhaps
library(maptools) #to read shapefiles
library(sp) #for prime meridian
library(maps) #for prime meridian

#########################

#cleansql, code to "clean" the sql query - replaces newline and \t with spaces 
cleanSql = function(sql){
  sql <- gsub("\n","",sql) 
  sql <- gsub("\t","",sql)
  sql
}


#authenticate the connection to BigQuery
BQ_connection <-  DBI::dbConnect(bigrquery::bigquery(), 
#                                 project = 'global-fishing-watch',
                                 project <- "world-fishing-827",
                                 dataset = "global_footprint_of_fisheries", 
                                 billing = "world-fishing-827", # your billing account name
                                 use_legacy_sql = FALSE) # specify we are using Standard SQL

#getting a map of the EEZ to obtain the bounding box of the region

#get shapefile of EEZ - TAKES TIME
#eez <- readOGR(dsn = "/Users/tiffanyong/Documents/GitHub/gfw/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox
eez <- readOGR(dsn = "/Users/timwhite/gfw2/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox


eez@data = eez@data[which(is.na(eez@data$Sovereign1) == F),] # remove a single NA value for disputed Chinese land, is this necessary

#get shapefile of MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
#mpa <- readOGR(dsn = "/Users/tiffanyong/Documents/GitHub/gfw/data/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")
#mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")

mpa <- readOGR(dsn = "/Users/timwhite/Dropbox/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")

#SEARCH FOR EEZs and MPAs HERE
#View(eez@data) 
#View(mpa@data)

#SET TERRITORY,MPA NAME,AND CREATION DATE HERE
territory = "Phoenix Group"
mpa_name = "Phoenix Islands Protected Area"

#territory = "Hawaii"
#mpa_name = "Phoenix Islands Protected Area"

creation_date = '2015-01-01'
curr_eez = eez[eez@data$Territory1 == territory, ] # get a specific EEZ
curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ]


#FOR MACQUARIE ISLAND AND COMMONWEALTH SOUTHWEST
#curr_mpa = curr_mpa[curr_mpa$NO_TAKE == 'All', ]
#FOR TERRES AUSTRALES FRANCAISES
#curr_mpa = mpa[mpa@data$MANG_AUTH == mpa_name, ]
#curr_eez2 = eez[eez@data$Territory1 == 'Kerguélen', ]
#curr_eez3 = eez[eez@data$Territory1 == 'Crozet Islands', ]

#Before you change it to an sf object, save the shapefile in the R object
shapefile = curr_eez
shapefile_mpa = curr_mpa

#BRING THE POLYGON TOGETHER
if (territory == "Hawaii") {
  print('I knew you were trouble')
  curr_mpa = spTransform(curr_mpa, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180 +lat_wrap180"))
  curr_eez = spTransform(curr_eez, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180 +lat_wrap180"))
}

###########################

#vizualize with package leaflet - creates a map
if (requireNamespace("leaflet")) {
  library('leaflet')
  leaflet() %>%
    addTiles() %>%
    addPolygons(data = curr_eez, color = "blue")  %>%
    addPolygons(data = curr_mpa, color ="green")
  # %>%
  #   addPolygons(data = curr_eez2, color ="blue")%>%  #last 2 terres australes only
  #   addPolygons(data = curr_eez3, color ="blue")
  }

###########################

# transform the shapefile into an sf object for eez
curr_eez <- curr_eez %>% 
  sf::st_as_sf() 

#transform shapefile into sf object for mpa 
curr_mpa <- curr_mpa %>% 
  sf::st_as_sf()  

#this sf transformation may have already happened

# get the bounding box of the shapefile
if(territory == "Hawaii") {
  bbox <- sf::st_bbox(shapefile) 
} else {
  bbox <- sf::st_bbox(curr_eez)
}

# extend the bounding box 1 degree in every direction.
min_lon <- bbox[["xmin"]] - 1 
max_lon <- bbox[["xmax"]] + 1
min_lat <- bbox[["ymin"]] - 1
max_lat <- bbox[["ymax"]] + 1 


# define mapping resoluion in degrees
resolution <- 0.1

# sets year before and year after dates
year_before_date = as.character(as.Date(creation_date) - 365)
year_after_date = as.character(as.Date(creation_date) + 365)
 
#sets local variables 
graph_title = territory
date1 = year_before_date
date2 = creation_date
binned_effort = list() 
subtitles = list()



##################
#Getting a query #
##################

for(i in 1:2) {
  
  if(i == 2) {
    date1 = creation_date
    date2 = year_after_date
  }    
  
  subtitle = paste(date1, "to", date2, sep =" ")  
  subtitles[[i]] = subtitle
  
      #process to query the data
      project <- "world-fishing-827" # put your project name here
      
      #convert the coordinates to from 100th of degree to degrees
      #filter coordinates so they are inside bounding box
      #exclude anything with no fishing effort 
      sql = paste("
          SELECT
              FLOOR(lat_bin/0.1)*",resolution," + 0.5*",resolution," lat_bin_center,
              FLOOR(lon_bin/0.1)*",resolution," + 0.5*",resolution," lon_bin_center,
              SUM(fishing_hours) fishing_hours
              FROM (
              SELECT
              lat_bin/100 lat_bin,
              lon_bin/100 lon_bin,
              fishing_hours
              FROM
              `global-fishing-watch.global_footprint_of_fisheries.fishing_effort`
              WHERE
              _PARTITIONTIME >= '",date1,"'
              AND _PARTITIONTIME < '",date2,"')
              WHERE 
              lat_bin >= ",min_lat,"
              AND lat_bin <= ",max_lat,"
              AND lon_bin >= ",min_lon,"
              AND lon_bin <= ",max_lon,"
              GROUP BY
              lat_bin_center,
              lon_bin_center
              ", sep = "")
      
      sql = cleanSql(sql)
      
      #############
     ####### CHECK SQL QUERY####
      ##########
      #############
      
      #test queries here for Hawaii
      sql = "SELECT
      FLOOR(lat_bin/0.1)*0.1 + 0.5*0.1 lat_bin_center,
      FLOOR(lon_bin/0.1)*0.1 + 0.5*0.1 lon_bin_center,
      SUM(fishing_hours) fishing_hours
      FROM (
        SELECT
        lat_bin/100 lat_bin,
        lon_bin/100 lon_bin,
        fishing_hours
        FROM
        `global-fishing-watch.global_footprint_of_fisheries.fishing_effort`
        WHERE
        _PARTITIONTIME >= '2015-01-01'
        AND _PARTITIONTIME < '2016-01-01')
      WHERE
      lat_bin >= 14.5631241704442
      AND lat_bin <= 32.7978661717864
      AND lon_bin BETWEEN 170 AND 179
      OR 
      lat_bin >= 14.5631241704442
      AND lat_bin <= 32.7978661717864
      AND lon_bin BETWEEN -179 AND -150
      GROUP BY
      lat_bin_center,
      lon_bin_center "
      
      binned_effort_around_eez <- query_exec(sql,project, use_legacy_sql = FALSE)
      
      binned_effort[[i]] = binned_effort_around_eez  
} 

###################################################
#DATAFRAME ARITHMETIC/MERGING FOR DIFFERENCE PLOT#
##################################################
merged = merge(binned_effort[[1]], binned_effort[[2]], by=c('lat_bin_center', 'lon_bin_center'), all=TRUE)
merged[is.na(merged)] <- 0
names(merged)[names(merged)=="fishing_hours.x"] <- "fishing_hours_before" 
names(merged)[names(merged)=="fishing_hours.y"] <- "fishing_hours_after"

#adding a "difference" column in merged 
merged$diff = (merged$fishing_hours_after - merged$fishing_hours_before)

#dataframe with just latitude, longitude, and difference (named as fishing_hours)
dataframe.difference = data.frame(lat_bin_center = merged$lat_bin_center,
                                  lon_bin_center = merged$lon_bin_center,
                                  fishing_hours = merged$diff)

# storing the difference dataframe in the same list that holds before/after effort
binned_effort[[3]] = dataframe.difference 


##############COMMENT OUT IF TESTING#########################
#save into an Rdata file - 
# file_name = paste("../data/", graph_title, "BeforeAfterEffort.Rdata", sep = '')
# save(graph_title, shapefile, shapefile_mpa, binned_effort , file = file_name) 
############################################################

############
# PLOTTING #
############
####################
# Before-After Plot#
####################

#PRIME MERIDIAN SHENANIGANS
# wm <- map.wrap(map(projection="rectangular", parameter=0, orientation=c(90,0,180), plot=FALSE))
# world_map <- data.frame(wm[c("x","y")])
# names(world_map) <- c("lon","lat")
# world_map$lon <- world_map$lon * 180/pi + 180
# world_map$lat <- world_map$lat * 180/pi
# 
# #changing coordinates to 360 for binned_effort[[3]]
# len = nrow(binned_effort[[3]])
# 
# for(t in 1:len) {
#   initial = binned_effort[[3]]$lon_bin_center[t]  
#   binned_effort[[3]]$lon_bin_center[t] = ((initial + 180) %% 360)
# }

###############

#################################################################
#huge issues with aes, geom_raster#######
#I've tried creating a new 0-360 map,while converting coordinates to the 360 system,
#but it's hard to plot that both fishing effort and the new 360 map
#with ggplot 
#################################################################
#################################################################
#################################################################
#I would check out geom_raster and aes 
#################################################################
effortPlot = list()
for(j in 1:2) {

  #plotting effort here 
  effortPlot[[j]] = binned_effort[[j]] %>% 
        filter(fishing_hours > 1) %>% 
        ungroup() %>% 
        ggplot()+
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
    viridis::scale_fill_viridis(name = "Fishing hours" ,
                                    trans = "log",
                                    breaks = scales::log_breaks(n = 10, base = 4))+
        geom_sf(data = curr_eez,
                fill = NA,
                col = "black",
                size = 0.6
        ) +
    #added this for mpa
        geom_sf(data = curr_mpa,
                fill = NA,
                col = "red",
                size = 0.1
        ) +
        hrbrthemes::theme_ipsum()+
        labs(title = graph_title,
             subtitle = subtitles[[j]],
             y = "",
             x = "")+
        theme(axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0),
              plot.subtitle = element_text(size = 10, hjust = 0),
              
              plot.margin = margin(10,0,0,0),
              legend.margin = margin(5,5,20,0)
              )
}


#########################
# Effort Difference Plot#
#########################

#name the file here
file = paste(territory, ' Effort and Difference', sep='')
title = paste('../Figures/', file, '.png', sep = '')   

#plotting difference here
effortPlot[[3]] = dataframe.difference %>% 
  filter(abs(fishing_hours) > 1) %>%
  ungroup() %>% 
  ggplot()+
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
  viridis::scale_fill_viridis(name = "Difference")+
  geom_sf(data = curr_eez,
          fill = NA,
          col = "black",
          size = 0.6
  ) +
  geom_sf(data = curr_mpa,
          fill = NA,
          col = "red",
          size = 0.1
  ) +
  hrbrthemes::theme_ipsum()+
  labs(title = "After-Before Difference",
       y = "",
       x = "")+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 0),
        # plot.subtitle = element_text(size = 10, hjust = 0),
        
        plot.margin = margin(0,0,0,0),
        legend.margin = margin(5,5,20,0))

#SAVE PLOT PICTURES TO FOLDER HERE 
png(title,units="in", width=8, height=7, res=300)
#png(title,units="in", width=8, height=15, res=300)
grid.arrange(effortPlot[[1]], effortPlot[[2]], effortPlot[[3]], nrow=2)
dev.off()

################################################################
#I would plot one at a time because it isn't working all at once
#################################################################
################################################################
################################################################





#testing FOR DIFFERENCE PLOT BINNEDEFFORT[[3]]

# ggplot() + geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), 
#                         aes(x=long, y = lat, group=group)) +
#   coord_fixed(1.3) + 
#   coord_equal(xlim = c(0, 360), ylim = c(-90, 90))


#APPARENTLY A BETTER WAY


#plotting difference here
# bruh = binned_effort[[3]] %>% 
#   filter(abs(fishing_hours) > 1) %>%
#   ungroup() %>% 
#   
#   ggplot(aes(x = lon_bin_center, y = lat_bin_center, fill=fishing_hours)) +
# 
#   #ggplot()+
#   #geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
#   # viridis::scale_fill_viridis(name = "Difference")+
#   # hrbrthemes::theme_ipsum()+
#   labs(title = "TEST",
#        y = "",
#        x = "")+
#   theme(axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         plot.title = element_text(size = 12, hjust = 0),
#         # plot.subtitle = element_text(size = 10, hjust = 0),
#         
#         plot.margin = margin(0,0,0,0),
#         legend.margin = margin(5,5,20,))
# 
# plot(bruh)
