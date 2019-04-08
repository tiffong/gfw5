#libraries -------------
#setwd('/Users/tiffanyong/Desktop/backup_gfw/backupgfw2/s')

setwd('/Users/tiffanyong/Documents/GitHub/GFW5/s')
library(sf)
library(mregions)
library(DBI)
library(bigrquery)
library(hrbrthemes)
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
library(leaflet) #for visualization
library(RColorBrewer)
library(scales)
library(lubridate)
###############
#rm(list=ls())# protecc with life
###############

#######INITIALIZING########
#get shapefiles for EEZ - TAKES TIME
eez <- readOGR(dsn = "/Users/tiffanyong/Documents/GitHub/gfw/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox
eez@data = eez@data[which(is.na(eez@data$Sovereign1) == F),] # remove a single NA value for disputed Chinese land, is this necessary

#get shapefiles for MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")

###########################

#SET TERRITORY,MPA NAME,AND CREATION DATE HERE
creation_date = '2016-08-24'
territory = 'Islas San Félix and San Ambrosio'
mpa_name = 'Nazca-Desventuradas'

curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ] 

if(mpa_name =="Pacific Remote Islands"){
  print('getting PRI eez shapefiles')
  curr_eez = eez[eez@data$MRGID_Sov1 == 2204,]
  curr_eez = curr_eez[curr_eez@data$ISO_Ter1 == "UMI",]  
  curr_eez = curr_eez[!(curr_eez@data$Territory1 %in% c("Wake Island", "Jarvis Island", "Johnston Atoll")),]

} else if (territory=="Hawaii") {
  print('getting Hawaii shapefiles')
  mpa_before = mpa[mpa@data$ORIG_NAME == 'Papahānaumokuākea', ]
} else {
  print('getting EEZ of normal MPA')
  curr_eez = eez[eez@data$Territory1 == territory,] # get a specific EEZ
}

#Before you convert to sf object, save the orig. shapefile in the Rdata
shapefile = curr_eez
#shapefile = mpa_before
shapefile_mpa = curr_mpa

#BRING THE POLYGON TOGETHER IF IT IS SEPARATED BY THE 180 LINE
if (territory == "Hawaii") { #wraps it at the 0
  curr_mpa = spTransform(curr_mpa, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  curr_eez = spTransform(curr_eez, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  mpa_before = spTransform(mpa_before, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
} else if (mpa_name == "Pacific Remote Islands") {
  print('transforming PRI shpefiles')
  curr_mpa = spTransform(curr_mpa, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  curr_eez = spTransform(curr_eez, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
}

########VISUALIZE SHAPEFILE#######

  leaflet() %>%
    addTiles() %>%
    addPolygons(data = curr_mpa, color ="green") %>%
    #addPolygons(data = mpa_before, color = "blue") 
#%>%
    addPolygons(data = curr_eez, color = "blue") 


#################################

#transform shapefile into sf object for plotting 
curr_mpa <- curr_mpa %>% 
  sf::st_as_sf() 
  print('mpa as sf')

if(territory != 'Hawaii') {
  print('eez as sf')
  curr_eez <- curr_eez %>% 
    sf::st_as_sf() 
}

if(territory == "Hawaii") {
  print('mpa_before')
  mpa_before <- mpa_before %>% 
    sf::st_as_sf() 
}


# get the bounding box of the shapefile 
# extend the bounding box 1 degree in every direction
if(territory != "Hawaii") {
  bbox <- sf::st_bbox(curr_eez)

  #bbox <- sf::st_bbox(curr_mpa)  

  min_lon <- bbox[["xmin"]] - 1 
  max_lon <- bbox[["xmax"]] + 1
  min_lat <- bbox[["ymin"]] - 1
  max_lat <- bbox[["ymax"]] + 1 
} 

# define mapping resolution in degrees
resolution <- 0.5

# sets year before and year after dates
year_before_date = as.Date(creation_date) %m+% years(-1)
year_after_date = as.Date(creation_date) %m+% years(1)
print(paste(year_before_date, creation_date , year_after_date,sep = ' ---- ' )) #print check right dates

##################
#Getting a query #
##################
#sets local variables 
graph_title = mpa_name
date1 = year_before_date
date2 = creation_date
binned_effort = list() 
subtitles = list()

if(mpa_name == "Pacific Remote Islands") {
#querying PACIFIC REMOTE ISLANDS -------------
  print('querying PRI')
  for(i in 1:2) {
    
    if(i == 2) {
      date1 = creation_date
      date2 = year_after_date
    }    
    print(date1)
    print(date2)
    
    subtitle = paste(date1, "to", date2, sep =" ")  
    subtitles[[i]] = subtitle
    
    project <- "gfw-tiff" # put your project name here
    
    ##test queries here for Hawaii
    sql = paste("SELECT
                FLOOR(lat/",resolution,")*",resolution," + 0.5*",resolution," lat_bin_center,
                FLOOR(lon/",resolution,")*",resolution," + 0.5*",resolution," lon_bin_center,
                SUM(IF(nnet_score2 > .5, hours,0)) fishing_hours
                FROM
                `world-fishing-827.gfw_research.nn7`
                WHERE
                hours>0
                AND _PARTITIONTIME BETWEEN '",date1,"'
                AND '",date2,"'
                AND seg_id IN (
                SELECT
                seg_id
                FROM
                `world-fishing-827.gfw_research.pipeline_p_p550_daily_segs`
                WHERE
                good_seg )
                AND mmsi IN (
                SELECT
                mmsi
                FROM
                `world-fishing-827.gfw_research.vessel_info_allyears_20180518`
                WHERE
                on_fishing_list_best )
                
                AND (lat BETWEEN -4.730837 and 24)
                AND (lon BETWEEN 162.0698 and 180 
                or lon between -180 and -156.4143)
                
                GROUP BY
                lat_bin_center,
                lon_bin_center", sep = '')
    
    sql <- gsub("\n","",sql)
    sql <- gsub("\t","",sql)
    
    binned_effort_around_eez <- query_exec(sql,project, use_legacy_sql = FALSE)
    binned_effort[[i]] = binned_effort_around_eez  
  }   
} else if (territory == "Hawaii") {
#querying Hawaii-------------
  print('querying Hawaii')
  for(i in 1:2) {
    
    if(i == 2) {
      date1 = creation_date
      date2 = year_after_date
    }    
    print(date1)
    print(date2)
    
    subtitle = paste(date1, "to", date2, sep =" ")  
    subtitles[[i]] = subtitle
    
    project <- "gfw-tiff" # put your project name here
    
    ##test queries here for Hawaii
    sql = paste("SELECT
                FLOOR(lat/",resolution,")*",resolution," + 0.5*",resolution," lat_bin_center,
                FLOOR(lon/",resolution,")*",resolution," + 0.5*",resolution," lon_bin_center,
                SUM(IF(nnet_score2 > .5, hours,0)) fishing_hours
                FROM
                `world-fishing-827.gfw_research.nn7`
                WHERE
                hours>0
                AND _PARTITIONTIME BETWEEN '",date1,"'
                AND '",date2,"'
                AND seg_id IN (
                SELECT
                seg_id
                FROM
                `world-fishing-827.gfw_research.pipeline_p_p550_daily_segs`
                WHERE
                good_seg )
                AND mmsi IN (
                SELECT
                mmsi
                FROM
                `world-fishing-827.gfw_research.vessel_info_allyears_20180518`
                WHERE
                on_fishing_list_best )
                
                AND lat BETWEEN 18.23458
                AND 32.79786
                AND (lon BETWEEN 176.84422 and 180 
                or lon between -180 and -160.0173)
                
                GROUP BY
                lat_bin_center,
                lon_bin_center", sep = '')
    
    sql <- gsub("\n","",sql)
    sql <- gsub("\t","",sql)
    
    binned_effort_around_eez <- query_exec(sql,project, use_legacy_sql = FALSE)
    binned_effort[[i]] = binned_effort_around_eez  
  } 
} else {
  #for everything other than hawaii------
  print(paste("Querying", mpa_name ,sep = ' '))
  for(i in 1:2) {
    if(i == 2) {
      date1 = creation_date
      date2 = year_after_date
    }    
    
    subtitle = paste(date1, "to", date2, sep =" ")  
    subtitles[[i]] = subtitle
    
    project <- "gfw-tiff" # put your project name here
    
    #da real query --------
    sql  = paste(
      "SELECT
      FLOOR(lat/",resolution,")*",resolution," + 0.5*",resolution," lat_bin_center,
      FLOOR(lon/",resolution,")*",resolution," + 0.5*",resolution," lon_bin_center,
      SUM(IF(nnet_score2 > .5, hours,0)) fishing_hours
      FROM
      `world-fishing-827.gfw_research.nn7`
      WHERE
      _PARTITIONTIME BETWEEN '",date1,"'
      AND '",date2,"'
      
      AND seg_id IN (
      SELECT
      seg_id
      FROM
      `world-fishing-827.gfw_research.pipeline_p_p550_daily_segs`
      WHERE
      good_seg )
      AND mmsi IN (
      SELECT
      mmsi
      FROM
      `world-fishing-827.gfw_research.vessel_info_allyears_20180518`
      WHERE
      on_fishing_list_best )
      AND lat >= ",min_lat,"
      AND lat <= ",max_lat,"
      AND lon >= ",min_lon,"
      AND lon <= ",max_lon,"
      GROUP BY
      lat_bin_center,
      lon_bin_center"
        , sep = "")

      sql <- gsub("\n","",sql)
      sql <- gsub("\t","",sql)

#end of everything other than hawaii--------   
  binned_effort_around_eez <- query_exec(sql,project, use_legacy_sql = FALSE, max_pages=Inf)
  binned_effort[[i]] = binned_effort_around_eez
  
  }
}

#try rasterizing (if I remember correctly, this should be ignored)------------

#small part of HI
#raster_before = rasterFromXYZ(binned_effort[[1]])
#proj4string(raster_before) = CRS("+proj=utm +zone=4+datum=WGS84")
#raster_before = projectRaster(raster_before, crs="+proj=utm +zone=10+datum=WGS84")

#large part of HI
#raster_after = rasterFromXYZ(binned_effort2[[1]])
#proj4string(raster_after) = CRS("+proj=utm +zone=10+datum=WGS84")

#hi = merge(raster_before, raster_after)

#z = binned_effort[[1]]


##################################################

#DATAFRAME ARITHMETIC/MERGING FOR DIFFERENCE PLOT#-----------
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


##############COMMENT OUT IF TESTING#########################----------
#save into an Rdata file  
#file_name = paste("../data/BeforeAfterEffort/", graph_title, "BeforeAfterEffort.Rdata", sep = '')
#save(graph_title, shapefile, shapefile_mpa, binned_effort , file = file_name) 
############################################################


############
# PLOTTING # -------
############
####################
# Before-After Plot#
################### 
###########MANUALLY CHANGING OLD MPA##########
if(territory == "Hawaii" ) {
  print('transforming hawaii shapefiles')
  b2 = curr_mpa$geometry + c(-180, 0)         
  b3 = mpa_before$geometry + c(-180.4, -0.26)
} else if (mpa_name == 'Pacific Remote Islands') {
  print('transforming pacific remote shapefiles')
  b2 = curr_mpa$geometry + c(-180, 0) 
  b3 = curr_eez$geometry + c(-180, 0) 
}

effortPlot = list()

#PLOT 1----------------

  if(territory=="Hawaii" || mpa_name=="Pacific Remote Islands") {
    yo = binned_effort[[1]]
    yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
    yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
  }
#change to yo2 instad of binned_effort[[k]] for Hawaii or PRI-----
  effortPlot[[1]] = binned_effort[[1]] %>% 
    filter(fishing_hours > 1) %>% 
    ungroup() %>% 
    ggplot()+
    #change to lon2 and b2 and b3 vs lon_bin_center curr_mpa, curr_eez
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
    geom_sf(data = curr_mpa,
            fill = NA,
            col = "red",
            size = 0.5
    ) +
    geom_sf(data = curr_eez,
            fill = NA,
            col = "black",
            linetype= 'dashed',
            size = 0.5
    ) +
    viridis::scale_fill_viridis(name = "Fishing hours" ,
                                trans = "log",
                                breaks = scales::log_breaks(n = 10, base = 4))+
    hrbrthemes::theme_ipsum()+
    labs(#title = graph_title,
         #subtitle = subtitles[[k]],
         y = "",
         x = "")+
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          #plot.title = element_text(size = 18, hjust = 0),
          #plot.subtitle = element_text(size = 16, hjust = 0),
          plot.margin = margin(10,0,0,0),
          legend.margin = margin(5,5,20,0))
  #+
    
#for pipa only -----------    
    #scale_x_discrete(breaks = c(-178,-173, -168))
    #scale_x_discrete(breaks = c(-134,-130, -125,-120))


quartz()
effortPlot[[1]]


#PLOT 2----------------

  if(territory=="Hawaii" || mpa_name=="Pacific Remote Islands") {
    yo = binned_effort[[2]]
    yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
    yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
  }
#change to yo2 instad of binned_effort[[k]] for Hawaii or PRI-----
effortPlot[[2]] = binned_effort[[2]] %>% 
  filter(fishing_hours > 1) %>% 
  ungroup() %>% 
  ggplot()+
  #change to lon2 and b2 and b3 vs lon_bin_center curr_mpa, curr_eez
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
  geom_sf(data = curr_mpa,
          fill = NA,
          col = "red",
          size = 0.5
  ) +
  geom_sf(data = curr_eez,
          fill = NA,
          col = "black",
          linetype= 'dashed',
          size = 0.5
  ) +
  viridis::scale_fill_viridis(name = "Fishing hours" ,
                              trans = "log",
                              breaks = scales::log_breaks(n = 10, base = 4))+
  hrbrthemes::theme_ipsum()+
  labs(title = graph_title,
       #subtitle = subtitles[[k]],
       y = "",
       x = "")+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        #plot.title = element_text(size = 18, hjust = 0),
        #plot.subtitle = element_text(size = 16, hjust = 0),
        plot.margin = margin(10,0,0,0),
        legend.margin = margin(5,5,20,0))
#+



#for pipa only -----------    
#scale_x_discrete(breaks = c(-178,-173, -168))
#scale_x_discrete(breaks = c(-134,-130, -125,-120))

quartz()
effortPlot[[2]]


#to see what it looks like
quartz()
grid.arrange(effortPlot[[1]], effortPlot[[2]], nrow=2)



#########################
# Effort Difference Plot#--------
#########################
#scale---------------------------
scale = viridis(100)[10:100]

#name the file here
file = paste(mpa_name, ' Effort and Difference', sep='')
title = paste('../Figures/Effort_and_Difference/', file, '.png', sep = '')   

#transform
if(territory=='Hawaii' || mpa_name = "Pacific Remote Islands") {
  yo = binned_effort[[3]]
  yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
  yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
} else {
  yo = binned_effort[[3]]
}

#make extremes the same color for 180 mpas
higher = 500
lower = -higher
#change yo2 to binned_effort[[3]]
for(i in 1:nrow(yo2)) {
  x=yo2$fishing_hours[i]
  if(x < lower) {
    yo2$fishing_hours[i] = lower
  } else if (x > higher) {
    yo2$fishing_hours[i] = higher
  } 
}

#make extremes same color for normal MPAs
higher = 750
lower = -higher
for(i in 1:nrow(yo)) {
  x=yo$fishing_hours[i]
  if(x < lower) {
    yo$fishing_hours[i] = lower
  } else if (x > higher) {
    yo$fishing_hours[i] = higher
  } 
}

#for pitcairn, make 150, nazca 250??
higher = 250
lower = -higher
for(i in 1:nrow(yo)) {
  x=yo$fishing_hours[i]
  if(x < lower) {
    yo$fishing_hours[i] = lower
  } else if (x > higher) {
    yo$fishing_hours[i] = higher
  } 
}

#REAL CODE for effortplot 3 BELOW ------------------------------------
#part1----------
#change to yo2 if Hawaii, yo for regular mpas
  effortPlot[[3]] = yo %>%
  filter(abs(fishing_hours) > 1) %>% 
  ungroup() %>% 
  ggplot()+
  #change to lon_bin_center
  geom_raster(aes(x =lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
  geom_sf(data = curr_mpa, #MPA or b2
          fill = NA,
          col = "red",
          size = 0.5
  ) +
  geom_sf(data = curr_eez, #EEZ or b3
          fill = NA,
          col = "black",
          linetype= 'dashed',
          size = 0.5
  ) +
  

# SCALE PART HERE ------ 
scale_fill_gradient2(name="Difference", low = "blue", mid="white", high = "red" ,
<<<<<<< HEAD
                    limits = c(-750,750),
                    breaks=seq(-750,750,250)
                    #breaks=seq(lower,higher,100)  
=======
                    #limits = c(-500,500),
                    #breaks=seq(lower,higher,250)
                    breaks=seq(lower,higher,100)  
>>>>>>> parent of 12a0692... more figures
                   )+
#part3----
  hrbrthemes::theme_ipsum()+
  labs(#title = "After-Before Difference",
       #subtitle = paste(year_before_date, "to", year_after_date, sep= ' '),
       y = "",
       x = "")+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 18, hjust = 0),
        plot.subtitle = element_text(size = 16, hjust = 0),
        plot.margin = margin(10,0,0,0),
        legend.margin = margin(5,5,20,0),
        panel.background=element_rect(fill = "white",
                                      colour = NA,
                                      size = 0.5, linetype = "solid")
        ) #+
    #scale_x_discrete(breaks = c(-178,-173, -168))
   #scale_x_discrete(breaks = c(-134,-130, -125,-120))

quartz() 
effortPlot[[3]]





#SAVE PLOT PICTURES TO FOLDER HERE ----------------
png('notitle7', units="in", width=4, height=4, res=300)
grid.arrange(effortPlot[[1]], nrow=1)
dev.off()

png('notitle8', units="in", width=4, height=4, res=300)
grid.arrange(effortPlot[[2]], nrow=1)
dev.off()

png('notitle9', units="in", width=4, height=4, res=300)
grid.arrange(effortPlot[[3]], nrow=1)
dev.off()





