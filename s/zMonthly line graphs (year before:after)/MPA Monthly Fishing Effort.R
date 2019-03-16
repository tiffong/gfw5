#libraries -----------
setwd('/Users/tiffanyong/Documents/GitHub/gfw3/s')
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
library(scales) #for plotting
library(leaflet) #for visualization
library(lubridate) #for date 

######INITIALIZING WORKSPACE##############
cleanSql = function(sql){
  sql <- gsub("\n","",sql) 
  sql <- gsub("\t","",sql)
  sql
}

#get shapefile of MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")


#TODO: set mpa name/creation date/mpant ------
###########VISUALIZING THE MPA#############
creation_date = '2016-08-26'
mpa_name = 'Papahānaumokuākea Marine National Monument'
curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ]
mpant = '8338'

#vizualize the MPA
leaflet() %>%
  addTiles() %>%
  addPolygons(data = curr_mpa, color ="green")

##########SETTING YEAR BEFORE/AFTER/SUBTITLES#################  
year_before_date = as.Date(creation_date) %m+% years(-1)
year_after_date = as.Date(creation_date) %m+% years(1)

subtitle = paste(as.character(as.Date(year_before_date), "%B %Y"), "-", as.character(as.Date(year_after_date), "%B %Y"), sept = "")
months = seq(as.Date(year_before_date), by = "month", length.out = 25) #get 2 years of data
len = length(months)-1

monthly_effort_list = list()

#transform shapefile into sf object for plotting 
curr_mpa <- curr_mpa %>% 
  sf::st_as_sf()  

#######QUERY PATH 1 with bbox and rasterization############
# get the bounding box of the shapefile
bbox <- sf::st_bbox(curr_mpa)

# # extend the bounding box 1 degree in every direction just for safety of the query
min_lon <- bbox[["xmin"]] - 1
max_lon <- bbox[["xmax"]] + 1
min_lat <- bbox[["ymin"]] - 1
max_lat <- bbox[["ymax"]] + 1

resolution <- 0.1

for(i in 1:len) {
  date1 = months[[i]]
  date2 = months[[i+1]]

  #query data
  project <- "gfw-tiff" # project name 

  sql  = paste(
    "SELECT
    FLOOR(lon/0.1)*0.1 + 0.5*0.1 lon_bin_center,     
    FLOOR(lat/0.1)*0.1 + 0.5*0.1 lat_bin_center,
    SUM(IF(nnet_score2 > .5, hours,0)) fishing_hours,
    mmsi
    
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
    lon_bin_center,
    lat_bin_center,
    mmsi
    "
    , sep = "")
  
  sql <- gsub("\n","",sql)
  sql <- gsub("\t","",sql)
  
  total_effort <- query_exec(sql,project, use_legacy_sql = FALSE)
  #store into variable called "total_effort"
  
  #rasterize
  raster_effort = rasterFromXYZ(total_effort)
  projection(raster_effort) <- CRS("+proj=longlat +datum=WGS84")
  #plot(raster_effort)
  #plot(curr_mpa,add=TRUE)
  masked_effort = mask(raster_effort,curr_mpa)
  
  masked_effort = as.data.frame(masked_effort)
  masked_effort[is.na(masked_effort)] <- 0
  sum_effort = sum(masked_effort$fishing_hours)
  
  monthly_effort_list[[i]] = sum_effort
  print(paste("iteration: ", i, sep=''))
  print(paste("sum: ", sum_effort,sep=''))
}

# hi = 0
# for(i in 1:11){
#   hi = hi+ monthly_effort_list[[i]]  
# }
# hi

######QUERY PATH 2 WITH MPANT ID  --------

for(i in 1:len) {
  project <- "gfw-tiff" 
  
  date1 = months[[i]]
  date2 = months[[i+1]]
  
  sql  = paste(
    "SELECT
    FLOOR(lat/0.1)*0.1 + 0.5*0.1 lat_bin_center,
    FLOOR(lon/0.1)*0.1 + 0.5*0.1 lon_bin_center,
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

    AND REGEXP_CONTAINS(regions,
    r'mpant:",mpant,"')

    GROUP BY
    lat_bin_center,
    lon_bin_center"
    , sep = "")
  
  sql <- gsub("\n","",sql)
  sql <- gsub("\t","",sql)
  
  total_effort <- query_exec(sql,project, use_legacy_sql = FALSE)
  sum_effort = sum(total_effort$fishing_hours)
  print(paste("iteration: ", i, sep=''))
  print(sum_effort)
  
  monthly_effort_list[[i]] = sum_effort
}

#######################################################################

#MAKING THE DATA FRAME - done a very janky way -----------
month_names = list() #X values of plot
for (i in 1:len) {
  date = format(months[[i]], format="%b %y")
  month_names[[i]] = date
}

#getting month names into a dataframe 
df2 = data.frame(Reduce(rbind, month_names))
names(df2)[names(df2)== colnames(df2) ] <- "Months"

#getting fishing effort into a dataframe
df = data.frame(Reduce(rbind, monthly_effort_list))
names(df)[names(df)== colnames(df) ] <- "Monthly Effort"

finaldf = data.frame("date" = df2$Months,
                    "fishing_effort" = df$`Monthly Effort`)

finaldf$date <- factor(finaldf$date, levels = finaldf$date)
#finaldf$monthly_effort <- factor(finaldf$monthly_effort, levels = finaldf$monthly_effort)
#to keep R from reformatting

#GET 24 MONTHS FOR X AXIS
months_o_year = seq(as.Date(year_before_date), by = "month", length.out = 24)

finaldf

##############SAVING TO AN R DATA FILE#########################
file_name = paste("../data/monthly_effort_scatterplot/", mpa_name, ".Rdata", sep = '')
save(mpa_name,  months_o_year, finaldf, file = file_name) 
###############################################################

##########PLOTTING###########
title = paste('../Figures/monthly_scatterplot/', mpa_name, '.png', sep = '')
png(title, units="in", width=13, height=7, res=300)

#quartz()
ggplot(finaldf, aes(months_o_year, finaldf$fishing_effort,group=1)) + 
  
  geom_point(size=0.1,stroke = 0, shape = 16) + #makes the points disappear
  geom_line(size=0.2) +

  labs(x="Month", y="Monthly Fishing Effort", 
       title = mpa_name, 
       subtitle = subtitle) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %y") +
  geom_vline(aes(xintercept = months_o_year[[13]]), color="dodgerblue", size=0.3)

dev.off()