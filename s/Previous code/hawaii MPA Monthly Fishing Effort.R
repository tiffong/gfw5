#libraries -----------
setwd('/Users/tiffanyong/Desktop/backup_gfw/backupgfw2/s')
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

#View(mpa@data)

#states <-readOGR(dsn='cb_2017_us_state_500k',layer='cb_2017_us_state_500k')
#curr_mpa = states[states@data$NAME == "Hawaii", ]

#TODO: set mpa name/creation date/mpant ------
###########VISUALIZING THE MPA#############
creation_date = '2016-08-24'
mpa_name = "Nazca-Desventuradas"
curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ]
mpant = '9175'

#ross sea
#curr_mpa = mpa[mpa@data$NAME == mpa_name,]
#great barrier reef
#curr_mpa = curr_mpa[curr_mpa$NO_TAKE == 'All', ]

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


# get the bounding box of the shapefile
bbox <- sf::st_bbox(curr_mpa)

# # extend the bounding box 1 degree in every direction just for safety of the query
min_lon <- bbox[["xmin"]] - 1
max_lon <- bbox[["xmax"]] + 1
min_lat <- bbox[["ymin"]] - 1
max_lat <- bbox[["ymax"]] + 1

#transform shapefile into sf object for mpa 
curr_mpa <- curr_mpa %>% 
  sf::st_as_sf()  

#######QUERY PATH 1 with bbox and rasterization############

resolution <- 0.1

for(i in 1:len) {
  date1 = months[[i]]
  date2 = months[[i+1]]
  
  #query data
  project <- "gfw-tiff" # project name 
  
  sql = paste("SELECT
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
  
  and lat >= ",min_lat,"
  AND lat <= ",max_lat,"
  AND lon BETWEEN ",min_lon," and ",max_lon,"
  
  GROUP BY
  lat_bin_center,
  lon_bin_center", sep = '')
  
  sql <- gsub("\n","",sql)
  sql <- gsub("\t","",sql)
  
  total_effort <- query_exec(sql,project, use_legacy_sql = FALSE, max_pages= Inf)
  #store into variable called "total_effort"
  
  #rasterize
  total_effort = total_effort[c("lon_bin_center","lat_bin_center","fishing_hours")]
  raster_effort = rasterFromXYZ(total_effort)
  projection(raster_effort) <- CRS("+proj=longlat +datum=WGS84")
  
  #testing plotting
  plot(raster_effort)
  #plot(curr_mpa,add=TRUE)
  
  masked_effort = mask(raster_effort,curr_mpa)
  plot(masked_effort)
  
  masked_effort = as.data.frame(masked_effort)
  masked_effort[is.na(masked_effort)] <- 0
  sum_effort = sum(masked_effort$fishing_hours)
  print(date1)
  print(date2)
  print(paste("hours: ", sum_effort, sep=''))
  
  monthly_effort_list[[i]] = sum_effort
  print(paste("iteration: ", i, sep=''))
}

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
  
  total_effort <- query_exec(sql,project, use_legacy_sql = FALSE)
  sum_effort = sum(total_effort$fishing_hours)
  print(paste("iteration",i, sep=''))
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

##############SAVING TO AN R DATA FILE#########################
file_name = paste("../data/zzzzzzztest_for_hawaii_monthly/", mpa_name, ".Rdata", sep = '')
save(finaldf, file = file_name) 
###############################################################

##this section is for the individual boats
#loading stuff
files = list.files(path = "../data/zzzzzzztest_for_hawaii_monthly/", pattern = NULL, all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() #list of 

for(i in 1:len) {
  file = files[[i]]
  file_name = paste("../data/zzzzzzztest_for_hawaii_monthly/", file,  sep = "")
  load(file_name)
  
  if (i=1) {
   large = finaldf
  } else {
    small = finaldf
  }
}

sum = data.frame(matrix(nrow = 24, ncol=1))
sum[is.na(sum)] <- 0
colnames(sum) = c('bruh')

for(i in 1:24) {
  sum$bruh[i] = small$fishing_effort[i] + large$fishing_effort[i]
}


#sum of first 12 months
summ=0
for(i in 1:12) {
  summ=summ+ sum$bruh[i]
}
summ
#first 12 months of Hawaii is 1469.541 bruh

##########PLOTTING###########
title = paste('../Figures/monthly_scatterplot/', mpa_name, '.png', sep = '')
png(title, units="in", width=13, height=7, res=300)

#quartz()
ggplot(finaldf, aes(months_o_year, finaldf$fishing_effort ,group=1)) + 
  
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

