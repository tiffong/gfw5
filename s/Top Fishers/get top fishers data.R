#THIS GETS THE TOP AND BOTTOM % MMSI EFFORT ON A MONTHLY SCALE BEFORE AND AFTER MPA CREATION

#libraries--------
#setwd('/Users/tiffanyong/Documents/GitHub/gfw2/s')
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
library(scales)
library(leaflet)
library(marmap) #for griddify
library(data.table) #for combining fishing hours in df
library(lubridate) #for date 

######INITIALIZING WORKSPACE##############

#get shapefile of MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")

#View(mpa@data)

##############VISUALIZING THE MPA#############

creation_date = '2016-08-26'
mpa_name = "Papahānaumokuākea Marine National Monument"
curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ]
mpant = '8338'

#vizualize the MPA
leaflet() %>%
  addTiles() %>%
  addPolygons(data = curr_mpa, color ="green")

##########GETTING THE QUERY DATA####################  

# sets year before and year after dates using lubridate
year_before_date = as.Date(creation_date) %m+% years(-1)
year_after_date = as.Date(creation_date) %m+% years(1)
 
subtitle = paste(as.character(as.Date(year_before_date), "%B %Y"),
                 "-", as.character(as.Date(year_after_date), 
                                   "%B %Y"), sept = "")


#####################################################
#query for names of top fishing mmsi boats within PIPA
#this is a year BEFORE MPA closing
date1 = year_before_date
date2 = creation_date
project <- "gfw-tiff" 

sql  = paste(
  "
SELECT
  distinct mmsi,
  hours
  FROM
  `world-fishing-827.gfw_research.nn7`
  WHERE
  MMSI>1
  AND
  HOURS > 0 
  AND
  _PARTITIONTIME BETWEEN '",date1,"'
  AND '",date2,"'
  AND REGEXP_CONTAINS(regions,
  r'mpant:",mpant,"')
  "
  , sep = "")

sql <- gsub("\n","",sql)
sql <- gsub("\t","",sql)

distinct_boats <- query_exec(sql,project, use_legacy_sql = FALSE)
#order by most hours fished
#distinct_boats = distinct_boats[with(distinct_boats, order(-hours)),] 

#combine the same data
dt = data.table(distinct_boats)
dt2 <- dt[,list(sumamount = sum(hours), freq = .N), by = c("mmsi")]
dt2 = dt2[with(dt2, order(-sumamount)),] 

#View histogram of top boats
# quartz()
# hist(dt2$sumamount)

#mmsi names of the top x boats
###
#get all boats in the 90th percentile and above
percentile = .75
#TOP fishers
#top_fishers = dt2[dt2$sumamount >= quantile(dt2$sumamount, percentile) ]
#nrow = nrow(top_fishers)

#BOTTOM fishers
top_fishers = top_fishers = dt2[dt2$sumamount <= quantile(dt2$sumamount, 1-percentile) ]
nrow = nrow(top_fishers)

#getting new df with mmsi and rank
b = data.frame(matrix(nrow=nrow,ncol=1))
colnames(b) <- c("mmsi")
b$mmsi =  top_fishers$mmsi
b$rank = 1:nrow

# ##################################################################
# #getting hours and date from the the top % of fishers in mpa######
# ##################################################################
#overall effort for top fishers 

#initialize df to 0 and iteratively add to it
thisdf3 = data.frame(matrix(nrow=24,ncol=2))
colnames(thisdf3) <- c("month","sum")
thisdf3$month = c(-12:11)
thisdf3$sum = 0 

for (i in 1:nrow) {

    mmsi_num = b[i, "mmsi"]
    rank = i
  
    sql  = paste(
      "
      SELECT
      date(timestamp) date, 
      hours
      
      FROM
      `world-fishing-827.gfw_research.nn7`
      WHERE
      mmsi = ",mmsi_num,"
      AND _PARTITIONTIME BETWEEN '2014-01-01'
      AND '2016-01-01'
      "
      , sep = "")
    
    sql <- gsub("\n","",sql)
    sql <- gsub("\t","",sql)
    
    mmsi3 = query_exec(sql,project, use_legacy_sql = FALSE)
    mmsi3$number <- format(as.Date(mmsi3$date), "%m %Y") #adds number of month as a column
    
    #get monthly sum of fishing 
    for(i in 1:nrow(mmsi3)) {
      
      month = as.integer(substr(mmsi3$number[i], 1,2))
      year = as.integer(substr(mmsi3$number[i], 4,7))
      
      if (year == year(year_before_date)) {
        thisdf3$sum[[month]] = thisdf3$sum[[month]] + as.integer(mmsi3$hours[[i]])  
      } else {
        thisdf3$sum[[month+12]] = thisdf3$sum[[month+12]] + as.integer(mmsi3$hours[[i]])    
      }
    }
}


#########################################################
#getting effort from TOP FISHERS JUST within PIPA MPA now
#########################################################

# mmsi_num = 1
# 
# sql  = paste(
#   "
#   SELECT
#   date(timestamp) date, 
#   hours
#   
#   FROM
#   `world-fishing-827.gfw_research.nn7`
#   WHERE
#   mmsi = ",mmsi_num,"
#   AND _PARTITIONTIME BETWEEN '2014-01-01'
#   AND '2016-01-01'
#   AND REGEXP_CONTAINS(regions,
#   r'mpant:7704395')
#   "
#   , sep = "")
# 
# sql <- gsub("\n","",sql)
# sql <- gsub("\t","",sql)
# 
# mmsi4 = query_exec(sql,project, use_legacy_sql = FALSE)
# mmsi4$number <- format(as.Date(mmsi4$date), "%m %Y") #adds number of month as a column
# 
# thisdf4 = data.frame(matrix(nrow=24,ncol=2))
# colnames(thisdf4) <- c("month","sum")
# thisdf4$month = c(-12:11)
# thisdf4$sum = 0 
# 
# for(i in 1:nrow(mmsi4)) {
#   
#   month = as.integer(substr(mmsi4$number[i], 1,2))
#   year = as.integer(substr(mmsi4$number[i], 4,7))
#   
#   if (year == year(year_before_date)) {
#     thisdf4$sum[[month]] = thisdf4$sum[[month]] + as.integer(mmsi4$hours[[i]])  
#   } else {
#     thisdf4$sum[[month+12]] = thisdf4$sum[[month+12]] + as.integer(mmsi4$hours[[i]])    
#   }
# }

###################


##############SAVING INDIVIDUAL AGGREGATE TO AN R DATA FILE######################
#top
#file_name = paste("../data/individual_aggregate_scatterplot/", mpa_name,  "/","top", (1-percentile)*100, ".Rdata", sep = '')

#bottom
file_name = paste("../data/individual_aggregate_scatterplot/", mpa_name, "/", "bottom", (1-percentile)*100, ".Rdata", sep = '')
save(mpa_name, thisdf3, file=file_name) 
#################################################################################


##############SAVING INDIVIDUAL EFFORT TO AN R DATA FILE######################
# file_name = paste("../data/individual_effort_scatterplot/", mpa_name, '/', rank, ".Rdata", sep = '')
# save(mpa_name, rank, thisdf3, thisdf4, file=file_name) 
##############################################################################



# #####PLOTTING INDIVIDUAL EFFORT WITH KIRIBATI LINE#######
#for the line under the individual plot

# thick = 0.4
# 
# title = paste('../Figures/aaaatop_mmsi_effort/', rank, "_", mmsi_num, '.png', sep = '')
# png(title, units="in", width=13, height=7, res=300)
# 
# ggplot(data=thisdf3, aes(x=month, y=sum, group=1)) +
#   geom_line(aes(color="Total Effort"), size = thick) +
#   geom_line(data=thisdf4, (aes(color="PIPA")), size = thick)+
#   geom_point()+theme_minimal()+
#   labs(title=mmsi_num, subtitle = paste(subtitle, "-- Rank:", rank, sep = " ") , x="Month", y = "Fishing hours", color="Legend") +
#   geom_vline(aes(xintercept = 0), color="dodgerblue", size=0.3)
# 
# 
# dev.off()


#######PLOTTING AGGREGATE INDIVIDUAL#################
thick = 0.4

#top
#title = paste('../Figures/aaaaIndividual_Aggregate/',mpa_name, "/", "top", (1-percentile)*100, '.png', sep = '')

#bottom
title = paste('../Figures/aaaaIndividual_Aggregate/',mpa_name, "/", "bottom", percentile*100, '.png', sep = '')

png(title, units="in", width=13, height=7, res=300)

#quartz()
ggplot(data=thisdf3, aes(x=month, y=sum, group=1)) +
  geom_line(aes(color="Total Effort"), size = thick) +
  geom_point()+theme_minimal()+
  labs(title=paste(mpa_name,  "--","Top",  (1-percentile)*100, "percent:", nrow, "boats"), subtitle = subtitle , x="Month", y = "Fishing hours", color="Legend") +
  geom_vline(aes(xintercept = 0), color="dodgerblue", size=0.3)

dev.off()
