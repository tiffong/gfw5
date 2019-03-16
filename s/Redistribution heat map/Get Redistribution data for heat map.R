#load libraries -------------
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

#Initializing workspace/getting shapefiles --------------------------------------------------

#get shapefiles for EEZ - TAKES TIME
eez <- readOGR(dsn = "/Users/tiffanyong/Documents/GitHub/gfw/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox
eez@data = eez@data[which(is.na(eez@data$Sovereign1) == F),] # remove a single NA value for disputed Chinese land, is this necessary

#get shapefile of MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")


#TODO: set MPA/eez/creation date/mpant/percentile here ----
#VISUALIZING THE MPA--------------------------------------------------------
creation_date = '2016-08-26'
mpa_name = 'Papahānaumokuākea Marine National Monument'
territory = 'Hawaii'
curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ]
curr_eez = eez[eez@data$Territory1 == territory, ]

mpa_before = mpa[mpa@data$ORIG_NAME == 'Papahānaumokuākea', ]

mpant = '8338'
top = TRUE
percentile = 1

#vizualize the MPA
leaflet() %>%
  addTiles() %>%
  addPolygons(data = curr_mpa, color ="green") %>%
  addPolygons(data = curr_eez, color ="blue") %>%
  addPolygons(data = mpa_before, color ="blue")

if (territory == "Hawaii") { #wraps it at the 0
  curr_mpa = spTransform(curr_mpa, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  curr_eez = spTransform(curr_eez, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  mpa_before = spTransform(mpa_before, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
}

curr_mpa <- curr_mpa %>% 
  sf::st_as_sf()  

curr_eez <- curr_eez %>% 
  sf::st_as_sf()  

mpa_before <- mpa_before %>% 
  sf::st_as_sf() 


#Sets year before and after and graph subtitles ------------------------------------------  

year_before_date = as.Date(creation_date) %m+% years(-1)
year_after_date = as.Date(creation_date) %m+% years(1)
print(paste(year_before_date, creation_date , year_after_date,sep = ' ---- ' )) #print check right dates

subtitles = list()

subtitles[[1]]= paste(as.character(as.Date(year_before_date), "%B %Y"),
                      "-", as.character(as.Date(creation_date), 
                                        "%B %Y"), sep = "")


subtitles[[2]]= paste(as.character(as.Date(creation_date), "%B %Y"),
        "-", as.character(as.Date(year_after_date), 
           "%B %Y"), sep = "")

#Query: getting ALL mmsi names within MPA---------------------------------------

#query for names of top fishing mmsi boats within PIPA
#this is a year BEFORE MPA closing
date1 = year_before_date
date2 = creation_date
project <- "gfw-tiff" 

sql  = paste(
  "
SELECT
  distinct mmsi,
  SUM(IF(nnet_score2 > .5, hours,0)) hours
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
  group by
  mmsi"
  , sep = "")

sql <- gsub("\n","",sql)
sql <- gsub("\t","",sql)

distinct_boats <- query_exec(sql,project, use_legacy_sql = FALSE)

#make dataframe
dt = data.table(distinct_boats)
# dt2 <- dt[,list(sumamount = sum(hours), freq = .N), by = c("mmsi")]
dt2 = dt[hours>0]
dt2 = dt2[with(dt2, order(-hours)),] 

#dt2 = all the mmsis within MPA a year before closing

#Get mmsi of top or bottom 10%, 25% of boats in MPA ----------------------------------

if(top == TRUE) {
  top_fishers = dt2[dt2$hours >= quantile(dt2$hours, percentile) ]
  nrow = nrow(top_fishers)
} else {
  top_fishers = dt2[dt2$hours <= quantile(dt2$hours, 1-percentile) ]
  nrow = nrow(top_fishers)
}

#getting new df with mmsi AND rank
b = data.frame(matrix(nrow=nrow,ncol=1))
colnames(b) <- c("mmsi")
b$mmsi =  top_fishers$mmsi
b$rank = 1:nrow

#FOR LOOP START IS RIGHT HERE DO NOT MISS IT -------------------------------------
merged_list = list()
for (j in 1:2) {
  date1 = year_before_date
  date2 = creation_date
  
  if(j==2) {
    date1 = creation_date
    date2 = year_after_date
  }

#Query: overall lat, lon, and fishing_hours from the the top/bottom % of fishers in MPA---------------------------------

mmsi_list = list()
for (i in 1:nrow) {
    print(paste("iteration ", j, ": ",  i, sep="" ))
    mmsi_num = b[i, "mmsi"]
    
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
      and mmsi = ",mmsi_num,"
      AND HOURS>0
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
      GROUP BY
      lat_bin_center,
      lon_bin_center"
      , sep = "")

    sql <- gsub("\n","",sql)
    sql <- gsub("\t","",sql)
    
    mmsi_list[[i]] = query_exec(sql,project, use_legacy_sql = FALSE)
}

#Dataframe Arithmetic: summing ALL effort from top/bottom x% into one df called MERGED  ---------------------------
merged = merge(mmsi_list[[1]], mmsi_list[[2]], by=c('lat_bin_center', 'lon_bin_center'), all=TRUE)
merged[is.na(merged)] <- 0
merged = data.frame(lat_bin_center=merged$lat_bin_center, 
                    lon_bin_center=merged$lon_bin_center, 
                    fishing_hours=merged$fishing_hours.x+merged$fishing_hours.y)


for(i in 3:nrow) {
  merged = merge(merged, mmsi_list[[i]], by=c('lat_bin_center', 'lon_bin_center'), all=TRUE)
  merged[is.na(merged)] <- 0
  merged = data.frame(lat_bin_center=merged$lat_bin_center,
                      lon_bin_center=merged$lon_bin_center,
                      fishing_hours=merged$fishing_hours.x+merged$fishing_hours.y)
}

#FOR LOOP ENDS HERE DO NOT MISS IT----------------------------------------
  merged_list[[j]] = merged
}

#Fixing 180 line for everyone--------------------------------------------
yo2 = list()
for(k in 1:2) {
    yo = merged_list[[k]]
    yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
    yo2[[k]] = yo[,c("lat_bin_center","lon2","fishing_hours")]
}


#Plotting Redistribution Heat Map ----------------------------------
# plot = list()
# thick = 0.4
# status = "before"
b2 = curr_mpa$geometry + c(-180, 0) 
#b3 = curr_eez$geometry + c(-180, 0) 
b3 = mpa_before$geometry + c(-180.4, -0.26) #for Hawaii
b4 = curr_eez$geometry + c(-180, 0) 


#Saving top/bottom x% total effort into R data file -------------------------------------------------------------
if(top == TRUE) {
  #before closure
  file_name = paste("../data/Redistribution/", mpa_name, '/', 'top', (1-percentile)*100, ".Rdata", sep = '')
  save(yo2, b2, b3, subtitles, percentile, top, file=file_name)
  
} else {
  file_name = paste("../data/Redistribution/", mpa_name, '/', 'bottom', (1-percentile)*100, ".Rdata", sep = '')
  save(yo2, b2, b3, subtitles, percentile,top, file=file_name)
  
}






####REFER TO OTHER REDISTRIBUTION FILE TO PLOT


#PLOTTING BEGINS HERE----
for(i in 1:2) {
#title shenanigans--------------
  if(top == TRUE) {
    #top
    graph_title = paste(mpa_name, ": Distribution of Top ", (1-percentile)*100, "% Boats",  sep = '')
    title = paste('../Figures/Redistribution/',mpa_name, "/", "top", (1-percentile)*100, 'Distribution', '.png', sep = '')
  
  } else {
    #bottom
    graph_title = paste(mpa_name, ": Distribution of Bottom ", (1-percentile)*100, "% Boats",  sep = '')
    title = paste('../Figures/Redistribution/',mpa_name, "/", "bottom", (1-percentile)*100, 'Distribution', '.png', sep = '')
  }
#plotting here------
  
  plot[[i]] = 
  #quartz() 
    yo2[[i]] %>% 
    filter(fishing_hours > 1) %>% 
    ungroup() %>% 
    ggplot()+
    geom_raster(aes(x = lon2, y = lat_bin_center, fill = fishing_hours))+
    viridis::scale_fill_viridis(name = "Fishing hours" ,
                                  trans = "log",
                                  breaks = scales::log_breaks(n = 10, base = 4))+
    geom_sf(data = b2,
            fill = NA,
            col = "red",
            size = 0.5
    ) +
    geom_sf(data = b3,
            fill = NA,
            col = "black",
            size = 0.5
    ) +
    hrbrthemes::theme_ipsum()+
    labs(title = graph_title,
         subtitle = subtitles[[i]],
         y = "",
         x = "")+
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0, vjust = -2),
          plot.subtitle = element_text(size = 10, hjust = 0),
          plot.margin = margin(0,0,0,0),
          legend.margin = margin(5,5,20,0)
     ) +
    scale_x_continuous(limits = c(-21, 29)) +
    scale_y_continuous(limits = c(-14, 12))
  
   # scale_x_discrete(limits = c(-30,65)) #for Hawaii
  #+
     #scale_y_discrete(limit = c(-20,15))
}

#save picture---------
png(title, units="in", width=13, height=7, res=300)
quartz()
grid.arrange(plot[[1]], plot[[2]], nrow=2)
dev.off()
