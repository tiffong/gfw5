######LIBRARIRES -----------
setwd('/Users/tiffanyong/Documents/GitHub/gfw4/s')
#setwd('/Users/timwhite/gfw4/s') # changed for Tim's computer
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

######INITIALIZING WORKSPACE------------------
cleanSql = function(sql){
  sql <- gsub("\n","",sql) 
  sql <- gsub("\t","",sql)
  sql
}

#get shapefile of MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
eez <- readOGR(dsn = "/Users/tiffanyong/Documents/GitHub/gfw/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox
# FOR TIM
eez <- readOGR(dsn = "/Users/timwhite/Dropbox/AIS/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox

#eez@data = eez@data[which(is.na(eez@data$Sovereign1) == F),] # remove a single NA value for disputed Chinese land, is this necessary

mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")


eezs = c('Tuvaluan Exclusive Economic Zone', 'Tokelau Exclusive Economic Zone',
        'American Samoa Exclusive Economic Zone', 'Samoan Exclusive Economic Zone',
        'Wallis and Futuna Exclusive Economic Zone', 'Kiribati Exclusive Economic Zone (Gilbert Islands)',
        'Kiribati Exclusive Economic Zone (Line Islands)', 'Cook Islands Exclusive Economic Zone',
        'French Polynesian Exclusive Economic Zone', 'Clipperton Exclusive Economic Zone',
        'Ecuadorian Exclusive Economic Zone (Galapagos)', 'United States Exclusive Economic Zone (Hawaii)',
        'Marshall Islands Exclusive Economic Zone', 'Tongan Exclusive Economic Zone',
        'Fijian Exclusive Economic Zone', 'Nauruan Exclusive Economic Zone', 'Vanuatu Exclusive Economic Zone',
        'Solomon Islands Exclusive Economic Zone', 'New Caledonian Exclusive Economic Zone',
        'Papua New Guinean Exclusive Economic Zone', 'Micronesian Exclusive Economic Zone',
        'Palau Exclusive Economic Zone', 'Costa Rican Exclusive Economic Zone',
        'Peruvian Exclusive Economic Zone')

#for Chile
geoname  = 'Chilean Exclusive Economic Zone'

#TODO: set mpa name/creation date/mpant ------
###########VISUALIZING THE MPA#############
# creation_date = '2016-08-26	'
# mpa_name = 'Papahānaumokuākea Marine National Monument'


for (i in 6:length(eezs)) {
  print (paste("EEZ no. ", i, sep=''))
  print(eezs[i])


  geoname = eezs[[i]]
  #print(geoname)
  # curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ] 
  # curr_mpa = spTransform(curr_mpa, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  
  #CHECK---------
  curr_eez = eez[eez@data$GeoName == geoname,]
  num = curr_eez@data$MRGID
  mrgid = as.character(num)
  print(mrgid)
  
  curr_eez = spTransform(curr_eez, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  
  #vizualize the MPA
  leaflet() %>%
    addTiles() %>%
    #addPolygons(data = curr_mpa, color ="green") %>%
    addPolygons(data = curr_eez, color ="blue") 
  
  ######SETTING YEAR BEFORE/AFTER/SUBTITLES---------- 
  #year_before_date = as.Date(creation_date) %m+% years(-1)
  #year_after_date = as.Date(creation_date) %m+% years(1)
  
  #4 YEARS TIME
  year_before_date = as.Date('2014-01-01')
  year_after_date = as.Date('2018-01-01')
  
  subtitle = paste(as.character(as.Date(year_before_date), "%B %Y"), "-", as.character(as.Date(year_after_date), "%B %Y"), sept = "")
  
  
  months = seq(as.Date(year_before_date), by = "month", length.out = 49) #get 2 years of data
  len = length(months)-1
  
  monthly_effort_list = list()
  
  #transform shapefile into sf object for plotting 
  curr_eez <- curr_eez %>% 
    sf::st_as_sf()  
  
######QUERY PATH WITH MPANT ID--------
  
  for(i in 1:len) {
    project <- "gfw-tiff" 
    #project <- "world-fishing-827" # FOR TIM
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
      r'eez:",mrgid,"')
  
      GROUP BY
      lat_bin_center,
      lon_bin_center"
      , sep = "")
    
    sql <- cleanSql(sql)
    
    total_effort <- query_exec(sql,project, use_legacy_sql = FALSE)
    sum_effort = sum(total_effort$fishing_hours)
    print(paste("iteration: ", i, sep=''))
    print(sum_effort)
    
    monthly_effort_list[[i]] = sum_effort
  }
  
#######################################################################
  
######MAKING THE DATA FRAME - done a very janky way -----------
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
  months_o_year = seq(as.Date(year_before_date), by = "month", length.out = 48)
  
  finaldf
  
##############SAVING TO AN R DATA FILE#########################
  #file_name = paste("../data/monthly_effort_CONTROL/", mpa_name, '/', geoname, ".Rdata", sep = '')
  file_name = paste("../data/monthly_effort_CONTROL_4YEAR_new/", geoname, ".Rdata", sep = '')
  
  file_name
  save(months_o_year, finaldf, geoname, file = file_name) 
###############################################################
  
######PLOTTING---------------
  title = paste('../Figures/CONTROL_EEZs_4YEAR/', geoname, '.png', sep = '')
  png(title, units="in", width=13, height=7, res=300)
  
  #quartz()
  ggplot(finaldf, aes(months_o_year, finaldf$fishing_effort,group=1)) + 
    
    geom_point(size=0.1,stroke = 0, shape = 16) + #makes the points disappear
    geom_line(size=0.2) +
  
    labs(x="Month", y="Monthly Fishing Effort", 
         title = geoname, 
         subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal() +
    scale_x_date(date_breaks = "3 months",
                 date_labels = "%b %y") +
    geom_vline(aes(xintercept = months_o_year[[13]], color='dodgerblue'), color="dodgerblue", size=0.4) +
    geom_vline(aes(xintercept = months_o_year[[25]], color='dodgerblue'), color="dodgerblue", size=0.4) +
    geom_vline(aes(xintercept = months_o_year[[37]], color='dodgerblue'), color="dodgerblue", size=0.4)
  
  
  dev.off()

} #starting at i = 6 because we already ran the first 5 MPAs to test functionality
