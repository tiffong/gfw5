#4 YEAR PERCENT CHANGE FROM YEAR TO YEAR

#libraries -----
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
library(scales)
library(reshape)
library(tm)
library(ggfortify) #for ts

#getting the names of the files in the folder-------

files = list.files(path = paste("../data/monthly_effort_CONTROL_4YEAR/", sep=''), pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
files #check filenames here

len = length(files) #how many Rdatafiles are in the folder 

#FOR name of each EEZ
legend = list()
stopwords = c(' Exclusive Economic Zone.Rdata', ' Exclusive Economic Zone (Gilbert Islands).Rdata',
              ' Exclusive Economic Zone (Line Islands).Rdata', 'Exclusive Economic Zone (Hawaii).Rdata')
for (i in 1:len) {
  legend[[i]] = removeWords(files[[i]], stopwords)
}

#STORE RDATA INTO LIST
Rdata = list() 
for(i in 1:len) {
  eez = files[[i]]
  file_name = paste("../data/monthly_effort_CONTROL_4YEAR/", eez,  sep = "")
  load(file_name)
  Rdata[[i]] =  finaldf
}

#get yearly effort for each eez
yearly_effort = list()
for(i in 1:len) {
  fishing_effort = Rdata[[i]]$fishing_effort
  
  #create new dataframe
  df = data.frame( year = 1:4, 
                   effort = 1:4)
  df$effort[1] = sum(fishing_effort[1:12])
  df$effort[2] = sum(fishing_effort[13:24])
  df$effort[3] = sum(fishing_effort[25:36])
  df$effort[4] = sum(fishing_effort[37:48])
  
  yearly_effort[[i]] = df
  
}

#get percent change for each eez
pchange = list()
for(i in 1:len) {
  fishing_effort = yearly_effort[[i]]$effort
  
  #create new dataframe
  df_change = data.frame( year = 1:4, 
                          #effort = fishing_effort,
                          change = 1:4)
  
  df_change$change[1] = 0 
  df_change$change[2] = -(fishing_effort[1]-fishing_effort[2]) /  fishing_effort[1]*100
  df_change$change[3] = -(fishing_effort[2]-fishing_effort[3]) /  fishing_effort[2]*100
  df_change$change[4] = -(fishing_effort[3]-fishing_effort[4]) /  fishing_effort[3]*100
  
  pchange[[i]] = df_change
  
}


#PLOTTING PERCENT CHANGE TO VISUALIZE 

