#libraries -----
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
library(reshape)
library(readr)#for loading packages
###############
#rm(list=ls())# protecc with life
###############

#getting the names of the files in the folder-------
mpa_name = 'Papahānaumokuākea Marine National Monument'

files = list.files(path = paste("../data/Redistribution/", mpa_name, '/', sep = ''), pattern = NULL, all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
files #check filenames here

len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() 
for(i in 1:len) {
  data = files[[i]]
  file_name = paste("../data/Redistribution/", mpa_name, '/', data,  sep = "")
  load(file_name)
  Rdata[[i]] = yo2
}

#PLOTTING-------
plot = list()
thick = 0.4

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
#actual plotting here------
plot[[i]] = 
    #quartz() 
    Rdata[[1]][[i]] %>% 
    filter(fishing_hours > 1) %>% 
    ungroup() %>% 
    ggplot()+
    geom_raster(aes(x = lon2, y = lat_bin_center, fill = fishing_hours))+
    viridis::scale_fill_viridis(name = "Fishing hours" ,
                                trans = "log",
                                breaks = scales::log_breaks(n = 10, base = 4))+
  # geom_sf(data = b4,
  #         fill = NA,
  #         col = "black",
  #         size = 0.5
  # ) +  
  
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
     ) #+
     #scale_x_continuous(limits = c(-21, 29)) + #these limits are for PIPA
     #scale_y_continuous(limits = c(-14, 12))
    # 
   #scale_x_discrete(limits = c(-30,65)) +
   #scale_y_discrete(limit = c(-20,15))
}

#save picture---------
png(title, units="in", width=13, height=7, res=300)
quartz()
grid.arrange(plot[[1]], plot[[2]], nrow=2)
dev.off()
