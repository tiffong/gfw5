#libraries -------------
#setwd('/Users/tiffanyong/Desktop/backup_gfw/backupgfw2/s')
setwd('/Users/tiffanyong/Documents/GitHub/GFW3/s')
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
library(gridExtra) #for grid arrange
library(png)
library(grid)
library(gridExtra)
###############
#rm(list=ls())# protecc with life
###############


#stitches together pngs of figure 2 (MPAs with control EEzs around them)

#get heat maps Rdata ------
files = list.files(path = paste("../Figures/Old Figures/Old - Control Sites sorted by MPA/MPA and Control sites plotted together/", sep=''), pattern = NULL, all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
files #check filenames here
length = length(files)


#REAL
plot1 <- readPNG(files[[1]])
plot2 <- readPNG(files[[2]])
plot3 <- readPNG(files[[3]])
plot4 <- readPNG(files[[4]])
plot5 <- readPNG(files[[5]])

#plot together pngs
margin = theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))
quartz()
grid.arrange(rasterGrob(plot1,width = unit(2,"in"), height=unit(2,"in")), 
             rasterGrob(plot2, width = unit(2,"in"), height=unit(2,"in")), 
             rasterGrob(plot3, width = unit(2,"in"), height=unit(2,"in")), 
             rasterGrob(plot4, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot5, width = unit(2,"in"), height=unit(2,"in")),
             ncol=2, widths=c(.5,.5,.5))



#CHECKIGN TO SEE data on  MPAS GREATER THAN 70K
curr_eez = mpa[mpa@data$NO_TAKE == 'All',]
curr_eez = mpa[mpa@data$REP_M_AREA > 70000,]
curr_eez = mpa[mpa@data$REP_M_AREA < 100000,]


View(curr_eez)

