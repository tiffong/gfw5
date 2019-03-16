#libraries -------------
#setwd('/Users/tiffanyong/Desktop/backup_gfw/backupgfw2/s')
setwd('/Users/tiffanyong/Documents/GitHub/GFW4/Figures/Individual_plots_samemargin')
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


#get heat maps Rdata ------
files = list.files(path = paste(".", sep=''), pattern = NULL, all.files = FALSE,
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
plot6 <- readPNG(files[[6]])
plot7 <- readPNG(files[[7]])
plot8 <- readPNG(files[[8]])
plot9 <- readPNG(files[[9]])
plot10 <- readPNG(files[[10]])
plot11 <- readPNG(files[[11]])
plot12 <- readPNG(files[[12]])
plot13 <- readPNG(files[[13]])
plot14 <- readPNG(files[[14]])
plot15 <- readPNG(files[[15]])


margin = theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))

png(filename="../Final_Figures/Figure4.png", units="in", width=6, height=9, res=300)

quartz()
grid.arrange(rasterGrob(plot1,width = unit(2,"in"), height=unit(2,"in")), 
             rasterGrob(plot2, width = unit(2,"in"), height=unit(2,"in")), 
             rasterGrob(plot3, width = unit(2,"in"), height=unit(2,"in")), 
             rasterGrob(plot4, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot5, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot6, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot7, width = unit(2,"in"), height=unit(2,"in")), 
             rasterGrob(plot8, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot9, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot10, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot11, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot12, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot13, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot14, width = unit(2,"in"), height=unit(2,"in")),
             rasterGrob(plot15, width = unit(2,"in"), height=unit(2,"in")),
             ncol=3, widths=c(.5,.5,.5))

dev.off()

