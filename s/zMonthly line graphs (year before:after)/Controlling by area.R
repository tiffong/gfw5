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

#getting the names of the files in the folder-------
files = list.files(path = "../data/monthly_effort_scatterplot/", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
files #check filenames here

len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() 
for(i in 1:len) {
  mpa_name = files[[i]]
  file_name = paste("../data/monthly_effort_scatterplot/", mpa_name,  sep = "")
  load(file_name)
  Rdata[[i]] =  finaldf
}

Rdata_temp = Rdata
Area = c(300035, 1277860, 1508870, 397447, 834334, 1651714)
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]]
}


###PLOTTING####-----
name = "5 MPAs Area controlled"
title = paste('../Figures/monthly_scatterplot/monthly_control/', name, '.png', sep = '')
png(title, units="in", width=10, height=7, res=300)
thick = 0.5

#quartz()
ggplot(Rdata_temp[[1]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  geom_line(aes(color="Nazca-Desventuradas"), size = thick+0.2, linetype='dashed') +
  geom_line(data=Rdata_temp[[2]], (aes(color="Pacific Remote Islands")), size = thick) +
  geom_line(data=Rdata_temp[[3]], (aes(color="Papahānaumokuākea")), size = thick) +
  #geom_line(data=Rdata_temp[[4]], (aes(color="Phoenix Islands Protected Area")), size = thick) +
  geom_line(data=Rdata_temp[[5]], (aes(color="Pitcairn")), size = thick)  +
  geom_line(data=Rdata_temp[[6]], (aes(color="Ross Sea")), size = thick)  +

  labs(color="MPA name") +
  labs(x="Month", 
       y="Monthly Fishing Effort", 
       title = "Monthly Fishing Effort", 
       subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

dev.off()

