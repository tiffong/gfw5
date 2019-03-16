#4 YEAR CONTROL ON SAME GRAPH

#libraries -----
setwd('/Users/tiffanyong/Documents/GitHub/gfw4/s')
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


legend = list()
stopwords = c(' Exclusive Economic Zone.Rdata', ' Exclusive Economic Zone (Gilbert Islands).Rdata',
              ' Exclusive Economic Zone (Line Islands).Rdata', 'Exclusive Economic Zone (Hawaii).Rdata')
for (i in 1:len) {
  legend[[i]] = removeWords(files[[i]], stopwords)
}

#SUBTITLES
year_before_date = as.Date('2014-01-01')
year_after_date = as.Date('2018-01-01')

subtitle = paste(as.character(as.Date(year_before_date), "%B %Y"), "-", as.character(as.Date(year_after_date), "%B %Y"), sept = "")
months_o_year = seq(as.Date(year_before_date), by = "month", length.out = 48)
#months_o_year<-seq(as.Date("2014-01-01"),as.Date("2017-12-01"),by="months")
#Format Dates
months_o_year<-as.POSIXct(months_o_year)


#STORE INTO LIST
Rdata = list() 
for(i in 1:len) {
  eez = files[[i]]
  file_name = paste("../data/monthly_effort_CONTROL_4YEAR/", eez,  sep = "")
  load(file_name)
  Rdata[[i]] =  finaldf
}

mpa_dates = c(as.integer(as.POSIXct('2015-01-01')),
              as.integer(as.POSIXct('2015-09-16')),
              as.integer(as.POSIXct('2016-08-24')),
              as.integer(as.POSIXct('2016-08-26')),
              as.integer(as.POSIXct('2014-09-25')),
              as.integer(as.POSIXct('2017-12-01')))


###PLOTTING------------------------
start = 5700

x = as.Date(c('2015-01-01',
              '2015-09-16',
              '2016-08-24',
              '2016-08-26',
              '2014-09-25'))

x2 = as.Date(c('2015-01-01',
              '2015-09-16',
              '2016-08-24',
              '2016-08-26',
              '2014-09-25',
              '2017-12-01'))

yval= c(3000, 3000,3000,3000,3000)
df = data.frame(x,yval)
names = c('PIPA','Pitcairn','Nazca','Papa','Pacific Remote Islands')

#SMALL FISHING------
name = "SMALL"
title = paste('../Figures/CONTROL_EEZs_4YEAR/eezs_plotted_together/', name, '.png', sep = '')
png(title, units="in", width=15, height=5, res=300)
thick = 0.5

#quartz()
ggplot(Rdata[[1]], aes(x=months_o_year, y=fishing_effort, group=1)) + 

#geom_lines--------- 
  geom_line(data=Rdata[[2]], (aes(color=legend[[2]])), size = thick) +
  geom_line(data=Rdata[[4]], (aes(color=legend[[4]])), size = thick) +
  geom_line(data=Rdata[[23]], (aes(color=legend[[23]])), size = thick) +
  geom_line(data=Rdata[[6]], (aes(color=legend[[6]])), size = thick) +
  geom_line(data=Rdata[[19]], (aes(color=legend[[19]])), size = thick) +
  geom_point(data = df,(aes(x=x, y=yval)), size = 1 ,group=1) +
#title----  
  geom_text(data = df, (aes(x=x, y=yval)), label= names, hjust=1,vjust=1.5, check_overlap = TRUE, angle=30, size = 3) +
#circles-------
# ggplot(Rdata[[1]], aes(x=months_o_year, y=fishing_effort, group=1)) +
#   geom_point() +
#   geom_point(aes(x=as.integer(as.POSIXct('2015-09-16')), y=5000), colour="blue")

#rest------
  labs(color="EEZ Name") +
  labs(x="Year", 
       y="Monthly Fishing Effort", 
       title = "Monthly Fishing Effort", 
       subtitle = subtitle) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0)) 
dev.off()

#MIDDLE FISHING------

name = 'MIDDLE'
title = paste('../Figures/CONTROL_EEZs_4YEAR/eezs_plotted_together/', name, '.png', sep = '')
png(title, units="in", width=15, height=5, res=300)
thick = 0.5

#quartz()
ggplot(Rdata[[6]], aes(x=months_o_year, y=fishing_effort, group=1)) + 
  geom_line(data=Rdata[[16]], (aes(color=legend[[16]])), size = thick) +
  geom_line(data=Rdata[[12]], (aes(color=legend[[12]])), size = thick) +
  geom_line(data=Rdata[[13]], (aes(color=legend[[13]])), size = thick) +
  geom_line(data=Rdata[[11]], (aes(color=legend[[11]])), size = thick) +

  theme_minimal() +
  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y="Monthly Fishing Effort", 
       title = "Monthly Fishing Effort", 
       subtitle = subtitle
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing

  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

dev.off()

#MED-LARGE--------
name = 'MED-LARGE'
title = paste('../Figures/CONTROL_EEZs_4YEAR/eezs_plotted_together/', name, '.png', sep = '')
png(title, units="in", width=15, height=5, res=300)
thick = 0.5

#quartz()
ggplot(Rdata[[6]], aes(x=months_o_year, y=fishing_effort, group=1)) + 

  
  geom_line(data=Rdata[[18]], (aes(color=legend[[18]])), size = thick) +
  
  geom_line(data=Rdata[[3]], (aes(color=legend[[3]])), size = thick) +
  geom_line(data=Rdata[[5]], (aes(color=legend[[5]])), size = thick) +
  geom_line(data=Rdata[[15]], (aes(color=legend[[15]])), size = thick) +

  
  theme_minimal() +
  
  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y="Monthly Fishing Effort", 
       title = "Monthly Fishing Effort", 
       subtitle = subtitle
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
 
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

dev.off()

#LARGE--------
name = 'LARGE'
title = paste('../Figures/CONTROL_EEZs_4YEAR/eezs_plotted_together/', name, '.png', sep = '')
png(title, units="in", width=15, height=5, res=300)
thick = 0.5

#quartz()
ggplot(Rdata[[6]], aes(x=months_o_year, y=fishing_effort, group=1)) + 
  theme_minimal() +
  geom_line(data=Rdata[[9]], (aes(color=legend[[9]])), size = thick) +
  geom_line(data=Rdata[[21]], (aes(color='US (Hawaii)')), size = thick) +
  geom_line(data=Rdata[[14]], (aes(color=legend[[14]])), size = thick) +
  geom_line(data=Rdata[[20]], (aes(color=legend[[20]])), size = thick) +
  geom_line(data=Rdata[[10]], (aes(color=legend[[10]])), size = thick) +
  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y="Monthly Fishing Effort", 
       title = "Monthly Fishing Effort", 
       subtitle = subtitle
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

dev.off()


#XLARGE--------
name = 'XLARGE'
title = paste('../Figures/CONTROL_EEZs_4YEAR/eezs_plotted_together/', name, '.png', sep = '')
png(title, units="in", width=15, height=5, res=300)
thick = 0.5

#quartz()
ggplot(Rdata[[6]], aes(x=months_o_year, y=fishing_effort, group=1)) + 
  theme_minimal() +
  geom_line(data=Rdata[[8]], (aes(color='Kiribati (Line)')), size = thick) +

  geom_line(data=Rdata[[17]], (aes(color=legend[[17]])), size = thick) +
  geom_line(data=Rdata[[22]], (aes(color=legend[[22]])), size = thick) +
  geom_line(data=Rdata[[7]], (aes(color='Kiribati (Gilbert)')), size = thick) +
  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y="Monthly Fishing Effort", 
       title = "Monthly Fishing Effort", 
       subtitle = subtitle
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

dev.off()

