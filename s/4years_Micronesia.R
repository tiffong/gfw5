#4 YEAR CONTROL EEZS, AREA CONTROLLED ON SAME GRAPH

#libraries -----
setwd('/Users/tiffanyong/Documents/GitHub/gfw5/s')
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
#library(rwdpa) #TO FIND SHAPEFILES POSSIBLY!!!!
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
library(stats) #for moving average



#getting the names of the files in the folder-------

files = list.files(path = paste("../data/monthly_effort_CONTROL_4YEAR/", sep=''), pattern = NULL, all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
files #check filenames here

len = length(files) #how many Rdatafiles are in the folder 

#titles for legend
legend = list()
stopwords = c(' Exclusive Economic Zone.Rdata', ' Exclusive Economic Zone (Gilbert Islands).Rdata',
              ' Exclusive Economic Zone (Line Islands).Rdata', 'Exclusive Economic Zone (Hawaii).Rdata')
for (i in 1:len) {
  legend[[i]] = removeWords(files[[i]], stopwords)
}

legend[[1]] = 'Micronesia'


#SUBTITLE FOR GGPLOT
year_before_date = as.Date('2014-01-01')
year_after_date = as.Date('2018-01-01')

subtitle = paste(as.character(as.Date(year_before_date), "%B %Y"), "-", as.character(as.Date(year_after_date), "%B %Y"), sept = "")
months_o_year = seq(as.Date(year_before_date), by = "month", length.out = 48)
#months_o_year<-seq(as.Date("2014-01-01"),as.Date("2017-12-01"),by="months")
#Format Dates
months_o_year<-as.POSIXct(months_o_year)



#STORE INTO LIST
#stores time series smoothed average into Rdata.avg list
#Rdata is monthly effort for different EEZs  


#loading micronesia data
Rdata = list() 

eez = files[[10]]
file_name = paste("../data/monthly_effort_CONTROL_4YEAR/", eez,  sep = "")
load(file_name)
Rdata[[1]] =  finaldf

#PLOTTING
plots = list()


title = paste('../Figures/Final_Figures/', 'MICRONESIA', '.png', sep = '')
png(title, units="in", width=11, height=4, res=300)
thick = 1

quartz()
ggplot(Rdata[[1]], aes(x=months_o_year, y=fishing_effort, group=1)) + 
  geom_line(data=Rdata[[1]]) +
  theme_minimal() +
  
  labs(color="EEZ Name") +
  labs(x=" ", 
       y= 'Fishing Hours',
       title = 'Fishing Hours in Micronesia by Month'
       #subtitle = '2014-2018'
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  
  theme(axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 30, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0) ,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 18)
  )




dev.off()
