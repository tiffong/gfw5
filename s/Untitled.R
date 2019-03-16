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

#SUBTITLE FOR GGPLOT
year_before_date = as.Date('2014-01-01')
year_after_date = as.Date('2018-01-01')

subtitle = paste(as.character(as.Date(year_before_date), "%B %Y"), "-", as.character(as.Date(year_after_date), "%B %Y"), sept = "")
months_o_year = seq(as.Date(year_before_date), by = "month", length.out = 48)
#months_o_year<-seq(as.Date("2014-01-01"),as.Date("2017-12-01"),by="months")
#Format Dates
months_o_year<-as.POSIXct(months_o_year)


order = 5
#STORE INTO LIST
#stores time series smoothed average into Rdata.avg list
#Rdata is monthly effort for different EEZs  
Rdata = list() 
Rdata.avg = list()
for(i in 1:len) {
  eez = files[[i]]
  file_name = paste("../data/monthly_effort_CONTROL_4YEAR/", eez,  sep = "")
  load(file_name)
  Rdata[[i]] =  finaldf
  ma.10 = stats::filter(finaldf,filter=rep(1/order,order))
  
  Rdata.avg[[i]] = as.data.frame(ma.10[,2])
}

#FOR VLINES I THINK
mpa_dates = c(as.integer(as.POSIXct('2015-01-01')),
              as.integer(as.POSIXct('2015-09-16')),
              as.integer(as.POSIXct('2016-08-24')),
              as.integer(as.POSIXct('2016-08-26')),
              as.integer(as.POSIXct('2014-09-25'))
              )


###PLOTTING------------------------
start = 5700

#this is for the points
x = as.Date(c('2015-01-01',
              '2015-09-16',
              #'2016-08-24',
              #'2016-08-26',
              '2014-09-25',
              '2017-12-01'))

#this is for the geom_text Mpa names, can make it whatever value
x2 = as.Date(c(
  '2014-09-25', #PRI
  '2015-01-01', #PIPA
  '2015-09-16', #Pitcairn
  #'2016-09-30', #Papa, need to add a bunch
  '2017-12-01' #Ross Sea
))

x3 = as.Date(c('2016-08-20')) #for Nazca becasue it is a little raised
yval = c(4100)
name_nazca = c('Nazca')


x_papa =  as.Date(c('2016-09-07'))
yval_papa = c(4050)
name_papa = c('PNM')

yval_2= c(4000,4000,4000, 4000) #FOR MPA CIRCLES
yval_med = c(15000,15000,15000,15000)
yval_large = c(30000,30000,30000,30000)

df = data.frame(x2,yval) #for regular other 4 MPAs
df2 = data.frame(x3, yval) #for Nazca
df_papa = data.frame(x_papa, yval_papa)
names = c('PRI', 'PIPA','Pitcairn','Ross Sea') #FOR GEOM_TEXT


#########################################################
######################RUNNING AVERAGES###################
#########################################################
plots = list()

#SMALL FISHING RUNNING AVERAGE ------
#name = "SMALL_Avg"
title = paste('../Figures/CONTROL_EEZs_4YEAR/eezs_plotted_together/Running_Avg/', 'smalltest', '.png', sep = '')
png(title, units="in", width=8, height=3, res=300)
thick = 1

quartz()
#plots[[1]] =  
ggplot(Rdata.avg[[6]], aes(x=months_o_year, y=x, group=1)) + 
  geom_line(data=Rdata.avg[[2]], (aes(color=legend[[2]])), size = thick) +
  geom_line(data=Rdata.avg[[4]], (aes(color=legend[[4]])), size = thick) +
  geom_line(data=Rdata.avg[[23]], (aes(color=legend[[23]])), size = thick) +
  geom_line(data=Rdata.avg[[19]], (aes(color=legend[[19]])), size = thick) +
  geom_line(data=Rdata.avg[[16]], (aes(color=legend[[16]])), size = thick) +
  
  theme_minimal() +
  geom_text(data = df, (aes(x=x2, y=yval_2)), label= names, hjust=1,vjust=1.5, angle=30, size = 5.5) + #4 regular MPAs
  geom_text(data = df2, (aes(x=x3-32, y=yval)), label= name_nazca, hjust=1,vjust=1.5, check_overlap = TRUE, angle=30, size = 5.5) + #Nazca
  geom_text(data = df2, (aes(x=x_papa+5, y=yval_papa)), label= name_papa, hjust=1,vjust=1.5, check_overlap = TRUE, angle=30, size = 5.5) + #papa  
  
  geom_point(data = df,(aes(x=x, y=yval_2)),group= 1, shape = 21, colour = "black", fill = "white", size=5) + #4 regular MPAs
  geom_point(data = df2,(aes(x=x3+4, y=4000)),group= 1, shape = 21, colour = "black", fill = "white", size=5) + #Nazca
  geom_point(data = df2,(aes(x=x3+6, y=4050)),group= 1, shape = 21, colour = "black", fill = "white", size=5) + #Papa
  
  
  labs(color="EEZ Name") +
  labs(x=" ", 
       y="Fishing Hours", 
       title = "Fishing Hours by Month" 
       #subtitle = subtitle
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  
  theme(axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 30, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0) ,
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18)
  )