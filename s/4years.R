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

files = list.files(path = paste("../data/monthly_effort_CONTROL_4YEAR_new/", sep=''), pattern = NULL, all.files = FALSE,
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

legend[[1]] = 'Chile'
legend[[2]] = 'Cook Islands'
legend[[3]] = 'French Polynesia'
legend[[4]] = 'Kiribati (Gilbert)'
legend[[5]] = 'Kiribati (Line)'
legend[[6]] = 'Peru'
legend[[7]] = 'United States (Hawaii)'


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


Rdata = list() 
for(i in 1:len) {
  eez = files[[i]]
  file_name = paste("../data/monthly_effort_CONTROL_4YEAR_new/", eez,  sep = "")
  load(file_name)
  Rdata[[i]] =  finaldf
}

#area control
#Pitcairn #Cook 1969507, french polynesian 4766884, pitcairn 842344.8
# Gilbert 1053239.7, Line 1640932.6, Phoenix 745700.8
#Kiribati Line 1640933, Papa 1508870, US Hawaii 2474714
#Chilean 2465054.7, Nazca (gis_rep_area) 300585.1, Peruvian 855322.3

Area = c(2465054.7,1969507,4766884, 1053239.7, 1640933,855322.3, 2474714)
#Area = c(1053239.7,1053239.7,1053239.7, 1640932.6, 745700.8,745700.8,745700.8,745700.8,745700.8) #PIPA
Rdata_temp = Rdata
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}


#FOR VLINES I THINK
mpa_dates = c(as.integer(as.POSIXct('2015-01-01')),
              as.integer(as.POSIXct('2015-09-16')),
              as.integer(as.POSIXct('2016-08-24')),
              as.integer(as.POSIXct('2016-08-26'))
              )


###PLOTTING------------------------
start = 5700

#this is for the points
x = as.Date(c('2015-01-01',#pipa
              '2015-09-16', #pitcairn
              #'2016-08-24',
              #'2016-08-26',
              '2014-09-25'#pri
              ))

#this is for the geom_text Mpa names, can make it whatever value
x2 = as.Date(c(
  '2014-09-25', #PRI
  '2015-01-01', #PIPA
  '2015-09-16' #Pitcairn
  #'2016-09-30', #Papa, need to add a bunch
  #'2017-12-01' #Ross Sea
))

x3 = as.Date(c('2016-08-20')) #for Nazca becasue it is a little raised
yval = c(4100)
yval_area_controlled = c(30)
name_nazca = c('Nazca')

#for papa's text placement
x_papa =  as.Date(c('2016-09-07'))
yval_papa = c(4050)
yval_papa = c(30050)
yval_papa = c(15)
yval_papa = c(30)

name_papa = c('PNM')

yval_2= c(4000,4000,4000) #FOR MPA CIRCLES
yval_med = c(15000,15000,15000)
yval_large = c(30000,30000,30000)

#df = data.frame(x2,yval) #for regular other 4 MPAs
#df2 = data.frame(x3, yval) #for Nazca
#df_papa = data.frame(x_papa, yval_papa)

df = data.frame(x2,yval_area_controlled) #for regular other 4 MPAs
df2 = data.frame(x3, yval_area_controlled) #for Nazca
df_papa = data.frame(x_papa, yval_papa)

names = c('PRI', 'PIPA','Pitcairn') #FOR GEOM_TEXT


#########################################################
######################4 years###################
#########################################################
plots = list()

#SMALL FISHING ------
#name = "SMALL_Avg"
title = paste('../Figures/Final_Figures/', '4year2', '.png', sep = '')
png(title, units="in", width=11, height=4, res=300)
thick = 1

#quartz()
#plots[[1]] =  
ggplot(Rdata[[1]], aes(x=months_o_year, y=fishing_effort, group=1)) + 
  geom_line(data=Rdata_temp[[1]], (aes(color=legend[[1]])), size = thick) +  
  geom_line(data=Rdata_temp[[2]], (aes(color=legend[[2]])), size = thick) + #clipperton +
  geom_line(data=Rdata_temp[[3]], (aes(color=legend[[3]])), size = thick) + #clipperton+
  geom_line(data=Rdata_temp[[4]], (aes(color=legend[[4]])), size = thick) + #clipperton
  geom_line(data=Rdata_temp[[5]], (aes(color=legend[[5]])), size = thick) + #clipperton
  geom_line(data=Rdata_temp[[6]], (aes(color=legend[[6]])), size = thick) + #clipperton
  geom_line(data=Rdata_temp[[7]], (aes(color=legend[[7]])), size = thick) + 
  
  theme_minimal() +
  geom_text(data = df, (aes(x=x2, y=yval_area_controlled)), label= names, hjust=1,vjust=1.5, angle=30, size = 5.5) + #4 regular MPAs
  geom_text(data = df2, (aes(x=x3-35, y=yval_area_controlled)), label= name_nazca, hjust=1,vjust=1.5, check_overlap = TRUE, angle=30, size = 5.5) + #Nazca
  geom_text(data = df2, (aes(x=x_papa+9, y=yval_papa)), label= name_papa, hjust=1,vjust=1.5, check_overlap = TRUE, angle=33, size = 5.5) + #papa  
  
  geom_point(data = df,(aes(x=x, y=yval_area_controlled)),group= 1, shape = 21, colour = "black", fill = "white", size=5) + #4 regular MPAs
  geom_point(data = df2,(aes(x=x3+4, y=30)),group= 1, shape = 21, colour = "black", fill = "white", size=5) + #Nazca
  geom_point(data = df2,(aes(x=x3+7, y=30.2)),group= 1, shape = 21, colour = "black", fill = "white", size=5) + #Papa
  
  
  labs(color="EEZ Name") +
  labs(x=" ", 
       y= expression("Fishing Hours/1000" ~km^2),
       title = 'Fishing Hours by Month'
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
