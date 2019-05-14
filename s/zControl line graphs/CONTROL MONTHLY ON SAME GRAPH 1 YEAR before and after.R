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

#plots monthly fishing effort a year before and after MPA creation and puts it against control EEZs

#getting the names of the files in the folder-------

mpa_name = 'Phoenix Islands Protected Area'
mpa_name = 'Pitcairn Islands Marine Reserve'
mpa_name = 'Papahānaumokuākea Marine National Monument'
mpa_name = 'Pacific Remote Islands'
mpa_name = 'Nazca-Desventuradas'

#GET Rdata-------
files = list.files(path = paste("../data/monthly_effort_CONTROL/", mpa_name, sep=''), pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
files #check filenames here

len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() 
for(i in 1:len) {
  eez = files[[i]]
  file_name = paste("../data/monthly_effort_CONTROL/", mpa_name, '/', eez,  sep = "")
  load(file_name)
  Rdata[[i]] =  finaldf
}


#Load the MPA's data
#phoneix = 4
#papa=3
#pitcairn = 5
#pacific remote islands = 2
#nazca = 2


#plot------
plots = list()
thick = 0.5


###PIPA####-----
# Gilbert 1053239.7, Line 1640932.6, Phoenix 745700.8
Rdata_temp = Rdata
Area = c(1053239.7,1053239.7,1053239.7, 1640932.6, 745700.8,745700.8,745700.8,745700.8,745700.8)
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}

plots[[1]] = 
#quartz()
ggplot(Rdata_temp[[1]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  #geom_line(aes(color="American Samoa"), size = thick+0.2, linetype='dashed') +
  #geom_line(data=Rdata[[2]], (aes(color="Cook Islands")), size = thick) +
  geom_line(data=Rdata_temp[[3]], (aes(color="Kiribati (Gilbert Islands)")), size = thick) +
  geom_line(data=Rdata_temp[[4]], (aes(color="Kiribati (Line Islands)")), size = thick) +
  geom_line(data=Rdata_temp[[5]], (aes(color="Phoenix Islands Protected Area")), size = thick+0.5, color="black") +
  #geom_line(data=Rdata[[5]], (aes(color="Samoan")), size = thick)  +
  #geom_line(data=Rdata[[6]], (aes(color="Tokelau")), size = thick)  +
  #geom_line(data=Rdata[[7]], (aes(color="Tuvaluan")), size = thick)  +
  #geom_line(data=Rdata[[8]], (aes(color="Wallis and Futuna")), size = thick)  +

  labs(color="EEZ") +
  labs(x="Month" ,
       y="Fishing Hours",
       title = "Phoenix Islands"
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()


#PITCAIRN----
#Cook 1969507, french polynesian 4766884, pitcairn 842344.8

Rdata_temp = Rdata
Area = c(1969507,1969507,1969507, 4766884, 842344.8)
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}

plots[[2]] = 
#quartz()
ggplot(Rdata_temp[[1]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  #geom_line(aes(color="Clipperton"), size = thick) +
  geom_line(data=Rdata_temp[[2]], (aes(color="Cook Islands")), size = thick) +
  geom_line(data=Rdata_temp[[4]], (aes(color="French Polynesian")), size = thick) +
  geom_line(data=Rdata_temp[[5]], (aes(color="Pitcairn")), size = thick+0.5, color="black") +
  
  labs(color="EEZ") +
  labs(x="Month", 
       y="Fishing Hours",
       title = "Pitcairn Islands"
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()


  

#PAPA-----
#Kiribati Line 1640933, Papa 1508870, US Hawaii 2474714

Rdata_temp = Rdata
Area = c(1640933,1640933,1508870, 2474714)
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}


plots[[3]] = 
#quartz()
ggplot(Rdata_temp[[1]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  geom_line(aes(color="Kiribati (Line Islands)"), size = thick) +
  #geom_line(data=Rdata[[2]], (aes(color="Marshall Islands")), size = thick) +
  geom_line(data=Rdata_temp[[3]], (aes(color="Papahānaumokuākea")), size = thick+0.5, color='black') +
  geom_line(data=Rdata_temp[[4]], (aes(color="US Hawaii")), size = thick) +

  
  labs(color="EEZ") +
  labs(x="Month", 
       y="Fishing Hours" ,
       title = "Papahānaumokuākea"
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()

#PRI-----
# Gilbert 1053239.7, Line 1640932.6, PRI 1277784 (some area from eez data)

Rdata_temp = Rdata
Area = c(1053239.7,1640932.6,1640932.6, 1277784)
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}


plots[[4]] =
#quartz()
ggplot(Rdata_temp[[3]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  geom_line(aes(color="Kiribati (Gilbert Islands)"), size = thick) +
  geom_line(data=Rdata_temp[[1]], (aes(color="Kiribati (Line Islands)")), size = thick) +
  geom_line(data=Rdata_temp[[2]], (aes(color="Kiribati (Line Islands)")), size = thick) +
  geom_line(data=Rdata_temp[[4]], (aes(color="Pacific Remote Islands")), size = thick+0.2, color = "black") +
 
  
  labs(color="EEZ") +
  labs(x="Month", 
       y="Fishing Hours",
       title = mpa_name
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()


#Nazca-----
#Chilean 2465054.7, Nazca (gis_rep_area) 300585.1, Peruvian 855322.3

Rdata_temp = Rdata
Area = c(2465054.7,300585.1,855322.3)
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}

plots[[5]] =
  #quartz()
  ggplot(Rdata[[1]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  geom_line(aes(color="Chilean"), size = thick) +
  geom_line(data=Rdata[[3]], (aes(color="Peruvian")), size = thick) +
  geom_line(data=Rdata[[2]], (aes(color="Nazca")), size = thick+0.2, color = "black") +
  
  labs(color="EEZ") +
  labs(x="Month", 
       y="Fishing Hours",
       title = mpa_name
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))


#dev.off()


#gridarrange----
quartz()
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],nrow=3)

