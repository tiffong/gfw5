#plot individual heatmaps from Rdata
setwd('/Users/tiffanyong/Documents/GitHub/GFW5/s')

library(sf)
library(mregions)
library(DBI)
library(bigrquery)
devtools::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)
library(ggsci)
library(tidyverse) #loads dplyr and friends
require(ggplot2) #there is no package called ggplot
library(hrbrthemes)
instlibrary(viridisLite)
library(rgdal)
library(dplyr)
require(dplyr)
require(sp)
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
library(gdtools)

##INITIALIZING##
#get shapefiles for EEZ - TAKES TIME
eez <- readOGR(dsn = "/Users/tiffanyong/Documents/GitHub/gfw5/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox
eez@data = eez@data[which(is.na(eez@data$Sovereign1) == F),] # remove a single NA value for disputed Chinese land, is this necessary

#get shapefiles for MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_Apr2019_marine-shapefile-polygons")

###########################

#SET TERRITORY,MPA NAME,AND CREATION DATE HERE
creation_date = '2016-08-24'
territory = 'Islas San Félix and San Ambrosio'
mpa_name = 'Nazca-Desventuradas'

curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ] 
curr_eez = eez[eez@data$Territory1 == territory,] # get a specific EEZ


########VISUALIZE SHAPEFILE#######

leaflet() %>%
  addTiles() %>%
  addPolygons(data = curr_mpa, color ="green") %>%
  #addPolygons(data = mpa_before, color = "blue") 
  #%>%
  addPolygons(data = curr_eez, color = "blue") 

###############################


#transform shapefile into sf object for plotting 
curr_mpa <- curr_mpa %>% 
  sf::st_as_sf() 
print('mpa as sf')

if(territory != 'Hawaii') {
  print('eez as sf')
  curr_eez <- curr_eez %>% 
    sf::st_as_sf() 
}

if(territory == "Hawaii") {
  print('mpa_before')
  mpa_before <- mpa_before %>% 
    sf::st_as_sf() 
}

####LOAD  RDATA########################

files = list.files(path = paste("../data/BeforeAfterEffort/", sep=''), pattern = NULL, all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

files #check filenames here

len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() 
names = list()
for(i in 3:len) {
  eez = files[[i]]
  file_name = paste("../data/BeforeAfterEffort/", eez,  sep = "")
  load(file_name)
  Rdata[[i-2]] = binned_effort
  names[[i-2]] = graph_title
}

##check what is loaded
View(Rdata)
View(names)

####load data locally####
binned_effort = Rdata[[1]][[1]]
#View(binned_effort)

effortPlot = list()

###############PLOT 1----------------

if(territory=="Hawaii" || mpa_name=="Pacific Remote Islands") {
  yo = binned_effort[[1]]
  yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
  yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
}
#change to yo2 instad of binned_effort[[k]] for Hawaii or PRI-----
effortPlot[[1]] = binned_effort %>% 
  filter(fishing_hours > 1) %>% 
  ungroup() %>% 
  ggplot()+
  #change to lon2 and b2 and b3 vs lon_bin_center curr_mpa, curr_eez
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
  geom_sf(data = curr_mpa,
          fill = NA,
          col = "red",
          size = 0.5
  ) +
  geom_sf(data = curr_eez,
          fill = NA,
          col = "black",
          linetype= 'dashed',
          size = 0.5
  ) +
  viridis::scale_fill_viridis(name = "Fishing hours" ,
                              trans = "log",
                              breaks = scales::log_breaks(n = 6, base = 4))+
  #hrbrthemes::theme_ipsum()+
  theme_minimal()+
  labs(#title = graph_title,
    #subtitle = subtitles[[k]],
    y = "",
    x = "")+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))#,
        #plot.title = element_text(size = 18, hjust = 0),
        #plot.subtitle = element_text(size = 16, hjust = 0),
        #plot.margin = margin(10,0,0,0),
        #legend.margin = margin(5,5,20,0))

quartz()
effortPlot[[1]]













#PLOT 2----------------

if(territory=="Hawaii" || mpa_name=="Pacific Remote Islands") {
  yo = binned_effort[[2]]
  yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
  yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
}
#change to yo2 instad of binned_effort[[k]] for Hawaii or PRI-----
effortPlot[[2]] = binned_effort[[2]] %>% 
  filter(fishing_hours > 1) %>% 
  ungroup() %>% 
  ggplot()+
  #change to lon2 and b2 and b3 vs lon_bin_center curr_mpa, curr_eez
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
  geom_sf(data = curr_mpa,
          fill = NA,
          col = "red",
          size = 0.5
  ) +
  geom_sf(data = curr_eez,
          fill = NA,
          col = "black",
          linetype= 'dashed',
          size = 0.5
  ) +
  viridis::scale_fill_viridis(name = "Fishing hours" ,
                              trans = "log",
                              breaks = scales::log_breaks(n = 10, base = 4))+
  hrbrthemes::theme_ipsum()+
  labs(title = graph_title,
       #subtitle = subtitles[[k]],
       y = "",
       x = "")+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        #plot.title = element_text(size = 18, hjust = 0),
        #plot.subtitle = element_text(size = 16, hjust = 0),
        plot.margin = margin(10,0,0,0),
        legend.margin = margin(5,5,20,0))
#+



#for pipa only -----------    
#scale_x_discrete(breaks = c(-178,-173, -168))
#scale_x_discrete(breaks = c(-134,-130, -125,-120))

quartz()
effortPlot[[2]]


#to see what it looks like
quartz()
grid.arrange(effortPlot[[1]], effortPlot[[2]], nrow=2)



#########################
# Effort Difference Plot#--------
#########################
#scale---------------------------
scale = viridis(100)[10:100]

#name the file here
file = paste(mpa_name, ' Effort and Difference', sep='')
title = paste('../Figures/Effort_and_Difference/', file, '.png', sep = '')   

#transform
if(territory=='Hawaii' || mpa_name = "Pacific Remote Islands") {
  yo = binned_effort[[3]]
  yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
  yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
} else {
  yo = binned_effort[[3]]
}

#make extremes the same color for 180 mpas
higher = 500
lower = -higher
#change yo2 to binned_effort[[3]]
for(i in 1:nrow(yo2)) {
  x=yo2$fishing_hours[i]
  if(x < lower) {
    yo2$fishing_hours[i] = lower
  } else if (x > higher) {
    yo2$fishing_hours[i] = higher
  } 
}

#make extremes same color for normal MPAs
higher = 750
lower = -higher
for(i in 1:nrow(yo)) {
  x=yo$fishing_hours[i]
  if(x < lower) {
    yo$fishing_hours[i] = lower
  } else if (x > higher) {
    yo$fishing_hours[i] = higher
  } 
}

#for pitcairn, make 150, nazca 250??
higher = 250
lower = -higher
for(i in 1:nrow(yo)) {
  x=yo$fishing_hours[i]
  if(x < lower) {
    yo$fishing_hours[i] = lower
  } else if (x > higher) {
    yo$fishing_hours[i] = higher
  } 
}

#REAL CODE for effortplot 3 BELOW ------------------------------------
#part1----------
#change to yo2 if Hawaii, yo for regular mpas
effortPlot[[3]] = yo %>%
  filter(abs(fishing_hours) > 1) %>% 
  ungroup() %>% 
  ggplot()+
  #change to lon_bin_center
  geom_raster(aes(x =lon_bin_center, y = lat_bin_center, fill = fishing_hours))+
  geom_sf(data = curr_mpa, #MPA or b2
          fill = NA,
          col = "red",
          size = 0.5
  ) +
  geom_sf(data = curr_eez, #EEZ or b3
          fill = NA,
          col = "black",
          linetype= 'dashed',
          size = 0.5
  ) +
  
  
  # SCALE PART HERE ------ 
scale_fill_gradient2(name="Difference", low = "blue", mid="white", high = "red" ,

                     limits = c(-750,750),
                     breaks=seq(-750,750,250)

)+
  #part3----
hrbrthemes::theme_ipsum()+
  labs(#title = "After-Before Difference",
    #subtitle = paste(year_before_date, "to", year_after_date, sep= ' '),
    y = "",
    x = "")+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 18, hjust = 0),
        plot.subtitle = element_text(size = 16, hjust = 0),
        plot.margin = margin(10,0,0,0),
        legend.margin = margin(5,5,20,0),
        panel.background=element_rect(fill = "white",
                                      colour = NA,
                                      size = 0.5, linetype = "solid")
  ) #+
#scale_x_discrete(breaks = c(-178,-173, -168))
#scale_x_discrete(breaks = c(-134,-130, -125,-120))

quartz() 
effortPlot[[3]]





#SAVE PLOT PICTURES TO FOLDER HERE ----------------
#png('nazca1', units="in", width=4, height=4, res=300)
#grid.arrange(effortPlot[[1]], nrow=1)
#dev.off()

ggsave("nazca3.png", width = 4, height = 4)

png('notitle8', units="in", width=4, height=4, res=300)
grid.arrange(effortPlot[[2]], nrow=1)
dev.off()

png('notitle9', units="in", width=4, height=4, res=300)
grid.arrange(effortPlot[[3]], nrow=1)
dev.off()
