#plot individual heatmaps from Rdata
setwd('/Users/tiffanyong/Documents/GitHub/GFW5/s')

library(sf)
library(mregions)
library(DBI)
library(bigrquery)
#devtools::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)
library(ggsci)
library(tidyverse) #loads dplyr and friends
require(ggplot2) #there is no package called ggplot
library(hrbrthemes)
#instlibrary(viridisLite)
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
#View(Rdata)
#View(names)


###########################

#NAZCA(1) DONE
#PRI(2)  
#PAPA(3) 
#PIPA(4) DONE
#PITCAIRN(5)DONE

##CHANGE INDEX HERE####
index = 1


#SET TERRITORY,MPA NAME
if(index ==1 ) {
  territory = 'Islas San Félix and San Ambrosio'
  mpa_name = 'Nazca-Desventuradas'
  sizing = 'small'

}
if(index==2){
  mpa_name = 'Pacific Remote Islands'
  index = 2
  sizing = 'large'
  territory = ''
  

}
if(index == 3) {
  mpa_name = 'Papahānaumokuākea Marine National Monument'
  territory = 'Hawaii'
  mpa_before = mpa[mpa@data$ORIG_NAME == 'Papahānaumokuākea', ]
  sizing = 'large'
  
}
if(index == 4) {
  mpa_name = 'Phoenix Islands Protected Area'
  territory = 'Phoenix Group'
  sizing = 'large'
  
}
if( index == 5) {
  mpa_name = 'Pitcairn Islands Marine Reserve'
  territory = 'Pitcairn'
  sizing = 'large'
 
}



###first thing to do is get shapefiles

curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ] 
curr_eez = eez[eez@data$Territory1 == territory,] # get a specific EEZ
if (mpa_name ==  'Pacific Remote Islands') {
  curr_eez = eez[eez@data$MRGID_Sov1 == 2204,]
  curr_eez = curr_eez[curr_eez@data$ISO_Ter1 == "UMI",]  
  curr_eez = curr_eez[!(curr_eez@data$Territory1 %in% c("Wake Island", "Jarvis Island", "Johnston Atoll")),]
  
}


#BRING THE POLYGON TOGETHER IF IT IS SEPARATED BY THE 180 LINE
if (territory == "Hawaii") { #wraps it at the 0
  curr_mpa = spTransform(curr_mpa, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  curr_eez = spTransform(curr_eez, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  mpa_before = spTransform(mpa_before, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
} else if (mpa_name == "Pacific Remote Islands") {
  print('transforming PRI shpefiles')
  curr_mpa = spTransform(curr_mpa, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
  curr_eez = spTransform(curr_eez, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
}

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





####load data locally####
binned_effort = Rdata[[index]][[1]]
binned_effort2 = Rdata[[index]][[2]]
binned_effort3 = Rdata[[index]][[3]]



###180 line transformations
if(territory=="Hawaii" || mpa_name=="Pacific Remote Islands") {
  yo = binned_effort
  yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
  yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
  names(yo2)[2]<-"lon_bin_center"
  binned_effort = yo2
}
if(territory=="Hawaii" || mpa_name=="Pacific Remote Islands") {
  yo = binned_effort2
  yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
  yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
  names(yo2)[2]<-"lon_bin_center"
  binned_effort2 = yo2
}
if(territory=="Hawaii" || mpa_name=="Pacific Remote Islands") {
  yo = binned_effort3
  yo$lon2 = with(yo,ifelse(lon_bin_center<0, 180+lon_bin_center,lon_bin_center-180))
  yo2 = yo[,c("lat_bin_center","lon2","fishing_hours")]
  names(yo2)[2]<-"lon_bin_center"
  binned_effort3 = yo2
}

#change shapefiles
if(territory == "Hawaii" ) {
  print('transforming hawaii shapefiles')
  b2 = curr_mpa$geometry + c(-180, 0)         
  b3 = mpa_before$geometry + c(-180.4, -0.26)
  curr_mpa = b2
  curr_eez = b3
  
} else if (mpa_name == 'Pacific Remote Islands') {
  print('transforming pacific remote shapefiles')
  b2 = curr_mpa$geometry + c(-180, 0) 
  b3 = curr_eez$geometry + c(-180, 0) 
  curr_mpa = b2
  curr_eez = b3
  
}


#View(binned_effort)

effortPlot = list()
setwd('/Users/tiffanyong/Documents/GitHub/GFW5/Figures/test2')

#############################################
########         PLOTTING    ################
#############################################

###############PLOT 1----------------

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
                              limits=c(NA, 3000),
                              breaks = scales::log_breaks(n = 6, base = 4))+
  #hrbrthemes::theme_ipsum()+
  theme_minimal()+
  labs(#title = graph_title,
    #subtitle = subtitles[[k]],
    y = "",
    x = "")+
  theme(axis.text.x =element_blank(),
        
        axis.text.y = element_blank(),
        legend.position="none"
  ) 
  
        #plot.title = element_text(size = 18, hjust = 0),
        #plot.subtitle = element_text(size = 16, hjust = 0),
        #plot.margin = margin(10,0,0,0),
        #legend.margin = margin(5,5,20,0))
  
#for papa only
  if (territory == "Hawaii") {
    scale_x_continuous(  breaks =seq(-2,20,2) )
  } 
if (territory == "") {
  scale_x_continuous( breaks =seq(-20,24,2) )
} 
if ( mpa_name  == 'Nazca-Desventuradas') {
  scale_x_continuous( breaks = seq(-84, 70, 4)   )
  
}
  




#quartz()
effortPlot[[1]]

title = paste(mpa_name, '1', '.png', sep = " ", collapse = NULL)
if(sizing == 'large'){
  ggsave(title, width = 5, height = 4)
w} else {
  ggsave(title, width = 10, height = 10)
}














#PLOT 2----------------


#change to yo2 instad of binned_effort[[k]] for Hawaii or PRI-----
effortPlot[[2]] = binned_effort2 %>% 
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
                              limits=c(NA, 1024),
                              breaks = scales::log_breaks(n = 6, base = 4))+
  #hrbrthemes::theme_ipsum()+
  theme_minimal()+
  labs(
       #subtitle = subtitles[[k]],
       y = "",
       x = "")+
  theme(axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        legend.position="none"
        #plot.title = element_text(size = 18, hjust = 0),
        #plot.subtitle = element_text(size = 16, hjust = 0),
        #plot.margin = margin(10,0,0,0),
        #legend.margin = margin(5,5,20,0))
  ) +
  if (territory == "Hawaii") {
    scale_x_continuous(  breaks =seq(-2,20,2) )
  }

effortPlot[[2]]
title = paste(mpa_name, '2', '.png', sep = " ", collapse = NULL)
if(sizing == 'large'){
  ggsave(title, width = 14, height = 10)
} else {
  ggsave(title, width = 10, height = 10)
}




#for pipa only -----------    




#########################
# Effort Difference Plot#--------
#########################
#scale---------------------------
scale = viridis(100)[10:100]

#transform
yo = binned_effort3

#make extremes the same color for all mpas
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
#hrbrthemes::theme_ipsum()+
theme_minimal()+
  labs(#title = "After-Before Difference",
    #subtitle = paste(year_before_date, "to", year_after_date, sep= ' '),
    y = "",
    x = "")+
  theme(axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position="none",
        #plot.title = element_text(size = 18, hjust = 0),
        #plot.subtitle = element_text(size = 16, hjust = 0),
        #plot.margin = margin(10,0,0,0),
        #legend.margin = margin(5,5,20,0),
        panel.background=element_rect(fill = "white",
                                      colour = NA,
                                      size = 0.5, linetype = "solid")
  ) +
  if (territory == "Hawaii") {
    scale_x_continuous(  breaks =seq(-2,20,2) )
  }
#scale_x_discrete(breaks = c(-178,-173, -168))
#scale_x_discrete(breaks = c(-134,-130, -125,-120))

#quartz() 
effortPlot[[3]]
title = paste(mpa_name, '3','.png', sep = " ", collapse = NULL)
if(sizing == 'large'){
  ggsave(title, width = 14, height = 10)
} else {
  ggsave(title, width = 10, height = 10)
}




