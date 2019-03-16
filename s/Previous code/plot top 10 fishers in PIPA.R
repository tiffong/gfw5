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

#protecc with life
#################
##rm(list=ls())##
#################

#getting the names of the files in the folder
files = list.files(path = "../data/individual_effort_scatterplot/Phoenix Islands Protected Area", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() #list of 

for(i in 1:len) {
  file = files[[i]]
  file_name = paste("../data/individual_effort_scatterplot/Phoenix Islands Protected Area/", file,  sep = "")
  load(file_name)
  Rdata[[i]] =  thisdf3
  Rdata[[i+5]] =  thisdf4
}

###PLOTTING####

title = paste('../Figures/aaaaIndividual_Cumulative/', mpa_name, '.png', sep = '')
png(title, units="in", width=13, height=7, res=300)
thick = 0.4

quartz()
ggplot(Rdata[[1]], aes(x=c(-12:11), y=sum, group=1)) + 
  geom_line(aes(color="1"), size = thick) +
  geom_line(data = Rdata[[6]], aes(color="1"), size = thick-.3) +
  
  geom_line(data=Rdata[[2]], (aes(color="2")), size = thick) +
  geom_line(data=Rdata[[7]], (aes(color="2")), size = thick-.3) +
  
  geom_line(data=Rdata[[3]], (aes(color="3")), size = thick) +
  geom_line(data=Rdata[[8]], (aes(color="3")), size = thick-.3) +
  
  geom_line(data=Rdata[[4]], (aes(color="4")), size = thick) +
  geom_line(data=Rdata[[9]], (aes(color="4")), size = thick-.3) +
  
  geom_line(data=Rdata[[5]], (aes(color="5")), size = thick) +
  geom_line(data=Rdata[[10]], (aes(color="5")), size = thick-.3) +
  
  geom_line(data=Rdata[[6]], (aes(color="6")), size = thick) +
  geom_line(data=Rdata[[11]], (aes(color="6")), size = thick-.3) +
  
  geom_line(data=Rdata[[7]], (aes(color="7")), size = thick) +
  geom_line(data=Rdata[[12]], (aes(color="7")), size = thick-.3) +
  
  geom_line(data=Rdata[[8]], (aes(color="8")), size = thick) +
  geom_line(data=Rdata[[13]], (aes(color="8")), size = thick-.3) +
  
  geom_line(data=Rdata[[9]], (aes(color="9")), size = thick) +
  geom_line(data=Rdata[[14]], (aes(color="9")), size = thick-.3) +
  
  geom_line(data=Rdata[[10]], (aes(color="10")), size = thick) +
  geom_line(data=Rdata[[15]], (aes(color="10")), size = thick-.3) +
  
  labs(color="Rank") +
  labs(x="Month", y="Monthly Fishing Effort", 
       title = paste("Monthly Fishing Effort ", mpa_name, sep = "") , 
       subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(shjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)

dev.off()


########################################
#testing not hard coding the plots######
########################################

mydata = list()

for(i in 1:5) {
  mydata[[i]]=Rdata[[i]]$sum
}

mydf = data.frame(mydata)

###another way

combined.df = Rdata[[1]]$month
combined.df = cbind(Rdata[[1]]$month, Rdata[[1]]$sum, Rdata[[2]]$sum)

for (i in 1:5) {
  combined.df$i = Rdata[[i]]$sum
}

##another try with looping cbind to get all col
#this is promising
#creating a matrix first 
ds = c()
ds = cbind(ds, Rdata[[1]]$month)
for(i in 1:10) {
  ds = cbind(ds, Rdata[[i]]$sum)
}


ds = t(data.frame(ds))


d = read.delim(textConnection(ds), sep = "")

d = melt(d, id.vars = "X1")


##trywith dplry
# quartz()
# x = cbind (Rdata[[1]]$month,Rdata[[2]]$month,Rdata[[3]]$month )
# y = cbind (Rdata[[1]]$sum,Rdata[[2]]$sum,Rdata[[3]]$sum )
# matplot(x,y, type = "p")

##THIS IS PROMISING
x=c()
y=c()
for(i in 1:5) {
  x = cbind(x, Rdata[[i]]$month)
  y = cbind(y, Rdata[[i]]$sum)
}

quartz()
plot(x,y)


##test plot code below
quartz()
ggplot(Rdata[[1]], aes(x=c(-12:11), y=sum, group=1)) + 
  geom_line(aes(color="1"), size = thick) +
  geom_line(data = Rdata[[6]], aes(color="1"), size = thick-.3) +
  
  geom_line(data=Rdata[[2]], (aes(color="2")), size = thick) +
  geom_line(data=Rdata[[7]], (aes(color="2")), size = thick-.3) +
  

  labs(color="Rank") +
  labs(x="Month", y="Monthly Fishing Effort", 
       title = paste("Monthly Fishing Effort ", mpa_name, sep = "") , 
       subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(shjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)


