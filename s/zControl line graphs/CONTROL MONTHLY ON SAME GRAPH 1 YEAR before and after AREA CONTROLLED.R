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
library(cowplot) #trying new plot things
library(ggpubr)

#plots monthly fishing effort a year before and after MPA creation and puts it against control EEZs

#getting the names of the files in the folder-------


#plot------
plots = list()
thick = 0.5
yax_size = 12
xax = 12

################PIPA PLOTTING

mpa_name= 'Phoenix Islands Protected Area'
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
###PIPA####-----
# Gilbert 1053239.7, Line 1640932.6, Phoenix 745700.8
Area = c(1053239.7,1053239.7,1053239.7, 1640932.6, 745700.8,745700.8,745700.8,745700.8,745700.8) #PIPA
Rdata_temp = Rdata


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

  labs(color="EEZ Name") +
  labs(x="Month" ,
       y=expression("Fishing Hours/1000" ~km^2),
       title = "Phoenix Islands Protected Area"
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = xax),
        axis.text.y = element_text(size = yax_size),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()



################PITCAIRN PLOTTING

mpa_name = 'Pitcairn Islands Marine Reserve'



################PITCAIRN PLOTTING

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
#PITCAIRN----
Rdata_temp = Rdata
Area = c(1969507,1969507,1969507, 4766884, 842344.8) #Pitcairn #Cook 1969507, french polynesian 4766884, pitcairn 842344.8

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
  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y=expression("Fishing Hours/1000" ~km^2),
       title = "Pitcairn Islands"
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = xax),
        axis.text.y = element_text(size = yax_size),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()



# title = paste('../Figures/aaaacumulative/', 'Cumulative', '.png', sep = '')
# png(title, units="in", width=13, height=7, res=300)
# 
# 
#   ggplot(Rdata[[1]], aes(c(-12:11), Rdata[[1]]$fishing_effort, group=1)) + 
#   geom_point(size=0.1,stroke = 0, shape = 16) + #makes the points disappear
#   geom_line(size=0.2) +
#     
#   labs(x="Month", y="Monthly Fishing Effort", 
#        title = "Monthly Fishing Effort", 
#        subtitle = "Before and After MPA Creation"
#        ) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_minimal() +
#   # scale_x_date(date_breaks = "1 month", 
#   #              date_labels = "%b %y") +
#   geom_vline(aes(xintercept = 0), color="dodgerblue", size=0.3)
# 
# dev.off()

#plots[[3]] = 
  


################PAPA PLOTTING

mpa_name= 'Papahānaumokuākea Marine National Monument'
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

  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y=expression("Fishing Hours/1000" ~km^2) ,
       title = "Papahānaumokuākea"
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = xax),
        axis.text.y = element_text(size = yax_size),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()


################PRI PLOTTING
mpa_name = 'Pacific Remote Islands'
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
#PRI-----
# Gilbert 1053239.7, Line 1640932.6, PRI 1277784 (some area from eez data)

Rdata_temp = Rdata
Area = c(1053239.7, 1640932.6, 1640932.6, 1277784)
for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}


plots[[4]] =
#quartz()
ggplot(Rdata_temp[[1]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  geom_line(aes(color="Kiribati (Gilbert Islands)"), size = thick) +
  geom_line(data=Rdata_temp[[2]], (aes(color="Kiribati (Line Islands)")), size = thick) +
  #geom_line(data=Rdata_temp[[2]], (aes(color="Kiribati (Line Islands)")), size = thick) +
  geom_line(data=Rdata_temp[[4]], (aes(color="Pacific Remote Islands")), size = thick+0.2, color = "black") +
 
  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y=expression("Fishing Hours/1000" ~km^2),
       title = 'Pacific Remote Islands'
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = xax),
        axis.text.y = element_text(size = yax_size),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))

#dev.off()


################NAZCA PLOTTING
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
#Nazca-----
#Chilean 2465054.7, Nazca (gis_rep_area) 300585.1, Peruvian 855322.3
  
Rdata_temp = Rdata
#Area = c(2465054.7,300585.1,855322.3)

Area = c(527930.9,300585.1,406981.5)

Rdata_temp[[3]]$fishing_effort = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) # update to the little Pacman slice
Rdata_temp[[1]]$fishing_effort = c(102.5,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) # update to the Fernandez islands


for(i in 1:len) {
  Rdata_temp[[i]]$fishing_effort =  Rdata_temp[[i]]$fishing_effort / Area[[i]] * 1000
}

plots[[5]] =
  #quartz()
  ggplot(Rdata_temp[[1]], aes(x=c(-12:11), y=fishing_effort, group=1)) + 
  geom_line(aes(color="Chile (Juan Fernandez Islands)"), size = thick) +
  geom_line(data=Rdata_temp[[3]], (aes(color="Chile (San Felix & San Ambrosio)")), size = thick) +
  geom_line(data=Rdata_temp[[2]], (aes(color="Nazca")), size = thick+0.2, color = "black") +
  
  labs(color="EEZ Name") +
  labs(x="Month", 
       y=expression("Fishing Hours/1000" ~km^2),
       title = 'Nazca-Desventuradas'
       #title = "Monthly Fishing Effort", 
       #subtitle = "Before and After MPA Creation"
  ) +
  ylim(0,2) +
  theme(plot.title = element_text(hjust = 0.5)) + #this does nothing
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)+
  theme(axis.text.x = element_text(size = xax),
        axis.text.y = element_text(size = yax_size),
        plot.title = element_text(size = 16, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.margin = margin(10,10, 10,10),
        legend.margin = margin(5,5,20,0))


#dev.off()


#gridarrange----
#quartz()

png(filename="../Figures/Final_Figures/Figure3WithTitle2_revision.png",
    units="in", width=13, height=7, res=300)

figure = ggarrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],
          labels = c('A', 'B', 'C', 'D', 'E'),
             nrow=3, ncol=2)

annotate_figure(figure,
                top = text_grob("Fishing Effort in Large MPAs and Nearby Nations", color = "black", face = "bold", size = 16)
)


dev.off()


