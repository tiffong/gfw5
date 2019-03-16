# getEEZOverlap.R
# 8/8/17 
# This calculated the proportion of a species' overlap with AIS data that occurs within various EEZs and the high seas
# The goal is to produce a barplot plot showing by species (with bar fill indicating nation or gear type) the % of total overlap that occurs in high seas
setwd("~/AIS/s")

# TO-DO LIST 10/13/17
# - CALCULATE TOTAL AREA OF NE PACIFIC THAT IS FISHED FOR COMPARISON WITH PERCENT OF CORE AREA THAT OVERLAPS

# HMcells is a vector of total core area cells by species, comes from getAnnualOverlapsum1.R

library(tidyr) # for gather()
library(ggplot2)
library(rgdal)  # for readOGR()
library(devtools)
#install("AIS")
#source("buildAIS.R")  #call this line whenever new changes are made to scripts that build the AIS package
require(AIS)
library(dplyr)
library(raster)
library(fields) # for image.plot
library(colorRamps) # for image.plot color scale
library(ggthemes) # for color schemes found here https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html
library(gridExtra) # for grid.arrange(), used to combine 2 ggplot barplots into 1 figure

bbox = c(10,70,-180,-100)
ylim <- c(bbox[1], bbox[2]) #define our map's ylim
xlim <- c(bbox[3], bbox[4])

plotNames = c("Bluefin tuna","Yellowfin tuna","Albacore tuna","White shark","Mako shark","Salmon shark","Blue shark") # Labels that will be used in plotting

load(file = "../data/coreoverlapHMprobabilitiessum1expNewMCP10N.RData", envir = parent.frame(), verbose = FALSE) # read in data from getAnnualOverlapsum1.R
Months = c("01","02","03","04","05","06","07","08","09","10","11","12")
species = c("Pacific Bluefin Tuna","Yellowfin Tuna","Albacore Tuna","White Shark","Shortfin Mako Shark","Salmon Shark","Blue Shark") # ,"Black-footed Albatross","Laysan Albatross","Sooty Shearwater

#### read in EEZ data that comes from http://www.marineregions.org/downloads.php

eez <- readOGR(dsn = "/Users/timwhite/Dropbox/AIS/data/World_EEZ_v9_20161021_LR/", layer = "eez_lr") # lower res file. too big to fit in Github so its in Dropbox

eez@data = eez@data[which(is.na(eez@data$Sovereign1) == F),] # remove a single NA value for disputed Chinese land
# View(eez@data) # this is the summary table for the EEZ polgons

US = eez[eez@data$Sovereign1 == "United States", ] # gets just US EEZ
Can = eez[eez@data$Sovereign1 == "Canada", ] # CAN EEZ
Mex = eez[eez@data$Sovereign1 == "Mexico", ] # MEX EEZ

###### bring in allmonths, a 12 month dataframe that comes from getAnnualOverlapsum1.R, and prepare it for EEZ calculation
#

plotList = list()

for(i in 1:length(plotNames)){  # loop over 7 species
  
eezdf = data.frame(matrix(NA, nrow = 12, ncol = 9))  # fill this with eez values and %s later
colnames(eezdf) = c("weightedMex","weightedUS","weightedCan","weightedHS","perMex","perUS","perCan","perHS","month")
  
allmonths =  overlapHM[[i]]  # brings in overlap values from getAnnualOverlapsum1.R - filter for just one species. this is used for monthly overlap plots by weighted values

# noNAs = overlapHM[[i]] # this will be used for EEZ overlap plots with cells&areas instead of weighted model values
# noNAs = noNull[complete.cases(noNAs), ] # remove all rows with NAs so I can accurately count how many pixels have overlap
# noNAs$annualinteract[noNAs$annualinteract > 0] = 1  # replaced weighted values with 1's to denote core area pixels
# length(noNAs$annualinteract)

# celldf = data.frame(matrix(NA, nrow = 12, ncol = 4)) # this will be the number of overlap cells in an eez each month
# colnames(eezdf) = c("cellsMex","cellsUS","cellsCan","cellsHS","month")

for (j in 1:length(Months)){

EEZmon = filter(allmonths, month == j)  # allmonths is a 12 month dataframe coming from getAnnualOverlapsum1.R
xyz = EEZmon[,c(2,1,8)]  # get just lon lat and interaction value
head(xyz)
xyz$annualinteract[xyz$annualinteract > 0] = 1  # replaced weighted values with 1's to denote core area COMMENT LINE OUT FOR WEIGHTS


quant = as.vector(quantile(xyz$annualinteract,probs = c(0.0, 1.0)))  #  setting this up for image.plot() since we need to define bounds

overlapRast = rasterFromXYZ(xyz, res = c(1,1)) # number of items to replace is not a multiple of replacement length
# res = c(1,1) is included because otherwise, I get an error for month 12 yellowfin tuna because there's only 1 cell for this month. not sure why this fix worked..

projection(overlapRast) <- CRS("+proj=longlat +datum=WGS84")
#overlapRast # checking raster

# quartz()   # plotting monthly overlap for 1 species and EEZ lines to confirm that it's working
# image.plot(overlapRast, legend.lab = "Relative overlap",las = 1, ylab = "latitude", xlab = "longitude", main = paste(species[i]," predicted overlap with fisheries in 2015", sep = ""), col=matlab.like2(255), xlim = xlim, ylim = ylim,zlim = c(quant[1],quant[2])) # works. now I think we can mask with eez.
# 
# plot(US,  xlim = xlim, ylim = ylim, col = NA, border = 'blue3', lwd = 3, add = T) # plot EEZ lines onto map
# plot(Can, xlim = xlim, ylim = ylim, col = NA, border = 'Red', lwd = 3, add = T)
# plot(Mex, xlim = xlim, ylim = ylim, col = NA, border = 'darkgreen', lwd = 3, add = T)
# map("world", xlim = xlim, ylim = ylim, fill = T, col='grey90',add = T)

# Repeat above for cell count - actually don't think I need this code...
# cellmon = filter(noNAs, month == j)
# xyzcell = cellmon[,c(2,1,8)]  # get just lon lat and interaction value
# head(xyzcell)
# overlapRastCell = rasterFromXYZ(xyzcell, res = c(1,1)) # number of items to replace is not a multiple of replacement length
# # res = c(1,1) is included because otherwise, I get an error for month 12 yellowfin tuna because there's only 1 cell for this month. not sure why this fix worked..
# projection(overlapRastCell) <- CRS("+proj=longlat +datum=WGS84")

### return Overlap within EEZ of Canada, US, Mexico, and no EEZ (high seas)

# calculate total value of overlap: 
mexTrim <- raster::mask(overlapRast, Mex)       # MCP mask comes from getMCPs.R; MCPs of the SSM data from the ATN site and TOPP dataset
summary(mexTrim)
mexDF = as.data.frame(mexTrim)     # covert to data.frame for summing
#mexDF$annualinteract[mexDF$annualinteract > 0] = 1  # replaced weighted values with 1's to denote core area

eezdf[j,"weightedMex"] = sum(mexDF$annualinteract, na.rm = T)  # 0.01819548              sum of values in 1 month
eezdf[j,"perMex"] = (sum(mexDF$annualinteract, na.rm = T)/sum(xyz$annualinteract))*100  # percent of monthly overlap values in an EEZ

# mexTrimCell <- raster::mask(overlapRastCell, Mex)     
# summary(mexTrimCell)
# mexDFCell = as.data.frame(mexTrimCell)     # covert to data.frame for summing



usTrim <- raster::mask(overlapRast, US)       # MCP mask comes from getMCPs.R; MCPs of the SSM data from the ATN site and TOPP dataset
summary(usTrim)
usDF = as.data.frame(usTrim)     # covert to data.frame for summing
#usDF$annualinteract[mexDF$annualinteract > 0] = 1  # replaced weighted values with 1's to denote core area
eezdf[j,"weightedUS"] = sum(usDF$annualinteract, na.rm = T)  # 0.01819548              sum of values in 1 month
eezdf[j,"perUS"] = (sum(usDF$annualinteract, na.rm = T)/sum(xyz$annualinteract))*100  # percent of monthly overlap values in an EEZ

canTrim <- raster::mask(overlapRast, Can)       # MCP mask comes from getMCPs.R; MCPs of the SSM data from the ATN site and TOPP dataset
summary(canTrim)
canDF = as.data.frame(canTrim)     # covert to data.frame for summing
eezdf[j,"weightedCan"] = sum(canDF$annualinteract, na.rm = T)  # 0.01819548              sum of values in 1 month
eezdf[j,"perCan"] = (sum(canDF$annualinteract, na.rm = T)/sum(xyz$annualinteract))*100  # percent of monthly overlap values in an EEZ

eezdf[j,"weightedHS"] = sum(xyz$annualinteract) - sum(eezdf[j,1:3])   # high seas values = monthly sum - values within EEZs
eezdf[j,"perHS"] = 100 - sum(eezdf[j,5:7])                            # high seas %s = 100% - %s within EEZs

eezdf[j,"month"] = j                                                  # adding a monthly label
 
}

### checking if all weighted values in our 4 categories add up to 1, and all percentages add up to 100 (or 100*12 for 12 months)
# sum(eezdf[,1:4]) # = 1. Good. 
# sum(eezdf[,5:8]) # = 1200. Good - 100% for each of 12 months

#colnames(eezdf) = c("wMex","wUS","wCan","wHS","Mex","US","Can","HS","month")
colnames(eezdf) = c("Mexico","U.S.","Canada","High seas","Mex","US","Can","HS","month")


head(eezdf)
perPlot = eezdf %>% gather(EEZ,percent,5:8)   # trying to condense these values into fewer columns so that all EEZ values are in a single column. currently there is one column for each EEZ
head(perPlot)
perPlot = perPlot[,5:7]

weightPlot = eezdf %>% gather(EEZ,weight,1:4)
head(weightPlot)
weightPlot = weightPlot[,5:7]


head(weightPlot)  # this is what we need for weights plots - 3 columns: month, EEZ, percent

# agg = aggregate(x = perPlot$percent, list(perPlot$EEZ), FUN = mean)  # using MEAN to calculate mean monthly overlap percentage, and SUM below to sum weighted values
# colnames(agg) = c("EEZ","Percent")
# plotList[[i]] = agg
# plotList[[i]]$Species = i

aggW = aggregate(x = weightPlot$weight, list(weightPlot$EEZ), FUN = sum)  # CHECK THAT "SUM" IS DOING WHAT IT SHOULD
# reorder these so that it's not alphabetical! I want order to be High Seas, Mexico, US, Canada
colnames(aggW) = c("EEZ","Overlap")

target = c("Canada", "U.S.","Mexico","High seas")  # re-orders nations for plotting - 10.24.17 this acutally doesn't seem to affect plotting
aggW = aggW[match(target,aggW$EEZ),]               # re-orders nations for plotting


plotList[[i]] = aggW
plotList[[i]]$Species = plotNames[i]   # plotNames is defined at top of script as a vector of names 
plotList[[i]]$Percent = (plotList[[i]]$Overlap/sum(plotList[[i]]$Overlap))*100  # COMMENT THIS LINE OUT IF YOU'RE PLOTTING WEIGHTS

plotList[[i]]$cellOverlap = (plotList[[i]]$Overlap/HMcells[i])*100

}

plotDF = do.call(rbind,plotList)         # combine results from all species into a single dataframe for plotting 
colnames(plotDF) = c("EEZ","Overlap","Species","Percent","cellOverlap")
plotDF

save(plotDF, file = "../data/EEZoverlap.RData") #




###### PLOTTING STACKED BARPLOT FOR ANNUAL OVERLAP DATA ######
load(file = "../data/EEZoverlap.RData", envir = parent.frame(), verbose = FALSE) # read in data from getEEZoverlap.R)
charts.data = plotDF

charts.data$EEZ = factor(charts.data$EEZ, levels = c("Canada","U.S.","Mexico","High seas")) # reorder levels for stacked barplot - order matches geography. https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2

### THIS IS THE PLOT WE WANT
#pdf("../maps/overlapInEEZbyCoreArea10Nptol.pdf",width = 8.5, height = 4.5)  # bar length = % core area that overlaps in this version
#png("../maps/overlapInEEZbyCoreArea10Nptol.png",width = 8.5, height = 4.5, units = "in", res = 300)  # bar length = % core area that overlaps in this version

speciesSums = aggregate(charts.data$cellOverlap, by=list(Category=charts.data$Species), FUN=sum) # get the total percent overlap by species to order from highest to lowest

tab = table(speciesSums$x) # NOT WORKING, TRYING TO ORDER
otab = tab[order(-tab)]

# manually setting position of bars from highest to lowest REMOVE PREVIOUS PLOTNAMES ABOVE
plotNames = c("Salmon shark","Yellowfin tuna","Bluefin tuna","Blue shark","White shark","Albacore tuna","Mako shark") # Labels that will be used in plotting

eezPlot = ggplot() + geom_bar(data = charts.data, aes(y = cellOverlap, x = Species, fill = EEZ),   # bar length = % core area that overlaps in this version
                    stat="identity") +   
  theme_classic()  +                                 # remove grey background boxes
  scale_x_discrete(limits = plotNames)  +       # reorder bars to tunas and sharks come in correct order, tunas on top
  #scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#999999")) + # colors I picked
  scale_fill_manual(values=c("#CC6677", "#4477AA", "#117733", "#999999")) +  #select colors from ptol scale, https://personal.sron.nl/~pault/colourschemes.pdf
  #scale_fill_ptol("EEZ") +    
  coord_flip() +                                     # flip from vertical bars to horizontal bars
  labs(x="Species", y="Percent of core area fished", title = "Location of fisheries overlap") +
  theme(plot.title = element_text(hjust = 0.5),title = element_text(size=14, face = 'bold'), axis.text=element_text(size=12, color = "black"),legend.text=element_text(size=11),plot.margin = margin(c(5.5, 10, 5.5, 5.5)))      # center the title

#dev.off()


###### Now plotting high seas overlap, trying to plot both on same figure ######

load(file = "../data/EEZoverlap.RData", envir = parent.frame(), verbose = FALSE) # read in data from getEEZoverlap.R)
eez.data = filter(plotDF, EEZ == "High seas") # we need this for scaling the bars to the % of overlap that occurs in high seas

load(file = "../data/HSoverlapByFlag.RData", envir = parent.frame(), verbose = FALSE) # read in data from code above, getOverlapByFlag.R)
charts.data = plotDF

charts.data = merge(x = charts.data, y = eez.data, by = "Species", all.x = TRUE) # add a column that shows the % of overlap that is in high seas for each species
charts.data$scaledPercent = charts.data$Percent.x * (charts.data$Percent.y/100)
# I need to multiply charts.data$Percent by eez.data$highseas

#

charts.data$Country = gsub("United States","U.S.",charts.data$Country)
charts.data.sub = charts.data



#charts.data$EEZ = factor(charts.data$EEZ, levels = c("Canada","U.S.","Mexico","High seas")) # reorder levels for stacked barplot - order matches geography. https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2


##### Stacked barplot, bar length = % of overlap that is in high seas for each species
plotNames = c("Blue shark","Albacore tuna","White shark","Yellowfin tuna","Bluefin tuna","Mako shark","Salmon shark") # Re-order bars by % of overlap in high seas

#png("../maps/HSoverlapByFlagScaledbarsOrdered.png",width = 8.5, height = 4.5, units = "in", res = 300)  # bar length = % core area that overlaps in this version

HSplot = ggplot() + geom_bar(data = charts.data.sub, aes(y = scaledPercent, x = Species, fill = Country), stat="identity") +   
  theme_classic()  +                                 # remove grey background boxes
  scale_x_discrete(limits = plotNames)  +       # reorder bars to tunas and sharks come in correct order, tunas on top
  scale_fill_ptol("Country") +                       # pick a color palette from ggthemes
  coord_flip() +                                     # flip from vertical bars to horizontal bars
  labs(x="Species", y="Percent of a species' total overlap", title = "Fisheries overlap on the high seas") +
  theme(plot.title = element_text(hjust = 0.5),title = element_text(size=14, face = 'bold'), axis.text=element_text(size=12, color = "black"),legend.text=element_text(size=11))      # center the title
HSplot
C <- plot_grid(HSplot, labels=c('C'), label_fontface = "bold", label_size = 15, hjust = -14, vjust = 2) # adds label to ggplot, hjust and vjust control position
C


### Combining both barplots into 1 figure

png("../maps/overlapInEEZbyCoreAreaANDhighseasSCIADV.png",width = 8.5, height = 8, units = "in", res = 300)  # both EEZ and high seas barplots 
grid.arrange(eezPlot, HSplot)

dev.off()

# end Figure
























































#pdf("../maps/overlapInEEZcleanedFullMCP10N.pdf",width = 8.5, height = 4.5)  # all bars = 100% in this version, weighted values plotted
ggplot() + geom_bar(data = charts.data, aes(y = Overlap, x = Species, fill = EEZ), 
                          stat="identity") +   
   theme_classic()  +                                 # remove grey background boxes
   scale_x_discrete(limits = rev(plotNames))  +       # reorder bars to tunas and sharks come in correct order, tunas on top
   scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#999999")) + 
   coord_flip() +                                     # flip from vertical bars to horizontal bars
   labs(x="Species", y="Relative overlap", title = "Location of fisheries overlap") +
   theme(plot.title = element_text(hjust = 0.5),title = element_text(size=14, face = 'bold'), axis.text=element_text(size=12, color = "black"),legend.text=element_text(size=11))      # center the title

dev.off()



pdf("../maps/overlapInEEZbyCoreAreaPercents.pdf",width = 8.5, height = 4.5)  # all bars = 100% in this version, values plotted is % overlap pixels (as opposed to weighted model values)
ggplot() + geom_bar(data = charts.data, aes(y = Percent, x = Species, fill = EEZ),   # bar length = % core area that overlaps in this version
                    stat="identity") +   
  theme_classic()  +                                 # remove grey background boxes
  scale_x_discrete(limits = rev(plotNames))  +       # reorder bars to tunas and sharks come in correct order, tunas on top
  scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#999999")) + 
  coord_flip() +                                     # flip from vertical bars to horizontal bars
  labs(x="Species", y="Relative overlap", title = "Location of fisheries overlap") +
  theme(plot.title = element_text(hjust = 0.5),title = element_text(size=14, face = 'bold'), axis.text=element_text(size=12, color = "black"),legend.text=element_text(size=11))      # center the title

dev.off()












### BELOW IS CODE THAT DIVIDES WEIGHTED MODEL VALUES INTO EEZS - ABOVE IS CODE FOR NUMBER OF PIXELS 

# Plan: determine total number of overlap pixels in a given month, allocate them to an EEZ zone, sum all values.
# Do this on a monthly basis instead of an annual basis because otherwise a cell with 12 months of overlap would be weighted the same as a cell that just had 1 month of overlap. 

# How to incorporate the amount of core area that is overlapped? Need to get values from 

# 10.29.31 RESUME HERE I NEED TO CHANGE AWAY FROM BINARY - THAT SPECIES COLUMN INDICATES HOW MANY SPECIES ARE THERE, NOT WHICH SPECIES

for(i in 1:length(plotNames)){  # loop over 7 species
  
  eezdf = data.frame(matrix(NA, nrow = length(plotNames), ncol = 8))  # fill this with eez values and %s later
  colnames(eezdf) = c("weightedMex","weightedUS","weightedCan","weightedHS","perMex","perUS","perCan","perHS")
  
  xyz = filter(binary, species == i)  # allmonths is a 12 month dataframe coming from getAnnualOverlapsum1.R
  quant = as.vector(quantile(xyz$species,probs = c(0.0, 1.0)))  #  setting this up for image.plot() since we need to define bounds
  overlapRast = rasterFromXYZ(xyz, res = c(1,1)) # number of items to replace is not a multiple of replacement length
  # res = c(1,1) is included because otherwise, I get an error for month 12 yellowfin tuna because there's only 1 cell for this month. not sure why this fix worked..
  
  projection(overlapRast) <- CRS("+proj=longlat +datum=WGS84")
  
  
  # calculate total value of overlap: 
  mexTrim <- raster::mask(overlapRast, Mex)       # MCP mask comes from getMCPs.R; MCPs of the SSM data from the ATN site and TOPP dataset
  summary(mexTrim)
  mexDF = as.data.frame(mexTrim)     # covert to data.frame for summing
  eezdf[i,"weightedMex"] = sum(mexDF$annualinteract, na.rm = T)  # 0.01819548              sum of values in 1 month
  eezdf[i,"perMex"] = (sum(mexDF$annualinteract, na.rm = T)/sum(xyz$annualinteract))*100  # percent of monthly overlap values in an EEZ
  
  usTrim <- raster::mask(overlapRast, US)       # MCP mask comes from getMCPs.R; MCPs of the SSM data from the ATN site and TOPP dataset
  summary(usTrim)
  usDF = as.data.frame(usTrim)     # covert to data.frame for summing
  eezdf[j,"weightedUS"] = sum(usDF$annualinteract, na.rm = T)  # 0.01819548              sum of values in 1 month
  eezdf[j,"perUS"] = (sum(usDF$annualinteract, na.rm = T)/sum(xyz$annualinteract))*100  # percent of monthly overlap values in an EEZ
  
  canTrim <- raster::mask(overlapRast, Can)       # MCP mask comes from getMCPs.R; MCPs of the SSM data from the ATN site and TOPP dataset
  summary(canTrim)
  canDF = as.data.frame(canTrim)     # covert to data.frame for summing
  eezdf[j,"weightedCan"] = sum(canDF$annualinteract, na.rm = T)  # 0.01819548              sum of values in 1 month
  eezdf[j,"perCan"] = (sum(canDF$annualinteract, na.rm = T)/sum(xyz$annualinteract))*100  # percent of monthly overlap values in an EEZ
  
  eezdf[j,"weightedHS"] = sum(xyz$annualinteract) - sum(eezdf[j,1:3])   # high seas values = monthly sum - values within EEZs
  eezdf[j,"perHS"] = 100 - sum(eezdf[j,5:7])                            # high seas %s = 100% - %s within EEZs
  
}
asd








### Trying to calculate total area of overlap by species, so we can compare this percentage across species
# FLAGGED FOR LATER - THIS IS CURRENTLY NOT WORKING, BUT FOR NOW, WE'LL GO ON WITH JUST A SUM OF PIXELS
# https://gis.stackexchange.com/questions/177622/r-calculate-raster-cell-size-in-map-units LOOK HERE FOR MORE

overlapPoly = rasterToPolygons(overlapRast)
areaPolygon(overlapPoly)
sum(area(overlapPoly))  # sum area of each cell? 
area.polygon(overlapPoly)

latTest = yFromRow(overlapRast,1:nrow(overlapRast))
a = area(overlapRast)
a
area = a[,1]
area
yres(overlapRast)
xres(overlapRast)



