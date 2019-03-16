###############################################################################
# Pacific centered world map with ggplot.
# Enhanced aspect with graticules and labels.
# The central/prime meridian can be shifted with any positive value towards west.
# Can use any project of known PROJ.4 string
###############################################################################

library(data.table)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maps)
library(maptools)
library(raster)
library(leaflet)

# Download shapefile from www.naturalearthdata.com

# Download countries data
download.file(url = "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", 
              destfile = "gallery/ne_110m_admin_0_countries.zip")
# unzip the shapefile in the directory mentioned with "exdir" argument
unzip(zipfile="gallery/ne_110m_admin_0_countries.zip", exdir = "gallery/ne_110m_admin_0_countries")
# delete the zip file 
file.remove("gallery/ne_110m_admin_0_countries.zip")
# read the shapefile with readOGR from rgdal package
NE_countries <- readOGR(dsn = "gallery/ne_110m_admin_0_countries", layer = "ne_110m_admin_0_countries")
class(NE_countries) # is a SpatialPolygonsDataFrame object

##get shapefiles for MPA - TAKES THE MOST TIME - about 2 mins 16 seconds bruh
mpa <- readOGR(dsn = "/Users/tiffanyong/Desktop/MPA_shapefiles/", layer = "WDPA_June2018_marine-shapefile-polygons")

#Get MPA shapefiles

#PIPA
mpa_name = 'Phoenix Islands Protected Area'
curr_mpa = mpa[mpa@data$ORIG_NAME == mpa_name, ]

#PITCAIRN
pitcairn_name = 'Pitcairn Islands Marine Reserve'
pitcairn_shapefile = mpa[mpa@data$ORIG_NAME == pitcairn_name, ]

#Nazca
nazca_name = 'Nazca-Desventuradas'
nazca_shapefile = mpa[mpa@data$ORIG_NAME == nazca_name, ]

#Hawaii
hawaii_name = "Papahānaumokuākea Marine National Monument"
hawaii_shapefile = mpa[mpa@data$ORIG_NAME == hawaii_name, ]
#hawaii_before_expansion = mpa[mpa@data$ORIG_NAME == 'Papahānaumokuākea', ]
#wrapping hawaii
#hawaii_shapefile = spTransform(hawaii_shapefile, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
#hawaii_before_expansion = spTransform(hawaii_before_expansion, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))

#PRI
pri_name = "Pacific Remote Islands"
pri_shapefile = mpa[mpa@data$ORIG_NAME == pri_name, ]
#wrapping pri
pri_shapefile = spTransform(pri_shapefile, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
pri_shapefile = spTransform(pri_shapefile, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))
pri_shapefile = spTransform(pri_shapefile, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180"))



########VISUALIZE SHAPEFILE#######

leaflet() %>%
  addTiles() %>%
  addPolygons(data = curr_mpa, color ="green") %>%
  addPolygons(data = pitcairn_shapefile, color = "blue") %>%
  addPolygons(data = nazca_shapefile, color = "orange")  %>%
  addPolygons(data = hawaii_shapefile, color = "red")  %>%
  addPolygons(data = hawaii_before_expansion, color = "blue")  %>%
  addPolygons(data = pri_shapefile, color = "purple") 

# =============================================================================
# Split world map by "split line"
# =============================================================================

# inspired from:
# https://stat.ethz.ch/pipermail/r-sig-geo/2015-July/023168.html

# shift central/prime meridian towards west - positive values only
shift <- 180+30

# create "split line" to split country polygons
WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
split.line = SpatialLines(list(Lines(list(Line(cbind(180-shift,c(-90,90)))), ID="line")), 
                          proj4string=WGS84)

# NOTE - in case of TopologyException' errors when intersecting line with country polygons,
# apply the gBuffer solution suggested at:
# http://gis.stackexchange.com/questions/163445/r-solution-for-topologyexception-input-geom-1-is-invalid-self-intersection-er
# NE_countries <- gBuffer(NE_countries, byid=TRUE, width=0)

# intersecting line with country polygons
line.gInt <- gIntersection(split.line, NE_countries)

# create a very thin polygon (buffer) out of the intersecting "split line"
bf <- gBuffer(line.gInt, byid=TRUE, width=0.000001)  

# split country polygons using intersecting thin polygon (buffer)
#World
NE_countries.split <- gDifference(NE_countries, bf, byid=TRUE)
#Pipa
curr_mpa.split <- gDifference(curr_mpa, bf, byid=TRUE)
#Pitcairn
pitcairn_shapefile.split <- gDifference(pitcairn_shapefile, bf, byid=TRUE)
#Nazca
nazca_shapefile.split <- gDifference(nazca_shapefile, bf, byid=TRUE)
#Hawaii
hawaii_shapefile.split <- gDifference(hawaii_shapefile, bf, byid=TRUE)
#pri
#Hawaii
pri_shapefile.split <- gDifference(pri_shapefile, bf, byid=TRUE)

#plot(NE_countries.split) # check map
#class(NE_countries.split) # is a SpatialPolygons object



# =============================================================================
# Create graticules --------MINIMZE THIS 
# =============================================================================

# create a bounding box - world extent
b.box <- as(raster::extent(-180, 180, -90, 90), "SpatialPolygons")
# assign CRS to box
proj4string(b.box) <- WGS84
# create graticules/grid lines from box
grid <- gridlines(b.box, 
                  easts  = seq(from=-180, to=180, by=20),
                  norths = seq(from=-90, to=90, by=10))

# create labels for graticules
grid.lbl <- labels(grid, side = 1:4)

# transform labels from SpatialPointsDataFrame to a data table that ggplot can use
grid.lbl.DT <- data.table(grid.lbl@coords, grid.lbl@data)

# prepare labels with regular expression:
# - delete unwanted labels
grid.lbl.DT[, labels := gsub(pattern="180\\*degree|90\\*degree\\*N|90\\*degree\\*S", replacement="", x=labels)]
# - replace pattern "*degree" with "°" (* needs to be escaped with \\)
grid.lbl.DT[, lbl := gsub(pattern="\\*degree", replacement="°", x=labels)]
# - delete any remaining "*"
grid.lbl.DT[, lbl := gsub(pattern="*\\*", replacement="", x=lbl)]

# adjust coordinates of labels so that they fit inside the globe
grid.lbl.DT[, long := ifelse(coords.x1 %in% c(-180,180), coords.x1*175/180, coords.x1)]
grid.lbl.DT[, lat  := ifelse(coords.x2 %in% c(-90,90), coords.x2*82/90, coords.x2)]

###############################################
# grid labels ADJUSTED BY ME
grid.lbl.DT=grid.lbl.DT[grid.lbl.DT$coords.x1 < 145 ,]
grid.lbl.DT=grid.lbl.DT[grid.lbl.DT$coords.x1 > -50 ,]
#grid.lbl.DT=grid.lbl.DT[grid.lbl.DT$coords.x2 > -60 ,]


# =============================================================================
# Prepare data for ggplot, shift & project coordinates
# =============================================================================

# give the PORJ.4 string for Eckert IV (now mercator) projection
PROJ <- "+proj=longlat +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 

# transform graticules from SpatialLines to a data table that ggplot can use
grid.DT <- data.table(map_data(SpatialLinesDataFrame(sl=grid, 
                                                     data=data.frame(1:length(grid)), 
                                                     match.ID = FALSE)))

###########################
#grid labels ADJUSTED BY ME
#grid.DT=grid.DT[grid.DT$long < 145 ,]
#grid.DT=grid.DT[grid.DT$long > -50 ,]
#grid.DT=grid.DT[grid.DT$lat > -60 ,]


# project coordinates
# assign matrix of projected coordinates as two columns in data table
grid.DT[, c("X","Y") := data.table(project(cbind(long, lat), proj=PROJ))]

# project coordinates of labels
grid.lbl.DT[, c("X","Y") := data.table(project(cbind(long, lat), proj=PROJ))]


#############################################
#PIPA#####################
#############################################

Pipa.DT = data.table(map_data(as(curr_mpa.split, "SpatialPolygonsDataFrame")))
# Shift coordinates
Pipa.DT[, long.new := long + shift]
Pipa.DT[, long.new := ifelse(long.new > 180, long.new-360, long.new)]
# project coordinates 
Pipa.DT[, c("X","Y") := data.table(project(cbind(long.new, lat), proj=PROJ))]

#############################################
#PITCAIRN#####################
#############################################

Pitcairn.DT = data.table(map_data(as(pitcairn_shapefile.split, "SpatialPolygonsDataFrame")))
# Shift coordinates
Pitcairn.DT[, long.new := long + shift]
Pitcairn.DT[, long.new := ifelse(long.new > 180, long.new-360, long.new)]
# project coordinates 
Pitcairn.DT[, c("X","Y") := data.table(project(cbind(long.new, lat), proj=PROJ))]

#############################################
#NAZCA#####################
#############################################

Nazca.DT = data.table(map_data(as(nazca_shapefile.split, "SpatialPolygonsDataFrame")))
# Shift coordinates
Nazca.DT[, long.new := long + shift]
Nazca.DT[, long.new := ifelse(long.new > 180, long.new-360, long.new)]
# project coordinates 
Nazca.DT[, c("X","Y") := data.table(project(cbind(long.new, lat), proj=PROJ))]


#############################################
#HAWAII#####################
#############################################

Hawaii.DT = data.table(map_data(as(hawaii_shapefile.split, "SpatialPolygonsDataFrame")))
# Shift coordinates
Hawaii.DT[, long.new := long + shift]
Hawaii.DT[, long.new := ifelse(long.new > 180, long.new-360, long.new)]
# project coordinates 
Hawaii.DT[, c("X","Y") := data.table(project(cbind(long.new, lat), proj=PROJ))]


#############################################
#PRI#####################
#############################################

pri.DT = data.table(map_data(as(pri_shapefile.split, "SpatialPolygonsDataFrame")))
# Shift coordinates
pri.DT[, long.new := long + shift]
pri.DT[, long.new := ifelse(long.new > 180, long.new-360, long.new)]
# project coordinates 
pri.DT[, c("X","Y") := data.table(project(cbind(long.new, lat), proj=PROJ))]

#############################################
#WORLD#####################
#############################################

# transform split country polygons in a data table that ggplot can use
Country.DT <- data.table(map_data(as(NE_countries.split, "SpatialPolygonsDataFrame")))

# Shift coordinates
Country.DT[, long.new := long + shift]
Country.DT[, long.new := ifelse(long.new > 180, long.new-360, long.new)]
# project coordinates 
Country.DT[, c("X","Y") := data.table(project(cbind(long.new, lat), proj=PROJ))]

#cut off some countries
Country.DT=Country.DT[Country.DT$long.new < 145 ,]
Country.DT=Country.DT[Country.DT$long.new > -50 ,]
Country.DT=Country.DT[Country.DT$lat > -60 ,]
Country.DT=Country.DT[Country.DT$lat < 78 ,]



# =============================================================================
# Plot
# =============================================================================

#png(filename="../Figures/Final_Figures/Figure1transparentfill.5.png",
#    units="in", width=4, height=4, res=300)

ggplot() + 
  geom_polygon(data = Country.DT, # add projected countries
               aes(x = X, y = Y, group = group), 
               colour = "black", 
               fill = "black", 
               size = 0.25) +
  
 #add a bounding box (select graticules at edges)
#     geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                     |(long %in% c(-180,180) & lat %in% c(-90,90) & region == "EW")], 
#    aes(x = X, y = Y, group = group), 
#      linetype = "solid", colour = "black", size = .3) +
  

  
# add graticule labels
# geom_text(data = grid.lbl.DT, # latitude
#            aes(x = X, y = Y, label = lbl), 
#           colour = "black", size = 5) +
  
#add MPAs
  geom_polygon(data = Pipa.DT,
              aes(x = X, y = Y, group = group),
              fill = 'red',
              alpha=0.3,
              col = "red",
              size = .5
  ) +
  geom_polygon(data = Pitcairn.DT,
               aes(x = X, y = Y, group = group),
               fill = 'blue',
               alpha=0.3,
               col = "blue",
               size = .5
  ) +
  geom_polygon(data = Nazca.DT,
               aes(x = X, y = Y, group = group),
               fill = 'forestgreen',
               alpha=0.3,
               col = "forestgreen",
               size = .5
  ) +
  geom_polygon(data = Hawaii.DT,
               aes(x = X, y = Y, group = group),
               fill = 'goldenrod3',
               alpha=0.3,
               col = "goldenrod3",
               size = .5
  ) +
  geom_polygon(data = pri.DT,
               aes(x = X, y = Y, group = group),
               fill = 'purple',
               alpha=0.3,
               col = "purple",
               size = .5
  ) +
  coord_equal() + # ensures that one unit on the x-axis is the same length as one unit on the y-axis
  theme_void()  + # set empty theme
  scale_colour_manual(values = c("red", "blue", "green", 'orange', 'purple'))
  
  
  #attempts with adding a legend
  scale_color_manual("Line.Color", values=c(red="red",green="green",blue="blue", orange="orange", red="red"))
  
  
  scale_fill_manual(
    name   = 'Margin',
    breaks = c('upper', 'lower'), # <<< corresponds to fill aesthetic labels
    values = c(lightGreen, lightRed),
    labels = c('Over', 'Under'))
  


  
  scale_colour_manual(name = 'the colour', 
                      values =c('black'='black','red'='red'), labels = c('c2','c1'))
  
  scale_fill_manual(values = c('darkgreen', 'red', 'yellow'),
                    labels = c('Above Mean', 'At Mean', 'Below Mean'))
  


plotclr <- c("red","yellow", "darkgreen")
###SAVING

#dev.off()

# Save as png and pdf file
#ggsave("gallery/Pacific centered world map with ggplot.png", 
#       width=15, height=8, units="cm", dpi=75)

#ggsave("gallery/Pacific centered world map with ggplot.pdf", 
#       width=15, height=8, units="cm")