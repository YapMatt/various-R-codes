
library(rgl)
library(rgdal)
library(raster)
WA_sa1 <- readOGR(dsn="Z:/Yap Project folder/source files/SA1 correspondence/shapefiles/SA1/SA1_2011_WA.shp", layer="SA1_2011_WA")
 ## subset shapefile to WheatBelt only
PerthSA1 <- subset(WA_sa1,substr(SA4_NAME11,1,3) == "Per")
b <- as(extent(PerthSA1),'SpatialPolygons')

library(ggplot2)
library(rasterVis)
library(dplyr)
library(rgeos)
library(sf) ## for st_union which performs a 'dissolve' on the shapefile
library(ggmap)
library(maptools)

sa1_count <- dim(PerthSA1@data)[1]
id2 <- c(rep("1",sa1_count))
plot(unionSpatialPolygons(PerthSA1,id2))

PerthSA1_f <- fortify(PerthSA1)
PerthSA1$id <- row.names(PerthSA1)
PerthSA1_f <- left_join(PerthSA1_f, PerthSA1@data)

register_google(key="AIzaSyCBlVN7qYuBhbhmBoGZM9XueK0hnr7bzzw")
PerthGoogle <- get_map(location = c(lon=115.9, lat=-32.0),
    color='color',
    source='google',
    maptype='roadmap',
    zoom=10)

# courtesy R Lovelace : https://gis.stackexchange.com/questions/155334/ggmap-clip-a-map
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}
PerthGoogle.rast <- ggmap_rast(map = PerthGoogle) # convert google map to raster object
PerthOnly <- mask(PerthGoogle.rast,unionSpatialPolygons(PerthSA1,id2))
# prep raster as a data frame for printing with ggplot
Perth.df <- data.frame(rasterToPoints(PerthOnly))
ggplot(Perth.df) + 
  geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + 
  scale_color_identity() + geom_polygon(dat = PerthSA1_f,
		aes(x = long,y = lat,group = group), #colour="black",
			fill=NA, size=0.5, alpha=0.4)
