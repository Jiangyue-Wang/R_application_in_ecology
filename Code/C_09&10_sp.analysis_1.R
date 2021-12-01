
install.packages("sp")
install.packages("maptools")
install.packages("rgdal")
install.packages("fields")
install.packages("rgeos")
install.packages("spdep")
install.packages("raster")

## libraries needed: sp, maptools, rgdal
library(sp); library(maptools); library(rgdal)
library(fields); library(rgeos); library(spdep)
library(raster)

## set the working directory
setwd("E:/My_Teaching/R_group/09&10 Spatial analysis with R/")

########################################################################################
## 1 input & output data layers
## four major data formats: shape polygons, shape polylines, shape points, and ASCII rasters
########################################################################################

## example 1: read a polygon data layer
conti <- readShapePoly(fn="continents.shp")
names(attributes(conti))

## more updated way
conti_1 <- readOGR(dsn = "continents.shp")
names(attributes(conti_1))

## extract data
conti$data
conti@data
conti@polygons
conti@proj4string
conti@bbox


## plot
## plot it out
if (Sys.info()["sysname"]=="Windows") windows(width=10, height=5); par(mar=c(2,4,2,4), xaxs="i", yaxs="i")
if (Sys.info()["sysname"] == "Darwin") quartz(width=10, height=5); par(mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
plot(conti, col="lightgray", border="gray", bg="skyblue", axes=F, xlim=c(-180, 180), ylim=c(-90,90))

cn <- readShapePoly(fn="e:/my_data/Boundary4M/probox.shp")
cn.l <- readShapeLines(fn="e:/my_data/Boundary4M/china_border.shp")
plot(cn, border="transparent", col="red", add=TRUE)
plot(cn.l, col="red", add=TRUE)

## select some specific polygons out
str(conti@data)
con <- !is.na(conti@data$CONTINENTS)
conti.1 <- conti[con,]
plot(conti.1, col="lightgreen", border="black", add=TRUE)

## write a polygon data layer out to the hard disk
writePolyShape(x=conti.1, fn="continents_1")

## more updated way
writeOGR(obj = conti.1, dsn = "aa", layer = "continents_2", driver = "ESRI Shapefile")


## example 2: read/write a polyline layer
latlong <- readShapePoly(fn="latlong.shp") ## Error message
latlong <- readShapeLines(fn="latlong.shp")
names(attributes(latlong))
## more updated way
latlong_1 <- readOGR(dsn="latlong.shp")
names(attributes(latlong_1))

## plot
plot(latlong, add=TRUE, col="lightgray")
class(latlong)

## select some lines and write them out
str(latlong@data)
con <- latlong@data$VALUE %in% c("0","90S", "60S", "30S", "30N", "60N", "90N", "180W", "150W", "120W", 
				"90W", "60W", "30W","30E", "60E", "90E", "120E", "150E","180E")
latlong.1 <- latlong[con,]
plot(latlong.1, lwd=0.5, col="gray", add=TRUE)
axis(side=1, at=c(-120, -60, 0, 60, 120), lab=c("120W", "60W", "0", "60E", "120E"), tck=-0.02, mgp=c(1,0.5,0))
axis(side=2, at=c(-60, -30, 0, 30, 60), lab=c("60S", "30S", "0", "30N", "60N"), tck=-0.02, mgp=c(1,0.5,0))
box()

## write a polyline shapefile out
writeLinesShape(x=latlong.1, fn="latlong_1")




## example 3: read/write a spatial point data layer
city <- readShapePoly(fn="CITIES.shp") ## Error message
city <- readShapeLines(fn="CITIES.shp") ## Error message
city <- readShapePoints(fn="CITIES.shp")
## more updated way
city_1 <- readOGR(dsn="CITIES.shp")


## plot
plot(city, add=TRUE, col="red", pch=1)

## write it out
writePointsShape(x=city, fn="City_1")



## example 4: read/write ASCII raster files
mat <- read.asciigrid(fname="MAT.ASC", colname="MAT") # mean annual temperature with a resolution of 1*1 degree
map <- read.asciigrid(fname="MAP.ASC", colname="MAP") # mean annual precipitation with a resolution of 1*1 degree

## plot them out
if (Sys.info()["sysname"]=="Windows") windows(width=10, height=10); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
if (Sys.info()["sysname"] == "Darwin") quartz(width=5, height=5); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
col <- tim.colors(n=100, 1)
image(mat, bg="lightblue", col=col)
plot(conti, add=TRUE, col="transparent", border="black")

breaks <- c(0, exp(seq(0, max(log(map$MAP),na.rm=T), length.out=100)))
image(map, bg="lightblue", col=col, breaks=breaks)
plot(conti, add=TRUE, col="transparent", border="black")

## write an image to an ASCII file
write.asciigrid(x=mat, fname="MAT-1.ASC")





########################################################################################
## 2 make data layers manually
########################################################################################

## example 1: make a point layer: using the long&lat of South American grid
SA <- read.csv("data_SouthAmerica_MacEco_Course.csv", header=T)
sites <- SpatialPointsDataFrame(coords = SA[,c(2:3)], data = SA[,c(4:6)])
plot(sites, add=FALSE, pch=19, col="blue", cex=0.1)

## write it out
writePointsShape(sites, fn = "SA_pts.shp")



## example 2: make a point layer: using the long&lat of South American grid
sites.1 <- SA[,c(1:3)]
coordinates(sites.1) <- c(2:3) ## sites must be a data.frame here
plot(sites.1, add=FALSE, pch=19, col="blue", cex=0.1)



## example 3: make a polygon layer
xy <- cbind(x=c(5, 5, 40, 40, 5), y=c(45,65,65,45,45))
po <- list(); po[[1]] <- Polygon(xy, hole=NA)
po1 <- list(); po1[[1]] <- Polygons(po, ID="1")
po2 <- SpatialPolygons(po1)
po3 <- SpatialPolygonsDataFrame(po2, data=data.frame(ID=1))
plot(po3, add=TRUE, col="transparent", border="black", lwd=2) ## to conti layer

## write it out
writePolyShape(po3, fn = "po3.shp")



## example 4: make a raster layer
gridded(sites) <- TRUE
class(sites)
plot(sites)






########################################################################################
## 3 projections
########################################################################################

## check the information of all projections
projInfo()

## define the project of the coordinate system: Geographic
proj4string(conti) <- CRS("+proj=longlat +datum=WGS84")
proj4string(latlong.1) <- CRS("+proj=longlat +datum=WGS84")

## project the layer into another project
conti.proj <- spTransform(x=conti, CRSobj = CRS("+proj=wintri +lat_1=50.466999999999999n +lon_0=162e"))
latlong.proj <- spTransform(latlong.1, CRSobj = CRS("+proj=wintri +lat_1=50.466999999999999n +lon_0=162e"))

## plot it out and compare it with original one
if (Sys.info()["sysname"]=="Windows") windows(width=10, height=10); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
if (Sys.info()["sysname"] == "Darwin") quartz(width=5, height=5); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
plot(conti, col="white", border="black", bg="lightblue", axes=FALSE, xlim=c(-180, 180), ylim=c(-90,90))
plot(conti.1, col="lightgreen", border="black", add=TRUE)
plot(latlong.1, lwd=2, col="gray", add=TRUE)

plot(conti.proj, col="lightgreen", border="black", bg="lightblue", axes=FALSE)
plot(latlong.proj, lwd=2, col="gray", add=TRUE)

## write the projected layers out
writePolyShape(x=conti.proj, fn="continents_proj")
writeLinesShape(x=latlong.proj, fn="latlong_1_proj")



## Map of China with different projections
prov <- readShapePoly(fn ="E:/My_Data/Boundary4M/province_albers.shp")
prov.geo <- readShapePoly(fn ="E:/My_Data/Boundary4M/province.shp")

plot(prov);plot(prov.geo)
proj4string(prov.geo) <- CRS("+proj=longlat")
prov.geo1 <- spTransform(prov.geo, CRS("+proj=aea +lat_1=25n +lat_2=47n +lat_0=0n +lon_0=110e +ellps=clrk66"))
prov.geo2 <- spTransform(prov.geo, CRS("+proj=aea +lat_1=25n +lat_2=47n +lat_0=0n +lon_0=90e +ellps=clrk66"))

plot(prov.geo1); plot(prov.geo2)









########################################################################################
## 4 overlay analysis
## similar to the overlay (e.g. identity) analysis in ArcGIS
########################################################################################

## example 1: to identify the polygon in which a point is located
## method 1
plot(conti); plot(city, add=T)

proj4string(city) <- CRS("+proj=longlat")
proj4string(conti) <- CRS("+proj=longlat")
sites.conti.1 <- over(x=city, y=conti) # the result is a vecotr showing the index of polygons in which a point is located
sites.conti.1 


## method 2
sites.conti.2 <- over(x=conti, y=city) # the result is a data.frame showing the info of the polygon in which a point in located
sites.conti.2


## example 2: to extract the grid cell value of points from a raster layer (SpatialGridDataFrame)
## plot them out
if (Sys.info()["sysname"]=="Windows") windows(width=10, height=10); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
if (Sys.info()["sysname"] == "Darwin") quartz(width=5, height=5); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
col <- tim.colors(n=100, 1)
image(mat, bg="lightblue", col=col)
plot(city, pch=19, add=T)
plot(conti, add=TRUE, col="transparent", border="black")

breaks <- c(0, exp(seq(0, max(log(map$MAP),na.rm=T), length.out=100)))
image(map, bg="lightblue", col=col, breaks=breaks)
plot(city, pch=19, add=T)
plot(conti, add=TRUE, col="transparent", border="black")

proj4string(mat) <- CRS("+proj=longlat")
proj4string(map) <- CRS("+proj=longlat")

sites.mat <- over(x=city, y=mat)
city@data <- cbind(city@data, mat=sites.mat[,1])
sites.map <- over(x=city, y=map)
city@data <- cbind(city@data, map=sites.map[,1])

## write the point layer out
writePointsShape(city, "city_clim.shp")


## example 3: get the intercection of two polygon layers 1
xy <- cbind(x=c(90, 90, 180, 180, 90), y=c(-50,30,30,-50,-50))
po <- list(); po[[1]] <- Polygon(xy, hole=NA)
po1 <- list(); po1[[1]] <- Polygons(po, ID="1")
po2 <- SpatialPolygons(po1); proj4string(po2) <- CRS("+proj=longlat")
islands.box <- SpatialPolygonsDataFrame(po2, data=data.frame(ID=1))
proj4string(islands.box) <- CRS("+proj=longlat")
islands <- gIntersection(conti, islands.box, byid=T) 

if (Sys.info()["sysname"]=="Windows") windows(width=10, height=10); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
if (Sys.info()["sysname"] == "Darwin") quartz(width=5, height=5); par(mfrow=c(2,1), mar=c(0.5,1,0.5,1), xaxs="i", yaxs="i")
plot(conti, col="lightgreen", border="black", bg="lightblue", axes=FALSE, xlim=c(-180, 180), ylim=c(-90,90))
plot(latlong.1, lwd=2, col="gray", add=TRUE)
plot(islands.box, add=TRUE, col="transparent", border="black", lwd=2)
plot(islands, col="lightgreen", border="black", bg="lightblue", axes=FALSE)



## example 4: get the intercection of two polygon layers 2
grid20d <- readShapePoly(fn="grid20d.shp")
proj4string(grid20d) <- CRS("+proj=longlat")

plot(grid20d, add=T)
conti.grid <- gIntersects(conti, grid20d, byid=TRUE)
dim(conti.grid)
rowSums(conti.grid)

#conti.grid.2 <- gIntersection(conti, grid20d[rowSums(conti.grid)>0, ], byid=TRUE)
#plot(conti.grid)


## example 5: non-overlapped parts of two polygon layers
poly.tmp <- gDifference(conti, islands.box, byid=F)
length(poly.tmp)
poly.tmp <- gDifference(conti, islands.box, byid=T)
length(poly.tmp)
class(poly.tmp)
plot(poly.tmp, col="lightgreen", border="black", bg="lightblue", axes=FALSE)



## example 6: merge polygons
conti.merged <- gUnion(conti, conti, byid=F)
plot(conti.merged, col="lightgreen", border="black", bg="lightblue", axes=FALSE, xlim=c(-180, 180), ylim=c(-90,90))
plot(conti)

plot(islands, add=F)
tmp <- gUnion(poly.tmp, islands, byid=T)
plot(tmp, add=T)
length(tmp)







########################################################################################
## 5 spatial distance
########################################################################################

## example 1: make a point layer: using the long&lat of South American grid
SA <- read.csv("data_SouthAmerica_MacEco_Course.csv", header=T)
head(SA)
dist.SA <- spDists(x = as.matrix(SA[1:10,c(2:3)]), y = as.matrix(SA[1,c(2:3)]), longlat = T)
dist.SA <- spDists(x = as.matrix(SA[1:10,c(2:3)]), y = SA[1:10,c(2:3)], longlat = T)
class(dist.SA)
dim(dist.SA)


## eample 2: comparison
dist.SA.1 <- spDists(x = as.matrix(SA[1:10,c(2:3)]), y = SA[1:10,c(2:3)], longlat = T)
dist.SA.2 <- spDists(x = as.matrix(SA[1:10,c(2:3)]), y = SA[1:10,c(2:3)], longlat = F)
dist.SA.3 <- dist(x = as.matrix(SA[1:10,c(2:3)]), diag = T, upper = T)




