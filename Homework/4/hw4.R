install.packages("sf")
install.packages("sp")
install.packages("terra")
install.packages("spData")
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")
install.packages("dplyr")
install.packages("tmap")
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(dplyr)
library(tmap)
library(grid)
library(sp)
chn<-st_read(dsn="~/Desktop/phD/Courses/R application in ecology/4/Exercise/province_albers.shp",
             options="ENCODING=GB2312")
res<-st_read(dsn="~/Desktop/phD/Courses/R application in ecology/4/Exercise/res2_4m.shp",
             options="ENCODING=GB2312")
chn_WGS84<-sf::st_transform(chn,4326) 
st_crs(chn_WGS84)
chn_WGS84_crop<-st_crop(chn_WGS84,c(xmax=110,ymin=35,xmin=70,ymax=50))
res_crop<-st_crop(res,c(xmax=110,ymin=35,xmin=70,ymax=50))
chn_f<-sf::st_transform(chn_WGS84_crop,"ESRI:102025")
res_f<-sf::st_transform(res_crop,"ESRI:102025")
map<-tm_shape(chn_f)+tm_polygons("NAMEEN")+tmap_options(max.categories = 32)+
  tm_shape(res_f)+tm_dots("PINYIN")+
  tm_shape(res_f)+tm_text("PINYIN",size=0.4)+
  tm_legend(show=FALSE)
map
