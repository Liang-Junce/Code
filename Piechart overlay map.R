rm(list = ls())
#install.packages("sf")
library(sf)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggspatial")
library(ggspatial)
#install.packages("marmap")
library(marmap)
#install.packages("dplyr")
library(dplyr) 
#library(rJava)
#library(xlsxjars)
#library(xlsx)
library(readxl) 
#install.packages("reshape")
library(reshape)  
#install.packages("rgdal")
library(rgdal)
#install.packages("marmap")
library(marmap) # 下载水深数据
#install.packages("directlabels")
library(directlabels) # 使用geom_dl添加标签
#install.packages("rgdal")
library(rgdal) # 读取地图数据
library(ggplot2) # 画图
library(marmap) # 下载水深数据
library(directlabels) # 使用geom_dl添加标签
#install.packages("scatterpie")
library(scatterpie) # 做复合气泡图
#install.packages("vegan")
library(vegan) 

# 设置环境
getwd()
setwd("C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\作图代码\\R专用底图")
windowsFonts(Times = windowsFont("Times New Roman"),
             ArialMT = windowsFont("Arial"))

# 载入地图数据
global<-readOGR(dsn="C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\作图代码\\R专用底图") 

global <- sf::read_sf("i\\GSHHS_i_L1.shp")
class(global)

## 下载水深数据
Bathy <- getNOAA.bathy(lon1 = 108, lon2 = 115, lat1 = 16, lat2 = 24,
                       resolution =1)
class(Bathy) # 目前为水深数据bathy,需要进行转化


## 数据转换
bathyLon = as.numeric(rownames(Bathy))
bathyLat = as.numeric(colnames(Bathy))
bathyZ = as.numeric(Bathy)
dim(bathyZ) = dim(Bathy) # dim() 查看维度
bf = fortify.bathy(Bathy)
class(bf)
head(bf)
#write.csv(bf, "bf.csv", col.names= T, row.names = T) ## 读出水深数据，可用于surfer作图。但是需要将陆地高度删除

## 可视化
p<-ggplot(global, fill = "gray", color = "black") +
  geom_sf()+
  xlab("Longitude") + 
  ylab("Latitude") +
  scale_x_continuous(limits = c(109, 114), breaks = seq(109, 114, 1), 
                     labels = c("109°E", "110°E", "111°E", "112°E", "113°E", "114°E")) +
  scale_y_continuous(limits = c(17, 22), breaks = seq(17, 22, 1), 
                     labels = c("17°N", "18°N", "19°N", "20°N", "21°N", "22°N")) +
  #annotation_scale(location = "bl",
  #                 text_cex =0.5, text_family = "Times", line_width = 0.5,
  #                 height = unit(0.1, "cm"),
  #                 pad_x = unit(0.2, "cm"),
  #                 pad_y = unit(0.2, "cm"),
  #                 text_pad = unit(0.15, "cm"),
  #                 tick_height = 0.6) +
  #annotation_north_arrow(location = "tl", which_north = "true", 
  #                       style = north_arrow_nautical,
  #                       height = unit(1, "cm"),
  #                       width = unit(1, "cm"),
  #                       pad_x = unit(0.2, "cm"),
  #                       pad_y = unit(0.2, "cm"),) +
  #geom_raster(data=bf[bf$z<=0,],aes(x, y, fill=z))+
  #coord_quickmap()+
  #scale_fill_distiller(palette="Blues", na.value="white")+
  geom_contour(data = bf, aes(x = x, y = y, z = z), breaks = c( -50, -100, -200), size = c(0.3),colour = "gray" ) +
  theme_bw() 
p

d<-read.csv("C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\ST08数据\\文章数据-新\\作图\\优势种丰度饼图\\冬季异足类亚目.csv")
d$abundance<-rowSums(d[,-1:-3])
colnames(d)
abundance<-d$abundance
abundance
station<-d$Station
lon<-d$lon
lat<-d$lat
#π*radius^2=abundance
radius <-sqrt(d$abundance/pi)#由丰度计算圆的半径

Max_radius<-max(radius)#计算得到最大半径
Max_radius<-80#为了统一各个季节圆的半径，统一将Max_radius设定为850，调节圆的大小
Bubble_Scale<-1#radius的倍数，可自行调整以美化图形
d$radius <-Bubble_Scale*radius/Max_radius
d
#col<-c("#1B1BB3", "#0B61A7", "#00AF64","#FFA500", "#FFFF00","#FFB600")
#colnames(gc)
col<-c("#FF83FA","#87CEEB","#551A8B")

p1<-p+ geom_scatterpie(aes(x=lon, y=lat, group=station, r=radius),data=d,
                      cols=colnames(d[4:6]),
                      alpha=0.8,color="black",size=0.01)+
  scale_fill_manual(values=col,limits=c("Atlantidae","Carinariidae","Pterotracheidae"))+
  geom_scatterpie_legend(d$radius, x=113.2, y=20.85,n=4,
                         labeller=function(x) round((x*Max_radius/Bubble_Scale)^2*pi,0))+ #调节Legend保留小数位数
  theme(legend.text=element_text(size=10), legend.title = element_blank(),legend.position = "right")+
  annotate("text",x=113,y=18.2,label=expression(paste("Abundance ","(ind/100",m^3,")")),size=4)

#scale_fill_manual(values=col,
#limits=c("Atlanta spp.","Creseis acicula","Desmopterus papilio","Limacina bulimoides",
 #"Limacina trochiformis"))
p1
ggsave("饼图2.png",p1,width=7.26,height =5.77)

