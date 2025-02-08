rm(list = ls())
#install.packages("maptools")
#install.packages("ggmap")
#---------------以下内容不需要修---------------#
#-----调用包
#library(maptools)
library(ggplot2)
library(plyr)
library(ggmap)
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
Bathy <- getNOAA.bathy(lon1 = 106, lon2 = 140, lat1 = 14, lat2 = 25,
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
#---------------需要修改的地方---------------#
#-----读取数据
#水柱积分数据
setwd("C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\ST08数据\\文章数据-新\\作图\\优势种柱状图")
data<-read.csv("winter-bar.csv")





#-----设置xy范围
ymin<-17
ymax<-23
xmin<-108.5
xmax<-114


#-----做柱形图
# 【每个柱子颜色不同】，如果希望所有柱子都是灰色，就都改成 color="#404040" 
# 【注意】修改每一列的名称

#---------------需要修改的地方---------------#
#当原始数据的取值范围是10-100的时候，ratio<-990合适
# 表层和底层数据用990，水柱积分用990*70
ratio<-990
size<-1.3
xgap<-0.038

## 可视化
p<-ggplot(global, fill = "gray", color = "black") +
  geom_sf()+
  xlab("Longitude") + 
  ylab("Latitude") +
  scale_x_continuous(limits = c(108.5, 114), breaks = seq(109, 114, 1), 
                     labels = c("109°E", "110°E","111°E","112°E","113°E","114°E")) +
  scale_y_continuous(limits = c(17, 23), breaks = seq(17, 23, 1), 
                     labels = c("17°N","18°N", "19°N", "20°N", "21°N", "22°N","23°N")) +
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
   #                      pad_x = unit(0.2, "cm"),
   #                      pad_y = unit(0.2, "cm"),) +
  #geom_raster(data=bf[bf$z<=0,],aes(x, y, fill=z))+
  #coord_quickmap()+
  #scale_fill_distiller(palette="Blues", na.value="white")+
  geom_contour(data = bf, aes(x = x, y = y, z = z), breaks = c( -50, -100, -200), size = c(0.3),colour = "gray" ) +
  theme_bw() +
  #geom_errorbar(aes(x=long-2*xgap, ymin=lat, ymax=lat+Atlanta.spp./ratio), data=data, size=size, color="#FFA500", width=0, alpha=1)+  # 画sp1的分布
  geom_errorbar(aes(x=long-xgap, ymin=lat, ymax=lat+Creseis.acicula/ratio), data=data, size=size, color="#000080", width=0, alpha=0.8)+   # 画sp2的分布
  #geom_errorbar(aes(x=long+xgap, ymin=lat, ymax=lat+Desmopterus.papilio/ratio), data=data, size=size, color="#478b00", width=0, alpha=0.8)+  # 画sp3的分布
  geom_errorbar(aes(x=long+xgap, ymin=lat, ymax=lat+Heliconoides.inflatus/ratio), data=data, size=size, color="#CD00CD", width=0, alpha=0.8)+  # 画sp3的分布
  geom_errorbar(aes(x=long-3*xgap, ymin=lat, ymax=lat+Limacina.bulimoides/ratio), data=data, size=size, color="#CD0000", width=0, alpha=0.8)+  # 画sp4的分布
  geom_errorbar(aes(x=long+3*xgap, ymin=lat, ymax=lat+Limacina.trochiformis/ratio), data=data, size=size, color="#8B4513", width=0, alpha=0.8)+  # 画sp5的分布
  #geom_errorbar(aes(x=long, ymin=lat, ymax=lat+Legend/ratio), data=data, size=size, color="#404040", width=0, alpha=1)+  # 画legend
  ylim(ymin,ymax)+xlim(xmin,xmax) # 设置x，y轴选择范围

p
