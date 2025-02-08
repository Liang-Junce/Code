rm(list=ls())
library(readxl)
library(dplyr)
library(ade4)
library(vegan)  # 应该先加载ade4后加载vegan以避免冲突
#install.packages("gclus")
library(gclus)
library(cluster)
library(RColorBrewer)
#install.packages("labdsv")
library(labdsv)
#library(readxl)
library(marmap) # 下载水深数据
library(directlabels) # 使用geom_dl添加标签
library(rgdal) # 读取地图数据
library(ggplot2) # 画图
#library(marmap) # 下载水深数据
#library(directlabels) # 使用geom_dl添加标签
library(scatterpie) # 做复合气泡图
library(patchwork)
library(ggsci)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(reshape)
library(ggpubr)
library(ggrepel)
library(readr)
library(lattice)
library(permute)
library(vegan)
library(sf)
library(tidyverse)
library(ggforce)

getwd()
setwd("C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\ST08数据\\文章数据-新\\作图\\RDA分析\\两个区域")

species_raw <- read.csv("冬季物种丰度.csv", row.names = 1)
env <- read.csv("冬季环境因子log.csv", row.names = 1)
species_raw <- decostand(species_raw,method = "hellinger")
print(decorana(species_raw))
rda1=rda(t(species_raw),env[-1],scale=T)
rda1

###提取样本得分
rda.data=data.frame(rda1$CCA$u[,1:2],env$Group)
colnames(rda.data)=c("RDA1","RDA2","Group")
rda.data
###提取物种得分
rda.spe=data.frame(rda1$CCA$v[,1:2])
rda.spe=as.data.frame(rda.spe)
rda.spe
###提取环境因子得分###
rda.env <- rda1$CCA$biplot[,1:2]
rda.env <- as.data.frame(rda.env)
rda.env
###计算轴标签
rda11=round(rda1$CCA$eig[1]/sum(rda1$CCA$eig)*100,2)
rda12=round(rda1$CCA$eig[2]/sum(rda1$CCA$eig)*100,2)
rda11
rda12

#层次分割
#install.packages("rdacca.hp")
library(rdacca.hp)
exp_adj <- RsquareAdj(rda1)$adj.r.squared * rda1$CCA$eig/sum(rda1$CCA$eig)  #获取校正后的 R2
rda1_exp <- paste('RDA1:', round(exp_adj[1]*100, 2), '%')
rda2_exp <- paste('RDA2:', round(exp_adj[2]*100, 2), '%')
plot(rda1, display = c('wa', 'cn'), type = 'n', xlab = rda1_exp, ylab = rda2_exp)  #RDA 的简单作图，只显示样本点和环境变量
text(rda1, display = 'cn', col = 'blue', cex = 0.8)
points(rda1, display = 'wa', pch = 19, cex = 1)

#使用层次分割在 RDA 中分解每个环境变量的解释，详情 ?rdacca.hp
#本示例计算校正后的 R2，RDA 默认使用 Ezekiel 公式计算调整后的 R2
mite.rda.hp <- rdacca.hp(t(species_raw), env[-1], method = 'RDA', type = 'adjR2', scale = FALSE)
mite.rda.hp
plot(mite.rda.hp)

#如需输出层次分割结果
#mite.rda.hp$Hier.part
#write.csv(mite.rda.hp$Hier.part, 'mite.rda.hp.csv')

#画图
windowsFonts(Times = windowsFont("Times New Roman"),
             ArialMT = windowsFont("Arial"))
rda.plot=ggplot(data=rda.data,aes(RDA1,RDA2))+
  geom_point(aes(color=Group,fill=Group,shape=Group),size=3,alpha=0.6)+
  scale_shape_manual(values = c(16,17))+  #自定义点的形状，分别为15， 19， 17
  scale_color_manual(values=c("#0000FF","#EE0000","darkgreen","black","grey","darkgreen"))+ #调节点的颜色，几组就取前几个
  labs(title = NULL,x=paste("RDA1",rda11," %"),y=paste("RDA2",rda12," %"))+
  theme_bw()+
  #geom_text_repel(aes(label=row.names(rda.data)),color="grey40",size=2.5)+ #标注站位名
  theme(axis.title = element_text(family = "serif", face = "bold", size = 18,colour = "black"))+ #坐标轴标题字体
  theme(axis.text = element_text(family = "serif", face = "bold", size = 16,color="black"))+ #坐标轴字体
  geom_mark_ellipse(aes(fill=Group),alpha=0.1)+ # 给散点图按组加上椭圆阴影
  coord_cartesian(clip = "off")
rda.plot 
### 添加环境因子数据，也可以不添加作PCA分析
rda.plot=rda.plot+theme(panel.grid=element_blank())+
  geom_hline(aes(yintercept = 0), colour="gray", linetype="dashed")+ #画网格横虚线
  geom_vline(aes(xintercept = 0), colour="gray", linetype="dashed")+ #画网格竖虚线
  geom_segment(data = rda.spe,aes(x=0, xend=rda.spe[,1], y=0, yend= rda.spe[,2] ), arrow = arrow(length = unit(0.35, "cm")
  ),linetype=1, size=0.6, colour = 'black') + #物种丰度的箭头，unit是箭头大小，size是箭头的粗细，linetype是箭头类型
  geom_text(data = rda.spe,aes(x = rda.spe[,1]*1.1, y = rda.spe[,2]*1.1, label = rownames(rda.spe)), size = 3.5, colour = 'black',check_overlap = TRUE) + #物种丰度的名称
  geom_segment(data=rda.env,aes(x=0,y=0,xend=rda.env[,1],yend=rda.env[,2]),colour="#FF0000",size=1,
               arrow=arrow(angle = 35,length=unit(0.3,"cm")))+ #环境因子的箭头，unit是箭头大小，size是箭头的粗细
  geom_text(data=rda.env,aes(x=rda.env[,1],y=rda.env[,2],label=rownames(rda.env)),size=3.5,
            colour="black", hjust=(1-sign(rda.env[,1]))/2,angle=(180/pi)*atan(rda.env[,2]/rda.env[,1]))+ #环境因子的名称
  theme(legend.position = "top") #修改hjust的参数可以调节标签距离箭头的远近
rda.plot ## 更换数据集绘制另外三个数据集的RDA图
