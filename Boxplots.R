rm(list=ls())
library(ggplot2)
library(ggpubr) # 继承ggplot语法
library(patchwork) # 拼图包
library(ggsci) #配色包
library(tidyverse)
library(rstatix)
#data(mpg)
#head(mpg)
#unique(mpg$class)
setwd("C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\ST08数据\\文章数据-新\\作图\\箱线图\\冬季优势种\\两个区域") #要改

mpg <- read.csv("冬季优势种.csv") #要改
mycol<-c("#3A5FCD","#CD0000")
p=ggboxplot(mpg,x="Year",y="abundance",fill="group",color="group",alpha = 0.5,palette = mycol,ylim=c(0,250), #y值要改
          outlier.shape = NA)+ 
  #geom_rect(xmin = 1.5, xmax = 2.5,ymin = -300, ymax = 2000, fill ="#E9E9E9") + #y值要改
  #geom_rect(xmin = 3.5, xmax = 4.5,ymin = -300, ymax = 2000, fill ="#E9E9E9") +
  #add = "jitter",add.params = list(size=3,jitter=0.1,alpha=0.5),
  #stat_compare_means(method = "kruskal.test",label.y = 6,label.x = 1,label = "p.format")+
  #stat_compare_means(comparisons = list(c("Con1","Con2"), c("Con1","H1"),c("Con1","H2"),c("Con1","L1"),c("Con1","L2"),
  #                                      c("Con2","H1"),c("Con2","H2"),c("Con2","L2"),c("Con2","L2"),
  #                                      c("H1","H2"),c("H1","L1"),c("H1","H2"),
  #                                      c("H2","L1"),c("H2","L2"),
  #                                      c("L1","L2")),method="wilcox",label="p.signif")+
  #geom_jitter(aes(color = group),alpha = 0.3,
  #            position = position_jitterdodge(dodge.width = 0.8))+
  stat_summary(mapping=aes(group=group),fun = "mean", geom = "point", shape = 16 , size=3,color="black", position=position_dodge(0.8))+ #color也可能用fill
  #stat_compare_means(aes(group = group),method = "wilcox.test",label = "p.signif",
  #                   label.y = 11)+ #y值要改
  theme(panel.grid = element_line(color = NA),panel.grid.minor = element_line(color = NA),
        legend.position="right",
        panel.border = element_rect(fill = NA,color = "black"),
        axis.text.x = element_text(size = 10,face = "plain",hjust = 0.5),##italic是斜体，plain是原来的，bold是加粗
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10,face = "plain",hjust = 0.5),
        axis.title.y = element_text(vjust=0.2,size=12,face="bold"))
p

