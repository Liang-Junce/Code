rm(list = ls())
library(readxl)
#install.packages("pheatmap")
library(pheatmap) # 作图
library(RColorBrewer) # 颜色包
#install.packages("ggcorrplot")
library(ggcorrplot) 
library(rgdal)
require(corrplot)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("corrplot")
#导入数据
#heat<-read.csv用到上面组合数据才用这行
heat<-read.xlsx("C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\ST08数据\\文章数据-新\\作图\\相关性热图\\冬季相关性热图.xlsx",sheet=1)
heat<-heat[,-1]#删掉第一列

matrix <- cor(heat, method = "spearman")  #“spearman,pearson
matrix <- round(matrix, 2) #保留一位小数
matrix_1 <- matrix[8:10,1:7] # 提取需要的矩阵 ，6:17是丰度1：5是环境因子

# 计算p值，显著性
p_mat <- cor_pmat(heat)
p_mat <- p_mat[8:10,1:7] # 生成数值矩阵

# 生成显著性*矩阵
p_mat_1 <- as.data.frame(p_mat) # 先把数值矩阵转化为数据框
p_mat_1[p_mat_1 < 0.001] <- "***"
p_mat_1[0.001 < p_mat_1 & p_mat_1 < 0.01] <- "**"
p_mat_1[0.01 < p_mat_1 & p_mat_1 < 0.05] <- "*"
p_mat_1[0.05 < p_mat_1] <- ""

p_mat_1 <- as.matrix(p_mat_1) # 把符号数据框转化为矩阵
#png("person.png",width=3000,height=2200)


corrplot(matrix_1, method = "circle",col = colorRampPalette(c("blue","white","#EE2C2C"))(100),
         p.mat=p_mat,
         insig = "label_sig",
         sig.level = c(0.001,0.01,0.05),
         addgrid.col = "gray", #外框线
         pch.cex =0.5,pch.col="black", #pch.cex是显著性标记的大小
         diag = FALSE,tl.srt =45,tl.col = "black",family="serif",
         cutree_cols = 0.1, cutree_rows = 0.1, 
         cluster_rows = F, cluster_cols = F, rect.lwd =0.8,tl.cex =1,
         cl.cex= 1,cl.ratio = 0.5,cl.offset=0.2, #cl.cex是字体大小，cl.ratio是宽度，cl.offset是标签与图例的偏移量
         #gaps_col = c(4), gaps_row = c(2, 4),
         
         breaks = seq(-0.6, 0.6, length.out = 100),
         legend_breaks = c(-0.6, 0.6))

#dev.off()