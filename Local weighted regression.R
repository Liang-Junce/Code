rm(list=ls())
# install.packages("ggplot2")  # 可选，用于绘图
library(ggplot2)

setwd('C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\ST08数据\\文章数据-新\\作图\\指示种\\局部加权回归')

# 读取三个物种的丰度和水体环境数据
dat1 <- read.delim('Creseis局部加权回归.txt', sep = '\t', row.names = 1)
dat2 <- read.delim('Limacina局部加权回归.txt', sep = '\t', row.names = 1)
dat3 <- read.delim('Heliconoides局部加权回归.txt', sep = '\t', row.names = 1)

# 假设你有三个物种的数据
dat1 <- data.frame(x = dat1$Tem, y = dat1$Creseis.acicula, Species = "Creseis")
dat2 <- data.frame(x = dat2$Tem, y = dat2$Limacina.bulimoides, Species = "Limacina")
dat3 <- data.frame(x = dat3$Tem, y = dat3$Heliconoides.inflatus, Species = "Heliconoides")

# 合并所有物种的数据
dat_all <- rbind(dat1, dat2, dat3)

# 计算每个物种的 loess 回归
loess_model_1 <- loess(y ~ x, data = dat1, span = 0.7)
loess_model_2 <- loess(y ~ x, data = dat2, span = 0.7)
loess_model_3 <- loess(y ~ x, data = dat3, span = 0.7)

# 创建预测数据框
predictions1 <- predict(loess_model_1, se = TRUE)
dat1$predicted <- predictions1$fit
dat1$se <- predictions1$se
alpha <- 0.05
dat1$lower <- dat1$predicted - qnorm(1 - alpha / 2) * dat1$se
dat1$upper <- dat1$predicted + qnorm(1 - alpha / 2) * dat1$se

predictions2 <- predict(loess_model_2, se = TRUE)
dat2$predicted <- predictions2$fit
dat2$se <- predictions2$se
dat2$lower <- dat2$predicted - qnorm(1 - alpha / 2) * dat2$se
dat2$upper <- dat2$predicted + qnorm(1 - alpha / 2) * dat2$se

predictions3 <- predict(loess_model_3, se = TRUE)
dat3$predicted <- predictions3$fit
dat3$se <- predictions3$se
dat3$lower <- dat3$predicted - qnorm(1 - alpha / 2) * dat3$se
dat3$upper <- dat3$predicted + qnorm(1 - alpha / 2) * dat3$se

# 合并所有物种的预测结果
dat_all <- rbind(dat1, dat2, dat3)

# 绘图
ggplot(dat_all, aes(x = x, y = y, color = Species)) +
  geom_point() +
  geom_line(aes(y = predicted), size = 1) +
  geom_ribbon(aes(y = predicted, ymin = lower, ymax = upper, fill = Species), alpha = 0.2, color = NA) +
  theme_minimal() +
  labs(title = "三个物种的局部加权回归", x = "X 值", y = "Y 值") +
  theme(legend.title = element_blank())