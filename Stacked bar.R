#参考跟着Nature学作图:R语言ggplot2堆积柱形图完整示例
rm(list=ls())
library(readxl)
dat01<-read_excel("C:\\Users\\11485\\Desktop\\南海所\\浮游软体动物研究\\ST08论文\\ST08数据\\文章数据-新\\作图\\堆叠柱状图\\Bargraph.xlsx", sheet = 1)
head(dat01)
library(ggplot2)

ggplot(dat01,aes(x=className,y=n,fill=rlCodes))+
  geom_bar(stat = "identity",
           position = "fill",width = 0.6)+ #百分比用fill，否则用stack
  scale_fill_manual(values = c("#FFF0CF","#FFEABB","#FFE2A1", "#FFDA88" ,"#FFD473", "#FFCD5C", "#FFC440", "#FFBB22","#FFB000","#EDA400","#D69400", "#BC8200", 
                               "#9E6D00",  "#875D00", "#724F00", "#593D00", "#422E00","#24DD31","#00A50C","#014F07",
                               "#F2F200",  "#C9C900", "#999900","#707000", "#E3E3FF", "#C2C2FF", "#9F9FFF", "#7676FF", "#4A4AFF", "#0000FF",
                               "#0000D1","#0000A0", "#000072", "#000044", "#DE00EA", "#A100AA", "#6C0072", "#E20000", "#A80000", "#680000")) +
  guides(fill=guide_legend(reverse=F,title=NULL,ncol=2),size=20)+

  labs(x="group",y="Relative abundance/%")+theme_classic()+
  theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),
        axis.text.x = element_text(size=20),axis.text.y = element_text(size=20),

        text=element_text(family = "serif"),legend.position = "right",legend.key.size = unit(18,"pt")) #更改图标大小
