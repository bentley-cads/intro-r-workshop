library(ggplot2)
library(ggcorrplot)

a=read.table("C:\\Users\\Moinak\\Desktop\\ST 625\\Spring2019\\estress.txt",header=T)
a

#----Creating a scattercloud matrix----#

pairs(a,panel = function(...) smoothScatter(...,cex=4, add = TRUE))

#----Creating a heatmap out of the pairwise correlation values----#
corr = round(cor(a), 2)
ggcorrplot(corr,title="Correlation heatmap among economic stress variables")

#----If you'd rather prefer circles---------#
ggcorrplot(corr, method = "circle",title="Correlation heatmap among economic stress variables")


library(ggplot2)
library(GGally)
a=read.table("C:\\Users\\Moinak\\Desktop\\ST 625\\Spring2019\\estress.txt",header=T)
a
gender=a[,5]
gendermf=ifelse(gender==1,"Female","Male")

a.m=a[,-5]
a.modified=cbind(a.m,gendermf)
a.modified
ggplot(a.modified,aes(estress,withdraw,colour=gendermf))+
geom_point()+geom_rug()

ggpairs(a.modified,mapping=ggplot2::aes(colour = gendermf))