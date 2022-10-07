a=read.table("C:\\Users\\Moinak\\Desktop\\ST 625\\Spring2019\\diamondrings.txt",header=T)
a

#install.packages("ggplot2",dependencies=TRUE)#
library(ggplot2)

ggplot(a,aes(Caratsize,Price,colour=Certification,shape=Clarity))+
geom_point()+geom_rug()+
ggtitle("Price as a function of caratsize, separated by certification and clarity")