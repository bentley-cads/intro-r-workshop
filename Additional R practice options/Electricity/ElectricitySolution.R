library(ggplot2)
library(forecast)
library(fpp2)
a=read.table("C:\\Users\\Moinak\\Desktop\\MA611, Fall2019\\Data Sets\\Chapter 1\\electricity.txt",header=T)
a

bill=a[,4]
bill
summary(bill)
which(bill=="*")

bill.num=as.numeric(levels(bill))[bill]
bill.num

bill.num[37]=(bill.num[36]+bill.num[38])/2
bill.num

Time.Stamp=seq(1,nrow(a),1)
e.data=cbind(Time.Stamp,bill.num,a)

e.data

Bill=e.data[,2]
Cons=e.data[,15]
Temp=e.data[,7]
H.Days=e.data[,8]
C.Days=e.data[,9]
Members=e.data[,10]

DATA=ts(cbind(Bill,Cons,Temp,H.Days,C.Days,Members),start=c(1991,1),frequency=12)
DATA

autoplot(DATA,facets=T)+geom_smooth()+
ggtitle("Comparison among electricity bill related time series")