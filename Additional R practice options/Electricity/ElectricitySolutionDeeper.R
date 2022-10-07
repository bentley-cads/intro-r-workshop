electricity
a=electricity

bill=a[,4]
bill

#---Converting to numeric values---#
bill.num=as.numeric(levels(bill))[bill]
bill.num

#---Constructing a time series object---#

dirty.data=ts(bill.num,start=c(1991,1),frequency = 12)
dirty.data

#---Detecting outliers---#
tsoutliers(dirty.data,lambda="auto")

#---Locating the troublemakers on a data-frame---#
Time.Stamp=seq(1,nrow(a),1)
e.data=cbind(Time.Stamp,bill.num)
e.data

#---Cleaning the data---#
clean.data=tsclean(dirty.data, replace.missing = TRUE,lambda="auto")
clean.data

#---Comparing them side by side on a data-frame---#
cbind(Time.Stamp,dirty.data,clean.data)

#---Comparing them on the same graph---#
autoplot(ts(cbind(dirty.data,clean.data),start=c(1991,1),frequency = 12))+
  ylab("Dirty and clean electricity bill time series")+ggtitle("Graph demonstrating data cleaning")