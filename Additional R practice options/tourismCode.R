

#--Quarterly data on tourists visiting different regions of Australia for different purposes--#

#--Importing our data--#
data=read.table("C:\\Users\\Moinak\\Desktop\\MA611, Fall2019\\Data Sets\\Chapter 1\\tourism.txt",header=T) #--that's one way, specifying the exact path--#
head(data) #--the first few lines--#
tail(data) #--the last few lines--#

#---Another option: through "Import Dataset", top-right--#
data=tourism #--notice the "Import" button offers various options: txts, Excel spreadsheets, etc.--#
str(data) #--"str" stands for the structure of an R object---#
dim(data) #--"helps us check the size of our data--#

#--Finding our way around the file---#
names(data) #--What are the column names?---#

unique(data$Year) #--what are the years we are covering? "unique" avoids repetitions--#
length(unique(data$Year)) #--how many years are we covering? "length" calculates the size of a vector--#

unique(data$Month)
length(unique(data$Month)) #--so those are quarters, actually--#

unique(data$Region)
length(unique(data$Region))

unique(data$State)
length(unique(data$State))

unique(data$Purpose)
length(unique(data$Purpose))

 #--20 years' worth of quarterly data on 76 regions on 4 purposes--#
 #--20x4x76x4 = 24320, so all the rows are there. Check your dim() result!!--#

 #--so all the rows are accounted for, but do we have missing trip values in certain rows?---#
is.na(data$Trips) #--a logic that spits FALSE if the condition is not satisfied--#
any(is.na(data$Trips)) #--so they are all present--#
                      #--please check "tourismCorrupted" or the penguins data or the electricity bill one to deal with missing observations--#
which(is.na(data$Trips)) #--locating the troublemakers--#
data[which(is.na(data$Trips)),] #--highlighting those problematic rows--#

#--Note how, if we import the Excel file instead, it loads as a "tibble" object--#
#--We can always convert it to a data frame using as.data.frame()---#

#--Let's work out the median year and the median number of trips--#
median(data$Year)
median(data$Trips)

#--some quicker ways--#
lapply(data.frame(data$Year,data$Trips),median) #--the "l" stands for a list--#
              #--we can "unlist" this object through unlist()--#
sapply(data.frame(data$Year,data$Trips),median) #--that's a vector, otherwise, similar--#


#--Let's find the mean and standard deviation for "Trips" under each "Purpose" category--#
#--library(tidyverse)--#

by.purpose=data %>% group_by(Purpose)
by.purpose
by.purpose %>% summarise(
  av.trips=mean(Trips),
  sd.trips=sd(Trips)
)

lapply(data$Trips,data$Region,mean)
#--Further confirmation--#

bus.data=subset(data,Purpose=="Business")
mean(bus.data$Trips)
sd(bus.data$Trips)

#---Let's now be more demanding: be conscious of the region too--#
by.purpose.region=data %>% group_by(Purpose,Region)
by.purpose.region
by.purpose.region %>% summarise(
  av.trips=mean(Trips),
  sd.trips=sd(Trips)
)

#--Confirmation---#
#--Let's extract certain pieces out of this large data set---#
#----Adelaide----#
adelaide.bus.extract=subset(data,Region=="Adelaide" & Purpose=="Business")
mean(adelaide.bus.extract$Trips)
sd(adelaide.bus.extract$Trips)
adelaide.hol.extract=subset(data,Region=="Adelaide" & Purpose=="Holiday")
adelaide.oth.extract=subset(data,Region=="Adelaide" & Purpose=="Other")
adelaide.vis.extract=subset(data,Region=="Adelaide" & Purpose=="Visiting")

#---Sydney-----#
sydney.bus.extract=subset(data,Region=="Sydney" & Purpose=="Business")
sydney.hol.extract=subset(data,Region=="Sydney" & Purpose=="Holiday")
sydney.oth.extract=subset(data,Region=="Sydney" & Purpose=="Other")
sydney.vis.extract=subset(data,Region=="Sydney" & Purpose=="Visiting")

#---Melbourne---#
melbourne.bus.extract=subset(data,Region=="Melbourne" & Purpose=="Business")
melbourne.hol.extract=subset(data,Region=="Melbourne" & Purpose=="Holiday")
melbourne.oth.extract=subset(data,Region=="Melbourne" & Purpose=="Other")
melbourne.vis.extract=subset(data,Region=="Melbourne" & Purpose=="Visiting")

#---Brisbane---#
brisbane.bus.extract=subset(data,Region=="Brisbane" & Purpose=="Business")
brisbane.hol.extract=subset(data,Region=="Brisbane" & Purpose=="Holiday")
brisbane.oth.extract=subset(data,Region=="Brisbane" & Purpose=="Other")
brisbane.vis.extract=subset(data,Region=="Brisbane" & Purpose=="Visiting")

#######################
#######################

adelaide.bus=adelaide.bus.extract[,6] #--if we want to count the columns--#
#--another way: adelaide.bus.extract$Trips# 
adelaide.hol=adelaide.hol.extract[,6]
adelaide.oth=adelaide.oth.extract[,6]
adelaide.vis=adelaide.vis.extract[,6]

sydney.bus=sydney.bus.extract[,6]
sydney.hol=sydney.hol.extract[,6]
sydney.oth=sydney.oth.extract[,6]
sydney.vis=sydney.vis.extract[,6]

melbourne.bus=melbourne.bus.extract[,6]
melbourne.hol=melbourne.hol.extract[,6]
melbourne.oth=melbourne.oth.extract[,6]
melbourne.vis=melbourne.vis.extract[,6]

brisbane.bus=brisbane.bus.extract[,6]
brisbane.hol=brisbane.hol.extract[,6]
brisbane.oth=brisbane.oth.extract[,6]
brisbane.vis=brisbane.vis.extract[,6]

#--Common operations on vectors---#
summary(adelaide.bus.extract$Trips)
boxplot(adelaide.bus.extract$Trips)

#---Let's plot the Adelaide-Business numbers over time---#

Time.Range=seq(1,length(adelaide.bus.extract$Trips),1) #--storing the quarter indices--#
plot(Time.Range,adelaide.bus.extract$Trips,"l", #the "l" stands for a linear way of joining them, change it to "p" to see what happens--#
     main="Time series plot showing the number of people visiting Adelaide on a business trip",
     xlab="Time (in quarters)", ylab="Scaled number of business travelers")
#--slightly more complex graphics--#
#--let's see which region attracts more business travelers--#

#---the concatenation "c" that you saw may be used to merge data as well--#
Business.data=c(adelaide.bus.extract$Trips,sydney.bus.extract$Trips,brisbane.bus.extract$Trips,melbourne.bus.extract$Trips)
Business.data #--a larger vector where the 4 "Business" pieces are placed side-by-side--#
length(Business.data) #--always a great idea to confirm the size!! We are expecting 80x4=320--#

#--Let's now BUILD a data frame instead of importing one--#
names=c(rep("Adelaide",80),rep("Sydney",80),rep("Brisbane",80),rep("Melbourne",80)) #--"rep" stands for "repeat"--#
Business.df<-data.frame(names,Business.data)
Business.df
colnames(Business.df)<-c("City","Trips") #--we can modify the column headings, if we want--#

write.table(Business.df,"OurBusinessData")

#--library(ggplot2)--#
ggplot(data=Business.df,aes(x=City,y=Trips,fill=City))+ 
  geom_violin()+ #--this geometry may vary depending on what we want--#
  geom_boxplot(fill='darkred',width=0.1,notch=TRUE)+ 
  geom_point(position='jitter',size=1)+ 
#  facet_grid(.~)+ #--just in case we had further subcategories--#
  ggtitle("Notched violinplots showing business trip distribution, separated by cities")

#---Let's delve deeper into city-purpose combinations---#
adelaide.extract=subset(data,Region=="Adelaide")
brisbane.extract=subset(data,Region=="Brisbane")
melbourne.extract=subset(data,Region=="Melbourne")
sydney.extract=subset(data,Region=="Sydney")

Region.Data=rbind(adelaide.extract,brisbane.extract,melbourne.extract,sydney.extract) #rbind stands for row-binding: placing the rows on top of each other--#

ggplot(data=Region.Data,aes(x=Region,y=Trips,fill=Region))+ 
  geom_violin()+ #--this geometry may vary depending on what we want--#
  geom_boxplot(fill='darkred',width=0.1,notch=TRUE)+ 
  geom_point(position='jitter',size=1)+ 
    facet_grid(.~Purpose)+ #--just in case we had further subcategories--#
  ggtitle("Notched violinplots showing business trip distribution, separated by cities and purpose")