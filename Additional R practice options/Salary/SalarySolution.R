library(ggplot2)

#---Feed the data into R this way. To choose the "path", right click on your txt file,go to ----#
#---"Properties" and copy and paste the "Location" string. Be sure to change one slash (\) to ---#
#---two (\\) manually and add the name of the fle with the .txt extension---#

data=read.table("C:\\Users\\Moinak\\Desktop\\ST 625\\Spring2019\\salaries.txt",header=T)
data


###########################
#---CHAPTER 1 ideas-------#
###########################

#---Rank distribution separated by gender-----#
ggplot(data=data,aes(x=rank,fill=gender))+ 
geom_bar(position='dodge')+
ggtitle("Bar-plot showing rank distribution, separated by gender")

#---Rank distribution separated by gender and discipline-----#
ggplot(data=data,aes(x=rank,fill=gender))+ 
geom_bar(position='dodge')+
facet_grid(. ~ discipline)+
ggtitle("Bar-plot showing rank distribution, separated by gender and discipline")

#---Rank distribution separated by gender and discipline (with numbers attached)-----#
ggplot(data=data,aes(x=rank,fill=gender))+ 
geom_bar(position='dodge')+
geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
facet_grid(. ~ discipline)+
ggtitle("Bar-plot showing rank distribution, separated by gender and discipline")


#---Histograms-------#
ggplot(data=data,aes(x=salary,,y=..density..))+ 
geom_histogram()+ 
geom_line(stat='density')+
ggtitle("Histogram (raw and smoothed) showing salary distribution")

ggplot(data=data,aes(x=salary,y=..density..,fill=gender))+ 
geom_histogram()+ 
geom_line(stat='density')+
ggtitle("Histogram (raw and smoothed) showing salary distribution, separated by gender")


ggplot(data=data,aes(x=salary,y=..density..,fill=gender))+ 
geom_histogram()+ 
geom_line(stat='density')+
facet_grid(. ~ discipline)+
ggtitle("Histogram (raw and smoothed) showing salary distribution, separated by gender and discipline")


ggplot(data=data,aes(x=salary,fill=gender))+ 
 geom_density(alpha=0.4)+ 
 facet_grid(. ~ discipline)+
ggtitle("Density curves showing salary distribution, separated by gender and discipline")


#---Boxplots----#

ggplot(data=data,aes(x=gender,y=salary,fill=gender))+ 
 geom_boxplot(notch=FALSE)+ 
 facet_grid(. ~ discipline)+
ggtitle("Boxplots showing salary distribution, separated by gender and discipline")

ggplot(data=data,aes(x=gender,y=salary,fill=gender))+ 
 geom_boxplot(notch=TRUE)+ 
 facet_grid(. ~ discipline)+
ggtitle("Notched boxplots showing salary distribution, separated by gender and discipline")


#---Violinplots----#
ggplot(data=data,aes(x=gender,y=salary,fill=gender))+ 
 geom_violin()+ 
 geom_boxplot(fill='darkred',width=0.1,notch=TRUE)+ 
 geom_point(position='jitter',size=1)+ 
 facet_grid(. ~ discipline)+
ggtitle("Notched violinplots showing salary distribution, separated by gender and discipline")