library(data.table)
library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)
library(hrbrthemes)
bike = read.csv("~/Desktop/NYC BikeShare/NYC-BikeShare-2015-2017.csv")
setDT(bike)
str(bike)
head(bike)

# Check for null values
#grep('NA',bike)
#convert date time column to date
bike[,Start.Time:=ymd(bike$Start.Time)]
bike[,Stop.Time:=ymd(bike$Stop.Time)]
str(bike)
head(bike)

## Average number of city bike trips per day 
perday= count(bike, vars = 'Start.Time')
head(perday)
sum(perday[,2])
tail(perday)
noofdays <- data.frame(date=c("2017/03/31"),tx_start=c("2015/09/21"))
noofdays$date_diff <- as.Date(as.character(noofdays$date), format="%Y/%m/%d")-
  as.Date(as.character(noofdays$tx_start), format="%Y/%m/%d")
noofdays
setDT(noofdays)
noofdays[,date_diff:=as.numeric(noofdays$date_diff)]
avgtripsperday= sum(perday[,2])/noofdays[,3]
avgtripsperday #  1320.47 


## Average trip Duration per day

totaltripduration= sum(bike[,17])
totaltripduration

avgtripdurationperday= totaltripduration/noofdays[,3]
avgtripdurationperday # 20560.61 minutes

# Quarterly trips 
Q4_15 = sum(bike[c(1:84743),17])/84743
Q4_15

Q1_16 = sum(bike[c(84744:113983),17])/29240
Q1_16

Q2_16 = sum(bike[c(113984:173760),17])/59777
Q2_16

Q3_16 = sum(bike[c(173761:265770),17])/92010
Q3_16

Q4_16 = sum(bike[c(265771:332327),17])/66557
Q4_16

DATE1 <- as.Date("2015-10-01")
DATE2 <- as.Date("2015-12-31")
dates <- seq(DATE1, DATE2, by="days")

ss= subset(bike, Start.Time %in% dates,c(3,17))
Q4_15 = sum(ss[,2])/130958
#Q4_15=bike[Start.Time>=as.date("2015-12-31"),sum(bike[,Trip_Duration_in_min])]/137626
Q4_15 # 19.71797 mins


DATE3 <- as.Date("2016-01-01")
DATE4 <- as.Date("2016-03-31")
dates1 <- seq(DATE3, DATE4, by="days")

ss1= subset(bike, Start.Time %in% dates1,c(3,17))
Q1_16 = ss1[,2]
str(Q1_16)
Q1_16 = sum(ss1[,2])/58480
Q1_16 # 25.97835 mins



DATE5 <- as.Date("2016-04-01")
DATE6 <- as.Date("2016-06-30")
dates2 <- seq(DATE5, DATE6, by="days")

ss2= subset(bike, Start.Time %in% dates2,c(3,17))
Q2_16 = ss2[,2]
str(Q2_16)
Q2_16 = sum(ss2[,2])/119554
Q2_16 # 14.29516 mins



DATE7 <- as.Date("2016-07-01")
DATE8 <- as.Date("2016-09-30")
dates3 <- seq(DATE7, DATE8, by="days")

ss3= subset(bike, Start.Time %in% dates3,c(3,17))
Q3_16 = ss3[,2]
str(Q3_16)
Q3_16 = sum(ss3[,2])/184020
Q3_16 # 13.71941 mins



DATE9 <- as.Date("2016-10-01")
DATE10 <- as.Date("2016-12-31")
dates4 <- seq(DATE9, DATE10, by="days")

ss4= subset(bike, Start.Time %in% dates4,c(3,17))
Q4_16 = ss4[,2]
str(Q4_16)
Q4_16 = sum(ss4[,2])/133114 
Q4_16 # 11.68499 mins


DATE11 <- as.Date("2017-01-01")
DATE12 <- as.Date("2017-03-31")
dates5 <- seq(DATE11, DATE12, by="days")

ss5= subset(bike, Start.Time %in% dates5,c(3,17))
Q1_17 = ss5[,2]
str(Q1_17)
Q1_17 = sum(ss5[,2])/102708 
Q1_17 #  10.53826 mins



xaxis=c('Q4-2015','Q1-2016','Q2-2016','Q3-2016','Q4-2016','Q1-2017')
yaxis= c(Q4_15,Q1_16,Q2_16,Q3_16,Q4_16,Q1_17)
linechart=data.frame(xaxis,yaxis)

ggplot(linechart, aes(x=xaxis, y=yaxis,group=1)) +
  geom_line() +
  labs(x = "Quarters", y = "Average trips", 
       title = "Average trips per quarter")+ 
  scale_x_discrete(limits = c('Q4-2015','Q1-2016','Q2-2016','Q3-2016','Q4-2016','Q1-2017'))


# Determining speed of the trips

library(geosphere)
# calculate distace and convert it to miles
bike$distance<-distHaversine(bike[,7:8], bike[,11:12])/1609.344
head(bike)
str(bike)

bike$trip_duration_in_hour=bike$Trip_Duration_in_min/60

bike$speed <- bike$distance/bike$trip_duration_in_hour #miles/hr

## Age group 100 trips, which age group rode the most

bike$age= 2020-bike$Birth.Year
agegroup = count(bike, vars = 'age')
agegroup = agegroup[c(1:61),]
setDT(agegroup)
class(agegroup)

ggplot(agegroup, aes(x=age, y=freq))+ geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7),width=0.2,position = 'dodge') +geom_text(aes(label= freq))

## Membership

membership = count(bike, vars = 'User.Type')
membership

# 6.5% of total are customers and 93.5% are subscribers and there is potential 
# to convert them into subscribers

#Cluster Analysis

# install.packages("cluster", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cluster)

# Standardizing the data with scale()
stdBike <- bike[,7:8]
# Creating a (Euclidean) distance matrix of the standardized data
distBike <- dist(stdBike, method="euclidean")
# Invoking hclust command (cluster analysis by single linkage method)
clusBike <- hclust(distBike, method = "single")
          
#Plotting

# Create extra margin room in the dendrogram, on the bottom (Station Name)
par(mar=c(8, 4, 4, 2) + 0.1)
# Object "clusBike" is converted into a object of class "dendrogram"
# in order to allow better flexibility in the (vertical) dendrogram plotting.
plot(as.dendrogram(clusBike),ylab="Distance between Stations",ylim=c(0,6),
     main="Dendrogram, CitiBike Stations in NYC")

# K-means, k=2, 3, 4, 5, 6
# Check how many clusters can be formed in the data
(kmeans2.bike <- kmeans(stdBike,2,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.bike$betweenss/kmeans2.bike$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3.bike <- kmeans(stdBike,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.bike$betweenss/kmeans3.bike$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for. Four clusters
(kmeans4.bike <- kmeans(stdBike,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.bike$betweenss/kmeans4.bike$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Computing the percentage of variation accounted for. Five clusters
(kmeans5.bike <- kmeans(stdBike,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.bike$betweenss/kmeans5.bike$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5

# We saw that variation was decreasing after each cluster, we found that 2 clusters would 
# be the most feasible 

set.seed(123)
(kmeans2bike <- kmeans(stdBike,2,nstart = 10))
kmeans2bike
kmeans2bike$cluster

bike[,clus2:=kmeans2bike$cluster]
bike[,clus2:=factor(clus2)]
ggplot(bike,aes(x=Longitude,y=Latitude,color=clus2)) + geom_point()


kmeans2bike$centers
class(kmeans2bike$centers)
kmeans2bike$centers[1,1]
centdt=data.table(kmeans2bike$centers)
centdt

ggplot(bike,aes(x=Longitude,y=Latitude,color=clus2)) + geom_point() + 
  geom_point(data=centdt,aes(x=Longitude, y=Latitude), colour="purple", shape=11,size=2) 



