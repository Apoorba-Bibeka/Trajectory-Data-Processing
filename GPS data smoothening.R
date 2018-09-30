# Script for processing GPS data log
#File Name: GPS_data_processing
#Created by: Apoorba Bibeka
#Creation date: 24 June 2016
#Purpose:To get stats about GPS data log for moving veh
#Last executed:
Sys.time()


#1 Housekeeping
ls()
rm(list=ls())
ls()

#*******************************************************************************************************#
#******Reading the file containing VehID, Lat, Long, Heading, Null char, time stamp*********************#
#*******************************************************************************************************#
setwd("C:\\Users\\a-bibeka\\Dropbox\\Signal_control\\Socket_Logs")
#setwd("/Users/Apoorb/Dropbox/Signal_control/Socket_Logs")
#Load data.table package for fread to work
library(data.table)
#fread create data table
options(datatable.fread.datatable=TRUE)
#Vehicle is moving
file1="socket_data_log_20160623T160522.txt"
loc_data<-read.table(file1,sep=",",header=FALSE,skipNul =TRUE)


#file2="socket_data_log_20160623T161202.txt"
#loc_data<-read.table(file2,sep=",",header=FALSE,skipNul =TRUE)

#Stationary Point 
file3="socket_data_log_20160623T161615.txt"
loc_data<-read.table(file3,sep=",",header=FALSE,skipNul =TRUE)


mean(loc_data[,2])
mean(loc_data[,3])

#*******************************************************************************************************#
#****** Change the column names of imported data and remove empty columns ******************************#
#*******************************************************************************************************#
str(loc_data)
loc_data<-data.table(loc_data)
#6th Column is empty, delete it 
loc_data[,V6:=NULL]
colnames(loc_data)<-c("vehID","lat","long","speed","heading","time")
#Change lat long unit to degree
loc_data[,lat:=lat/10e6]
loc_data[,long:=long/10e6]
loc_data$No<-1:nrow(loc_data)
loc_data[,time:=as.character(loc_data[,time])]


#*******************************************************************************************************#
#******Extract Date and time in seconds from the time stamp (Date"T"HourMinSec**************************#
#*******************************************************************************************************#
library("plyr")
#ldply converts the list obtained from strsplit into a data frame
#Split Time stamp into date and time 
loc_data[,t:=ldply(strsplit(time,"T"))[,2]]
loc_data[,date:=ldply(strsplit(time,"T"))[,1]]
loc_data[,t:=as.numeric(t)]

#Extract Hour Min and Seconds from the time 
hr<-substr(loc_data[,t],1,2)
min<-substr(loc_data[,t],3,4)
sec<-substr(loc_data[,t],5,10)
hr<-as.numeric(hr)
min<-as.numeric(min)
sec<-as.numeric(sec)

#Calculate time in seconds from Hrs min and seconds
z<-data.frame(cbind(hr,min,sec))
loc_data$t1<-with(z,hr*3600+min*60+sec)
time_diff<-diff(loc_data[,t1])
time_diff<-data.frame(time_diff)
time_diff<-rbind(0,time_diff)
loc_data[,time_d:=time_diff]


#*******************************************************************************************************#
#******Calculate the rate of change of heading *********************************************************#
#*******************************************************************************************************#
heading_diff<-diff(loc_data[,heading])
heading_diff<-data.frame(heading_diff)
heading_diff<-rbind(0,heading_diff)
heading_diff<-abs(heading_diff)
d=ifelse(heading_diff[,1]>180|heading_diff[,1]<(-180),(360-heading_diff[,1]),heading_diff[,1])
loc_data$heading_rate_ch=0.1*d/(time_diff)

#*******************************************************************************************************#
#******Convert Lat Long to XY **************************************************************************#
#*******************************************************************************************************#

#*******************#
#Using latlong2grid
#*******************#

#install.packages("SpatialEpi")
##library(SpatialEpi)
#first long then lat
#returns x and y 
##ta<-as.matrix(cbind(loc_data[,long],loc_data[,lat]))  
##loc_data$x11<-latlong2grid(ta)[,1]*1000
##loc_data$y11<-latlong2grid(ta)[,2]*1000


#*******************#
#Using RGDAL and SP
#*******************#
#install.packages("sp")
#install.packages("rgdal")
library(sp)
library(rgdal)
#Function to convert Lat Long in WGS84 Coordinate reference system to UTM for zone 14 north (Texas)
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," +north"," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
#Texas falls in UTM 
f<-LongLatToUTM(loc_data[,long],loc_data[,lat],14)

#RGDAL and SP to get X and Y 
loc_data$x11<-f$X
loc_data$y11<-f$Y

#*******************************************************************************************************#
#******Remove 1st Row of loc_data***********************************************************************#
#*******************************************************************************************************#
#first row has 0 time gap for data logging
loc_data<-loc_data[-1]



#*******************************************************************************************************#
#******Stats on Time gap of data logging****************************************************************#
#*******************************************************************************************************#
library(lattice)
histogram(~time_d,data=loc_data,xlab="Difference in Time Gap (seconds)")

apply(time_diff,2,mean)
apply(time_diff,2,quantile,probs=0.99)
apply(time_diff,2,max)
loc_data[time_d>.2]

#*******************************************************************************************************#
#******Draw Plots***************************************************************************************#
#*******************************************************************************************************#

#***************************#
#Plot of XY from LatLong2Grid
#***************************#

par(mfrow=c(1,2))
loc_data1<-loc_data
#install.packages("calibrate")
library(calibrate)
plot(loc_data1[,x11],loc_data1[,y11],xlab="xcoordinate (m)",ylab="ycoordinate (m)",main="X_Y_from_latlong2grid")
textxy(loc_data1[,x11],loc_data1[,y11],loc_data1[,No])

#*******************#
#Plot of XY from RGDAL
#*******************#
plot(f$X,f$Y,xlab="xcoordinate (m)",ylab="ycoordinate (m)",main="X_Y_from_latlong2grid")

#*********************************************#
#plot of xy, heading and time gap side by side 
#*********************************************#
par(mfrow=c(1,3))
#loc_data2<-loc_data[50:800]  
loc_data2<-loc_data[1000:1560]
plot(loc_data2[,x11],loc_data2[,y11],xlab="xcoordinate (Km)",ylab="ycoordinate (km)",main="X_Y_from_latlong2grid")
plot(loc_data2[,heading_rate_ch],loc_data2[,y11],type="l",xlab="Heading rate (deg/0.1 sec)",ylab="ycoordinate (km)",main="Rate of Change of Heading")
plot(loc_data2[,time_d],loc_data2[,y11],type="s",xlab="Dato log time gap (sec)",ylab="ycoordinate (km)",main="Time Gap")


#*******************************************************************************************************#
#******Write the data to a table ***********************************************************************#
#*******************************************************************************************************#
write.table(loc_data1,file="trajectory_1heading_rate.csv",sep=",",row.names = FALSE)


#*******************************************************************************************************#
#******Function for converting XY to Long Lat***********************************************************#
#*******************************************************************************************************#
#Function to convert cartesian UTM coordinates to Long and lat
UTMtoLongLat<-function(x,y){
  xy <- data.frame(X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS(paste("+proj=utm +zone=","14"," +North"," ellps=WGS84",sep=''))  ## for example
  res <- spTransform(xy,CRS("+proj=longlat +datum=WGS84") )
  return(as.data.frame(res))
}
#*******************************************************************************************************#
#******Calculating Rolling Mean of X and Y**************************************************************#
#*******************************************************************************************************#
#install.packages("zoo")
library(zoo)
loc_data2<-loc_data
k=3
a1<-rollmean(loc_data2[,x11],k)
b1<-rollmean(loc_data2[,y11],k)

average_headings<-function(a,b,c){
  li<-c(a,b,c)
  li<-sort(li)
    if(li[1]<=90){
      if(li[3]>=270){
        li[3]=li[3]-360
        
        if(li[2]>=270){
          li[2]=li[2]-360
          }
      }
    }

  avg_head=mean(li)
  avg_head
}

i=1     
a<-data.frame()
while(i<=(nrow(loc_data2)-k+1)){
  u=average_headings(loc_data2[i,heading],loc_data2[i+1,heading],loc_data2[i+2,heading])
  a=rbind(a,u)
  i=i+1
}
colnames(a)<-"a1"
a$a1[a$a1<0]=a$a1[a$a1<0]+360



loc_data2<-loc_data2[k:nrow(loc_data2)]
loc_data2$x12<-a1
loc_data2$y12<-b1
loc_data2$avg_heading<-a$a1
loc_data2$diff=loc_data2$avg_heading-loc_data2$heading

plot(loc_data2[,x12],loc_data2[,y12],xlab="xcoordinate (m)",ylab="ycoordinate (m)",main="X_Y_Moving_Average")

loc_data2<-loc_data2[,.(vehID,heading,avg_heading,diff,speed,x12,y12,time)]
write.table(loc_data2,file="test.txt",sep=",",row.names = FALSE)



la<-UTMtoLongLat(loc_data2[,x12],loc_data2[,y12])
#returns long then lat
loc_data2$lat<-la[,2]*10e6
loc_data2$long<-la[,1]*10e6
loc_data2$nu<-"     "
temp<-loc_data2[,.(vehID,lat,long,speed,heading,nu,time)]
write.table(temp,file="moving_avg_VISSIM.txt",sep=",",row.names = FALSE,quote = FALSE)

temp<-loc_data2[,.(vehID,lat,long,speed,avg_heading,nu,time)]
write.table(temp,file="moving_avg__AVG_heading_VISSIM.txt",sep=",",row.names = FALSE,quote = FALSE)


#*******************************************************************************************************#
#******Getting Distance between two farthest points ****************************************************#
#*******************************************************************************************************#
#get only unique points by lat and long
fix_pt_unique<-unique(loc_data,by=c("lat","long"))

#Using latlong2grid
#first long then lat
#returns x and y 
ma<-as.matrix(cbind(fix_pt_unique[,long],fix_pt_unique[,lat]))  
str(ma)
#fix_pt_unique$x<-latlong2grid(ma)[,1]
#fix_pt_unique$y<-latlong2grid(ma)[,2]


fix_pt_unique[,x:=x11]
fix_pt_unique[,y:=y11]

par(mfrow=c(2,2))
plot(fix_pt_unique[,x],fix_pt_unique[,y],xlab="xcoordinate (Km)",ylab="ycoordinate (km)",main="X_Y_from_latlong2grid")
#Get the convex hull of points 
a<-chull(fix_pt_unique[,x],fix_pt_unique[,y])
reduc<-fix_pt_unique[a]
plot(reduc[,x],reduc[,y],xlab="xcoordinate (Km)",ylab="ycoordinate (km)",main="Convex Hull of X_Y_from_latlong2grid")
reduc[,no:=1:nrow(reduc)]

#Function to Calculate distance 
dis_pts<-function(a,b,c,d){
  dis=sqrt((d-b)^2 +(c-a)^2)
  return(dis)
}

#Getting the largest distance distance 
i=1
temp=0
q=data.frame()

while(i<=nrow(reduc)) {
  j=i+1
  while(j<=nrow(reduc)) {
    t=dis_pts(reduc[i,x],reduc[i,y],reduc[j,x],reduc[j,y])
    q=rbind(q,t)
    
    a_dis=dis_pts(reduc[i,x],reduc[i,y],reduc[j,x],reduc[j,y])
    if(a_dis>temp) temp=a_dis
    j=j+1
  }
  i=i+1
}
largest_dist=temp
largest_dist


#*******************************************************************************************************#
#******Plotting Long Lat on a Map***********************************************************************#
#*******************************************************************************************************#
#Get the location of Test size from Lat Long data 
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("rjson")
library(ggplot2)
library(ggmap)
map_<-get_map(location=c(lon=mean(la$X),lat=mean(la$Y)),zoom=4,maptype="satellite",scale=2)
points(la$X,la$Y,col="red")
ggmap(map_)+geom_point(data=la,aes(x=X,y=Y,fill="red",alpha=0.8),size=5,shape=21)+guides(fill=FALSE, alpha=FALSE, size=FALSE)


dev.off()
