# Script for processing GPS data log
#File Name: GPS_data_processing_new_format
#Created by: Apoorba Bibeka
#Creation date: 8 July 2016
#Purpose:To get stats about GPS data log for moving veh
#Last executed:
Sys.time()


#1 Housekeeping
ls()
rm(list=ls())
ls()

#2 File Reading
#*******************************************************************************************************#
#******Reading the file containing VehID, Lat, Long, Heading, Null char, time stamp*********************#
#*******************************************************************************************************#
setwd("C:\\Users\\a-bibeka\\Dropbox\\Signal_control\\Socket_Logs")
file4="socket_data_log_2nd_run_20-30_MPH.txt"
loc_data<-read.table(file4,sep=",",header=FALSE,skipNul =TRUE)
#setwd("/Users/Apoorb/Dropbox/Signal_control/Socket_Logs")
#Load data.table package for fread to work
library(data.table)

#Washington Runs
setwd("C:\\Users\\a-bibeka\\Dropbox\\TTI_Projects\\DS_baa_project_Signal_control_and_Socket\\Socket_Folder\\Socket_Logs")
file4="socket_data_log_20161107T135149.txt"


setwd("C:\\Users\\a-bibeka\\Dropbox\\Signal_control\\Socket_Logs\\test_runs_07_13_2016")
file4="socket_data_log_20160713T170429_10mph.txt"
file4="socket_data_log_20160713T170825_20mph.txt"
file4="socket_data_log_20160713T171102_30mph.txt"
file4="socket_data_log_20160713T171318_40mph.txt"
file4="socket_data_log_20160713T171520_50mph.txt"

file4="socket_data_log_20160713T173120_signal_20mph.txt"
file4="socket_data_log_20160713T173349_signal_30.txt"

file4="socket_data_log_20160713T173600_signal_40.txt"
file4="socket_data_log_20160713T173752_signal_50.txt"


loc_data<-read.table(file4,sep=",",header=FALSE,skipNul =TRUE)

#3 Rename Columns 
#New file format 
#*******************************************************************************************************#
#****** Change the column names of imported data and remove empty columns ******************************#
#*******************************************************************************************************#
str(loc_data)
loc_data<-data.table(loc_data)
#8th Column is empty, delete it 
loc_data[,V8:=NULL]
colnames(loc_data)<-c("vehID","lat","long","speed","heading","Msg_Id","ivp_time","time")
#Change lat long unit to degree
loc_data[,lat:=lat/10e6]  
loc_data[,long:=long/10e6]
loc_data$No<-1:nrow(loc_data)
loc_data[,time:=as.character(loc_data[,time])]



#Extract Time from Vissim time stamp
#*******************************************************************************************************#
#******Extract Date and time in seconds from the time stamp (Date"T"HourMinSec**************************#
#*******************************************************************************************************#
require("plyr")
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
#time diff for vissim data logging
loc_data[,vissim_dt:=time_diff]

#Extract time from ivp
#*******************************************************************************************************#
#******Extract Date and time in seconds from the ivp_time hour:min:sec:milliseconds*********************#
#*******************************************************************************************************#
#Split the ivp_time
#Get a data frame with hr min sec and milliseconds 
loc_data[,ivp_time:=as.character(ivp_time)]
buffer=ldply(strsplit(loc_data[,ivp_time],":"))
#Get the time in seconds 
temp=(as.numeric(buffer[,1])-1)*3600+as.numeric(buffer[,2])*60+as.numeric(buffer[,3])+as.numeric(buffer[,4])/1000
loc_data$ivp_t=temp

a<-diff(temp)
#time diff for ivp data logging
loc_data$ivp_dt<-rbind(0,data.frame(a))

#Correct the data- ivp data has an error of 1 seconds in certain places
loc_data[ivp_dt<0,ivp_t:=ivp_t+1]
loc_data[,ivp_dt:=c(0,diff(ivp_t))]


############
with(loc_data[-1,],plot(ivp_dt,vissim_dt))
#packet drop
P<-diff(loc_data[,Msg_Id])
summary(P)

############

#Get a subset of data with t1 adn ivp_t
buffer<-loc_data[,.(t1,ivp_t)]
#Calculate the diff between VISSIM time stamp and ivp time stamp 
buffer[,diff:=t1-ivp_t]
min1=with(buffer,min(diff))

#Offset all the difference values based on the min value
buffer[,diff1:=diff-min1]

#Summary Statistics 
with(buffer,plot(t1,diff1))
with(buffer,hist(diff1))
with(buffer,summary(diff1))

#diff in vissim and ivp time 
loc_data$ivp_vissim_dt<-buffer[,diff1]

loc_data<-loc_data[-1,]
csum_ivp_dt<-cumsum(loc_data[,ivp_dt])
csum_vissim_dt<-cumsum(loc_data[,vissim_dt])

par(mfrow=c(2,1))
plot(loc_data$ivp_t,csum_ivp_dt,type="s")
plot(loc_data$ivp_t,csum_vissim_dt,type="s")

di=csum_vissim_dt-csum_ivp_dt
#*******************************************************************************************************#
#******Remove unused data ******************************************************************************#
#*******************************************************************************************************#
setnames(loc_data,'t1',"vissim_t")
loc_data1<-loc_data[,.(vehID,lat,long,heading,ivp_t,vissim_t,ivp_vissim_dt,ivp_dt,vissim_dt,No)]
rm(list=setdiff(ls(),c("loc_data1","loc_data")))

write.csv(loc_data1,file="d.csv")
#*******************************************************************************************************#
#******Function to Convert Lat Long to VISSIM XY *******************************************************#
#*******************************************************************************************************#
          
lat_long_xy_vissim<-function(lat,long){
              #Radius of earth in m
              R = 6371000
              #VISSIM reference x and y 
              vissim_x=-3.3
              vissim_y=-0.5
              #Global lat and long for VISSIM reference x and y 
              gb_lat=38.954990*pi/180
              gb_lon=-77.149381*pi/180
              #Convert lat long to x and y . Simple offset addition 
              offset_y=R*((lat*pi/180)-gb_lat)
              offset_x=(R*cos(gb_lat)*((long*pi/180)-gb_lon))
              loc_y=vissim_y+offset_y
              loc_x=vissim_x+offset_x
              #plot
              
              return(cbind(loc_x,loc_y))
              
}            
#*******************************************************************************************************#
#****** Convert Lat Long to VISSIM XY ******************************************************************#
#*******************************************************************************************************#
par(mfrow=c(1,1))
#Get data x and y 
loc_data$x<-lat_long_xy_vissim(loc_data$lat,loc_data$long)[,1]
loc_data$y<-lat_long_xy_vissim(loc_data$lat,loc_data$long)[,2]

#Get Rsu x and y 
rsu_lat=30.637864
rsu_long=-96.478332
rsu_x<-lat_long_xy_vissim(rsu_lat,rsu_long)[,1]
rsu_y<-lat_long_xy_vissim(rsu_lat,rsu_long)[,2]


with(loc_data,plot(x,y))

#*******************************************************************************************************#
#****** Function to Calculate distance *****************************************************************#
#*******************************************************************************************************#
dis_pts<-function(a,b,c,d){
  dis=sqrt((d-b)^2 +(c-a)^2)
  return(dis)
}


#*******************************************************************************************************#
#****** Calculate distance from rsu ********************************************************************#
#*******************************************************************************************************#
loc_data1$dis_rsu<-dis_pts(loc_data1$x,loc_data1$y,rsu_x,rsu_y)
pdf("summary_stat_ds_baa.pdf")
#Plot vissim lag, ivp lag and vissim and ivp lag 
par(mfrow=c(3,1))
with(loc_data1,plot(dis_rsu,vissim_dt,type="p",xlab="Distance from RSU (meters)",ylab="Time Lag in VISSIM Timestamp"))
with(loc_data1,plot(dis_rsu,ivp_dt,type="p",xlab="Distance from RSU (meters)",ylab="Time Lag in ivp Timestamp"))
with(loc_data1,plot(dis_rsu,ivp_vissim_dt,type="p",xlab="Distance from RSU (meters)",ylab="Time Lag in VISSIM Timestam and ivp Timestamp"))




a=min(loc_data[1,ivp_t])
b=max(loc_data[,ivp_t])
bi<-seq(a,b,1)
require(plyr)
bin<-findInterval(loc_data[,ivp_t],bi)
freq_tab<-as.data.table(count(bin))
x=seq(1:length(bi))
freq_tab1<-data.table(cbind(x))
freq_tab<-merge(freq_tab1,freq_tab,by="x",all.x=T)
freq_tab[is.na(freq),freq:=0]
freq_tab[,dt:=c(0,diff(ivp_t))]

freq_tab[,mask:=cumsum(freq)]
freq_tab$ivp_t<-loc_data1[freq_tab$mask,ivp_t]
freq_tab$distance<-loc_data1[freq_tab$mask,round(dis_rsu,digits=2)]
freq_tab$tp<-c(NA,freq_tab[-nrow(freq_tab),round(distance,digits=2)])
freq_tab[,lab:=paste(tp,distance,sep=" - ")]


with(freq_tab[freq!=0],plot(distance,freq))
axis(1,at=freq_tab$distance,labels=freq_tab$lab,las=2)
graphics.off()
#*******************************************************************************************************#
#******Stats on Time gap of data logging****************************************************************#
#*******************************************************************************************************#
library(lattice)
a<-histogram(~vissim_dt,data=loc_data,xlab="Difference in VISSIM Time Gap (seconds)")
b<-histogram(~ivp_dt,data=loc_data1,xlab="Difference in IVP Time Gap (seconds)")
c<-histogram(~ivp_vissim_dt,data=loc_data,xlab="Difference in VISSIM and IVP Time Gap (seconds)")
# Plot prints
print(a, split = c(1, 1, 2, 2), more = TRUE)
print(b, split = c(2, 1, 2, 2), more = TRUE)
print(c, split = c(1, 2, 2, 2), more = FALSE)


#Plot vissim lag, ivp lag and vissim and ivp lag 
par(mfrow=c(3,1))
with(loc_data1,plot(ivp_t,vissim_dt,type="s",xlab="IVP Time in Seconds",ylab="Time Lag in VISSIM Timestamp"))
with(loc_data1,plot(ivp_t,ivp_dt,type="s",xlab="IVP Time in Seconds",ylab="Time Lag in ivp Timestamp"))
with(loc_data1,plot(ivp_t,ivp_vissim_dt,type="s",xlab="IVP Time in Seconds",ylab="Time Lag in VISSIM Timestam and ivp Timestamp"))
graphics.off()

#Summary_statistics
with(loc_data,summary(vissim_dt))
with(loc_data1,summary(ivp_dt))
with(loc_data1,summary(ivp_vissim_dt))



with(loc_data,plot(t1,vissim_dt,type="s",xlab="VISSIM Time in Seconds",ylab="Time Lag in VISSIM Timestamp"))
