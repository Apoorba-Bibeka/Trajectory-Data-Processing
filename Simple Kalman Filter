#Script for Implementing a simplified Kalman Filter
#File Name : Kalman_imp_ap.R
#Created by: Apoorba Bibeka
##Creation date: 15 July 2016
#Purpose:To implement a simple Kalman Filter
#Last executed:
Sys.time()


#1 Housekeeping
ls()
rm(list=ls())
ls()

#1 Read the input files 
#*****************************************************************************************************************#
setwd("C:\\Users\\a-bibeka\\Dropbox\\Signal_control\\Socket_Logs")
file1="point_file.csv"
data1<-read.table(file1,sep=",",header=T,skipNul =TRUE)

library(data.table)
#fread create data table
options(datatable.fread.datatable=TRUE)
data1<-data.table(data1)

#*******************************************************************************************************#
#******Extract Date and time in seconds from the time stamp (Date"T"HourMinSec**************************#
#*******************************************************************************************************#
library("plyr")
#ldply converts the list obtained from strsplit into a data frame
#Split Time stamp into date and time 
data1[,time:=as.character(data1[,time])]
data1[,t:=ldply(strsplit(time,"T"))[,2]]
data1[,date:=ldply(strsplit(time,"T"))[,1]]
data1[,t:=as.numeric(t)]

#Extract Hour Min and Seconds from the time 
hr<-substr(data1[,t],1,2)
min<-substr(data1[,t],3,4)
sec<-substr(data1[,t],5,10)
hr<-as.numeric(hr)
min<-as.numeric(min)
sec<-as.numeric(sec)

#Calculate time in seconds from Hrs min and seconds
z<-data.frame(cbind(hr,min,sec))
data1$t1<-with(z,hr*3600+min*60+sec)
time_diff<-diff(data1[,t1])
time_diff<-data.frame(time_diff)
time_diff<-rbind(0,time_diff)
data1[,time_d:=time_diff]
data1<-data.frame(data1)

#*******************************************************************************************************#
#******Ec**************************#
#*******************************************************************************************************#
a=matrix(data=c(1,0,0,1),nrow=2,ncol=2)
q=matrix(data=c(0.23,0,0,0.026),nrow=2,ncol=2)
h=matrix(data=c(1,0,0,1),nrow=2,ncol=2)
r=matrix(data=c(1.51,0,0,5.58),nrow=2,ncol=2)

p0=matrix(data=c(3.7,0,0,6.7),nrow=2,ncol=2)
#z0_=matrix(data=c(data1[2,"loc_x"],data1[2,"loc_y"]),nrow=2)
#x0=matrix(data=c(data1[1,"loc_x"],data1[1,"loc_y"]),nrow=2,ncol=1)

p=p0
x_kal<-list()
y_kal<-list()
for(i in 1:(nrow(data1)-1)){
  
  x_k_1=matrix(data=c(data1[i,"loc_x"],data1[i,"loc_y"]),nrow=2,ncol=1)
  b=matrix(data=c(data1[(i+1),"time_d"],0,0,data1[(i+1),"time_d"]),nrow=2,ncol=2)
  u_k_1=matrix(data=c((data1[i,"speed"]*cos(data1[i,"heading"])),(data1[i,"speed"]*sin(data1[i,"heading"]))),nrow=2,ncol=1)
  x_k_=a%*%x_k_1+b%*%u_k_1

    x_k=matrix(data=c(data1[(i+1),"loc_x"],data1[(i+1),"loc_y"]),nrow=2,ncol=1)
    z_k=h%*%x_k
    K_k=p%*%solve(p+r)
    
    x_k_hat=x_k_+K_k%*%(z_k-(h%*%x_k_))
    p=a%*%p%*%t(a) +q
    
    x_kal[[i]]=x_k_hat[1]
    y_kal[[i]]=x_k_hat[2]
    

}

data1[,"x_kal"]<-rbind(NA,ldply(x_kal))
data1[,"y_kal"]<-rbind(NA,ldply(y_kal))

data1[,"d_x"]=data1$x_kal-data1$loc_x
data1[,"d_y"]=data1$y_kal-data1$loc_y


data1<-data1[-1]
par(mfrow=c(1,1))
with(data1,plot(loc_x,loc_y))

with(data1,points(x_kal,y_kal,col="red",pch=3))

temp<-data1[,c("vehID","x_kal","y_kal","speed","heading","nu","time")]
