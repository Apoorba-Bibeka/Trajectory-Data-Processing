# Script for Reading VISSIM Link Data and snapping points from trajectory file to links based on headings 
#File Name: Snap_to_link_heading
#Created by: Apoorba Bibeka
#Creation date: 12 July 2016
#Purpose:To get link data from VISSIM and snap trajectory points to it. Also, use heading for snapping
#Last executed:
Sys.time()


#1 Housekeeping
ls()
rm(list=ls())
ls()




#******
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Function to snap GPS points to vISSIM link using headings 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

apoorb_snap_to_line<-function(heading_fn_dat,link_data,point1,point1_heading){
  
  #Testers 
  # heading_fn_dat<-heading_dat
  # link_data<-lines_data
  # point1<-cbind(-120,3)
  # point1_heading<-250
  
  #Get a list of link ID's which have heading in +- 90 deg range of the points heading 
  #X2 is heading of link, X1 is line/link ID and hd is buffer to get heading diff
  
  heading_fn_dat$X2<- as.numeric(as.character(heading_fn_dat$X2))
  heading_fn_dat$X1<-as.character(heading_fn_dat$X1)
  heading_fn_dat$hd=heading_fn_dat$X2-point1_heading
  heading_fn_dat$hd=abs(heading_fn_dat$hd)
  d=ifelse( heading_fn_dat$hd>=180| heading_fn_dat$hd<=(-180),(360-heading_fn_dat$hd),heading_fn_dat$hd)
  heading_fn_dat$hd<-d
  #Mask contains the list of link IDs we are interested in 
  mask<-heading_fn_dat[heading_fn_dat$hd<=60,"X1"]
  
  
  #Define a spatial point using the x and y coordinates from trajectory data 
  Spoint=SpatialPoints(point1)
  require(rgeos)
  #Get the nearest coordinates on the link for all the points from trajectory data 
  snap_pt<-snapPointsToLines(Spoint,link_data[mask,])
  snap_pt<-data.frame(snap_pt)
  
  #add link heading to data frame
  temp<-snap_pt[1,"nearest_line_id"]
  temp<-as.character(temp)
  snap_pt<-cbind(snap_pt,heading_fn_dat[heading_fn_dat$X1==temp,"X2"])
  snap_pt[,1]<-NULL
  snap_pt[,3]<-NULL
  colnames(snap_pt)[3]<-"heading"
  
  #Return the data frame 
  snap_pt
}




#3 Read File
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Reading the file containing VehID, Lat, Long, Heading, Null char, time stamp
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("C:\\Users\\a-bibeka\\Dropbox\\Signal_control\\Socket_Logs")
#setwd("/Users/Apoorb/Dropbox/Signal_control/Socket_Logs")
#Load data.table package for fread to work
library(data.table)
#fread create data table
options(datatable.fread.datatable=TRUE)
require(xml2)
#Read VISSIM inpx file (XML format)
x<-read_xml("riverside.inpx")
#Read points data from trajectory file
file="point_file.csv"
file="point_file1.csv"

points_gps<-fread(file,header = TRUE)




#4 Get link data from VISSIM XML file 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Get link data from riverside.inpx file. links->link->geometry->3Dpoints->3Dpoint->x, y, z
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#get the links node
links<-xml_child(x,"links")
xml_length(links)
final_data<-data.table()
#extract all the link data
i=1
for(i in 1:xml_length(links)){
  #interate through each link 
  link<-xml_child(links,i)
  #get link no
  n<-xml_attr(link,"no")
  #get to point3d ============geometry->points3D->point3D
  geometry<-xml_child(link,"geometry")
  points3d<-xml_child(geometry)
  point3d<-xml_children(points3d)
  #get x y and z from point3d
  a<-xml_attrs(point3d,"x")
  #covert x y and z to a data table 
  dt.tab<-as.data.table(a)
  dt.tab$no<-n
  dt.tab$cd<-c("x","y","z")
  d1<-melt(dt.tab,id.vars=c("no","cd"),value.name="Cord",variable.name = "obs")
  final1<-dcast(d1,no+obs~cd,value.var = "Cord")
  
  final1[,x:=as.numeric(x)]
  #Get dx and dy to calculate heading of the link. First row will not have dx or dy or heading
  d_x=diff(final1[,x])
  final1$d_x=c(NA,d_x)
  
  final1[,y:=as.numeric(y)]
  d_y=diff(final1[,y])
  final1$d_y=c(NA,d_y)
  final_data<-rbindlist(list(final_data,final1))
}

final_data[,heading:=NA]

#Use dx and dy instead of dy and dx to get the heading for VISSIM
final_data[,heading:=atan2(d_x,d_y)*180/pi]
final_data[heading<0,heading:=heading+360]




#4 Convert link coordinates into lines. Create a list of headings 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Use sp and maptool packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#heading_dat contains the line Id and heading value 
# line Id= final_data$no+ final_data$obs
heading_dat<-data.frame(mask=character(0),heading=numeric(0))

  fdat<-final_data
  fdat$N=(1:nrow(fdat))
  #install.packages("rgeos")
  require(sp)
  require(maptools)
  j=1
  line_list<-list()
  ls<-NULL
  # fdat<-fdat[heading<(hd+80)|heading>(hd+80),]
  ls<-unique(fdat[,no])
  #Loop through different links of VISSIM and creae a list of lines for each link
  for(t in ls){
    
    it=1
    while(it<nrow(fdat[no==t,])){
      buf<-fdat[no==t]
      s=Line(buf[c(it,it+1),.(x,y)])
      s1=Lines(list(s),ID=paste(t,buf[(it+1),obs],sep=""))
      
      
      heading_dat<- rbind(heading_dat, data.frame(cbind(paste(t,buf[(it+1),obs],sep=""),buf[(it+1),heading])))
      buf<-NULL
      line_list[[j]]=s1
      j=j+1
      it=it+1
    }
    
  }
  
  #Create a list of spatial lines 
  S1<-SpatialLines(line_list)
  par(mfrow=c(1,1))
  #plot(S1)
  #S1@lines[[1]]@ID
  
  
  #Create a data frame to easily access lines by ID
  lines_data<-(1:nrow(heading_dat))
  lines_data<-data.frame(lines_data)
  row.names(lines_data)<-heading_dat$X1 #X1 is the line ID
  lines_data<- SpatialLinesDataFrame(S1,lines_data)

 #5 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
 #For each point find the links in VISSIM with +- 90 heading and snap to link
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
 
  snap_data<-data.frame()
  for(i in 1:nrow(points_gps)) {
    snap_data<-rbind(snap_data, apoorb_snap_to_line(heading_dat,lines_data,points_gps[i,.(loc_x,loc_y)],points_gps[i,heading]))
    i=i+1
  }
  
  
  with(snap_data,plot(X,Y))
  
  
  #6 Convert VISSIM coordinates to lat long  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #=
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #Radius of earth in mts
  R = 6371000
  #VISSIM coordiantes of a particular point
  vissim_x=-118.943
  vissim_y=-0.645
  #Lat long for the point in VISSIM
  gb_lat=30.637920*pi/180
  gb_lon=-96.478419*pi/180
  #Get the offset 
  offset_x=snap_data$X-vissim_x
  offset_y=snap_data$Y-vissim_y
  #Convert offset to lat long and add to global reference point
  lat_f=(gb_lat+offset_y/R)*180*10e6/pi
  long_f=(gb_lon+offset_x/(R*(cos(gb_lat))))*180*10e6/pi
  
  #Write data to file 
  temp<-cbind(points_gps$vehID,points_gps$speed,points_gps$time,snap_data$heading,lat_f,long_f)
  colnames(temp)<-c("vehID","speed","time","heading","lat_f","long_f")
  temp<-data.table(temp)
  temp[,nu:=NULL]
  temp[,nu:="  "]
  temp<-temp[,.(vehID,lat_f,long_f,speed,heading,nu,time)]
  write.table(temp,file="snap_2_line_v1.csv",quote=FALSE,sep=",",row.names = FALSE)
  
  
