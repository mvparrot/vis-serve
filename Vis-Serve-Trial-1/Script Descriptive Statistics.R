#================================================
#--- Descriptive Statistics
#--- Alwin Wang
#================================================

library(ggplot2)

#--- Run the script to load the data
source("Helper Load Data.R")


# Start Position, top down view
### outlier <- atp_serves[atp_serves$start.x <=8,]
### 3_13_05_1_212924.trj IS AN OUTLIER, TOO SMALL
qplot(start.x, start.y, data=atp_serves[atp_serves$start.x >= 8,], color=side, 
      xlab="x", ylab="y") 

# Start Position, view from behind
### outlier <- atp_serves[atp_serves$start.z <=1.8,]
### 3_13_05_1_212924.trj IS AN OUTLIER, TOO SMALL
### 4_06_01_1_181311.trj IS AN OUTLIER, TOO SMALL
### Some are too high?
### Some deuce court serves too left??
qplot(start.y, start.z, data=atp_serves[atp_serves$start.z > 2.5,], color=side, 
      xlab="y", ylab="z") 

# Start Position, view from the side
### outlier <- atp_serves[atp_serves$start.z <=1.8,]
### all outliers seem to be found
qplot(start.x, start.z, data=atp_serves[atp_serves$start.z > 2.5 & atp_serves$start.x >= 8,], color=side, 
      xlab="x", ylab="z")

# Histogram of service speeds
### There is one quite low, 
### 4_06_01_1_181311.trj
hist(atp_serves$speed[atp_serves$speed >= 25]*3.6)

# Bounce Position, top down view
### outlier <- atp_serves[atp_serves$center.x <= -6.6,]
### "4_06_01_1_181311.trj" "2_05_04_1_162347.trj" "3_03_04_2_211949.trj"
qplot(center.x, center.y, data=atp_serves[atp_serves$center.x >= -6.6,], colour=side,
      xlab="x", ylab="y") 

# Net Clearnace
### No clear outliers
hist(atp_serves$net.clearance)

# Arc durations
### outlier <- atp_serves[atp_serves$duration.arc1 >=0.8,]
### 4_06_01_1_181311.trj IS AN OUTLIER
### There are several other ones that have a arc3 time too low?
qplot(duration.arc1, duration.arc3, data=atp_serves[atp_serves$duration.arc1 <=0.8,], color=serve.x,
      xlab="arc1", ylab="arc3") 


### NO CLEAR RELATION?
qplot(x0.1, start.x, data=atp_serves[atp_serves$start.x >= 8,], color=side, 
      xlab="x0.1", ylab="start.x") 

### NO CLEAR RELATION EITHER?
qplot(x0.1, y0.1, data=atp_serves[atp_serves$start.x >= 8,], color=side, 
      xlab="x", ylab="y") 
