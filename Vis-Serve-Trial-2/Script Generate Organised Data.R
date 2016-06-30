#================================================
#--- Generated Orgasnised Data
#--- Alwin Wang
#================================================

#--- Load the data
source("Helper Load Data.R")

#--- Load Generate Organised Data Functions
source("Helper Generate Organised Data.R")

#--- Load Generate Physical Properties Functions
source("Helper Generate Physical Properties.R")

sample <- GeneratePoints(atp_serves[1:10,],arc3=10,plot=TRUE)

sample <- physics(sample)

# library("ggplot2")
# ggplot(sample,aes(x=pos.x,y=vel.x*3.6))
# qplot(pos.x,vel,data=sample)
# 
# library("rgl")
# plot3d(sample$pos.x,sample$pos.y,sample$vel, type='s', size = 2, #aspect="iso",
#        xlim=c(-12,12), ylim=c(-6,6))
# plot3d(sample$pos.x,sample$pos.y,sample$vel, type='h', add=FALSE)

source("Helper Spin Functions.R")

test <- sample[sample$serveid=="1_01_03_1_194206.trj" & sample$arc=="1",] 

Fx(test$vel.x,test$vel.y,test$vel.z,test$vel.h,test$vel,30,180)/0.057

