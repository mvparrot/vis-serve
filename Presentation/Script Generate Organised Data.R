#================================================
#--- Generated Orgasnised Data
#--- Alwin Wang
#================================================

library(ggplot2)
library(plotly)
library(dplyr)
library(purrr)

courtTrace <- data.frame(x = c(-11.89, -11.89, 0, 0, 0, 0, 0, 0, 11.89, 11.89, -11.89, -11.89, 11.89, 11.89, -11.89, -6.4, -6.4, 6.4, 6.4, 6.4, -6.4),
                         y = c(5.49, -5.49, -5.49, -6.5, -6.5, 6.5, 6.5, -5.49, -5.49, 5.49, 5.49, 4.115, 4.115, -4.115, -4.115, -4.115, 4.115, 4.115, -4.115, 0, 0),
                         z = c(0, 0, 0, 0, 1.09, 1.09, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

#--- Load the data
source("Helper Load Data.R")

#--- Load Generate Organised Data Functions
source("Helper Generate Organised Data.R")

#--- Load Generate Physical Properties Functions
source("Helper Generate Physical Properties.R")

#--- Remove outliers that had been identified
atp_serves_stub <- atp_serves[atp_serves$serveid != "2_06_02_1_214102.trj" 
                              & atp_serves$serveid != "2_04_01_1_172646.trj"
                              & atp_serves$serveid != "3_01_03_2_181341.trj"
                              & atp_serves$serveid != "4_03_05_1_190717.trj"
                              & atp_serves$serveid != "4_02_02_1_180400.trj"
                              & atp_serves$serveid != "1_09_02_1_191947.trj"
                              & atp_serves$serveid != "3_04_03_1_205313.trj"
                              & atp_serves$serveid != "4_05_01_1_213913.trj"
                              & atp_serves$serveid != "3_06_08_2_174140.trj",]

#atp_serves_stub <- atp_serves_stub[1:10,]

sample <- GeneratePoints(atp_serves_stub,
                         matchid,server,start.x,start.y,start.z,speed,center.x,center.y,net.clearance,
                         arc3=10,plot=TRUE)
sample <- physics(sample)
