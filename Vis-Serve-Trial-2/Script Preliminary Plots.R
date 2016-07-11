#================================================
#--- Preliminary Plots
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
atp_serves_stub <- atp_serves[atp_serves$start.x >= 8 & atp_serves$start.z > 2.5 & atp_serves$speed >= 25,]

#atp_serves_stub <- atp_serves_stub[1:100,]

sample <- GeneratePoints(atp_serves_stub,server,arc3=10,plot=TRUE)
sample <- physics(sample)

#--- Plot the court in 2D
ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    geom_line(aes(x=pos.x,y=pos.y, group=serveid), data=sample)

ggplot() + geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0), size=0.5) +
    geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='darkgrey') +
    geom_line(aes(x=pos.x, y=pos.z, group=serveid, colour=time), data=sample)

#--- Plot the court in 3D
fed <- sample %>% filter(server == "FEDERER")
plot_ly(fed, x=pos.x, y=pos.y, z=pos.z, group=serveid, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=courtTrace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))

# Court background theme
theme_blank <- theme_bw()
theme_blank$line <- element_blank()
theme_blank$axis.text <- element_blank()
theme_blank$axis.title <- element_blank()