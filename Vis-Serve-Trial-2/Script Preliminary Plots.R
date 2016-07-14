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
atp_serves_stub <- atp_serves[atp_serves$serveid != "2_06_02_1_214102.trj" 
                              & atp_serves$serveid != "2_04_01_1_172646.trj"
                              & atp_serves$serveid != "3_01_03_2_181341.trj"
                              & atp_serves$serveid != "4_03_05_1_190717.trj"
                              & atp_serves$serveid != "4_02_02_1_180400.trj"
                              & atp_serves$serveid != "1_09_02_1_191947.trj"
                              & atp_serves$serveid != "3_04_03_1_205313.trj "
                              & atp_serves$serveid != "4_05_01_1_213913.trj"
                              & atp_serves$serveid != "3_06_08_2_174140.trj",]

# atp_serves_stub <- atp_serves_stub[1:10,]

sample <- GeneratePoints(atp_serves_stub,
                         matchid,server,start.x,start.y,start.z,speed,center.x,center.y,net.clearance,speed,
                         arc3=10,plot=TRUE)
sample <- physics(sample)

#--- Plot the court in 2D
# Top down view of all serves
ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    geom_line(aes(x=pos.x,y=pos.y, group=serveid, colour = server), data=sample, alpha = 0.1) +
    coord_fixed()

# Side of view of all serves
ggplot() + geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0), size=0.5) +
    geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='darkgrey') +
    geom_line(aes(x=pos.x, y=pos.z, group=serveid, colour=time), data=sample) +
    coord_fixed()

# Top down view of starting position (atp_serves_stub)
ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    geom_point(data = atp_serves_stub, aes(x=start.x, y=start.y)) +
    coord_fixed()

# Top down view of landing position (atp_serves_stub) coloured by side
ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    geom_point(data = atp_serves_stub, aes(x=center.x, y=center.y, colour = side)) +
    coord_fixed()

# Top down view of landing position (atp_serves_stub) coloured by type
ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    geom_point(data = atp_serves_stub, aes(x=center.x, y=center.y, colour = serve_classification)) +
    coord_fixed()

# Top down view of landing position (atp_serves_stub) coloured by type
ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    geom_point(data = atp_serves_stub, aes(x=center.x, y=center.y, colour = serve_classification)) + 
    coord_fixed()

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

baseplot <- ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') + 
    coord_fixed()

baseplot + geom_point(data = atp_serves_stub, aes(x=center.x, y=center.y, colour = serve_classification))

baseplot + geom_line(aes(x=pos.x,y=pos.y, group=serveid, colour = speed), data=sample, alpha = 0.5) + 
    geom_point(data = atp_serves_stub, alpha = 0.5, aes(x=center.x, y=center.y)) #+ 
#    geom_density_2d(data = atp_serves_stub, aes(center.x, center.y))

speedhist <- function(classnum, classname) {
    hist(x = atp_serves_stub$speed[atp_serves_stub$serve_classification == classnum]*3.6,
         xlim = c(100, 240), plot=TRUE, main = classname, xlab = "Speed")
}

speedhist(0,"Ace")

#--- This turns the numeric data into factor data.
atp_serves_stub$serve_classification <- 
    factor(atp_serves_stub$serve_classification, levels= 0:3, label=c("Ace", "Returned", "Not Returned","Fault"))

ggplot(atp_serves_stub, aes(speed, fill = factor(scorer))) + 
    geom_density(alpha = 0.2)

ggplot(atp_serves_stub, aes(speed*3.6, fill = serve_classification)) + 
    geom_density(alpha = 0.25)

ggplot(atp_serves_stub, aes(speed)) + 
    geom_density(alpha = 0.25)

sample$speedbin <- 
    cut(sample$speed,c(100/3.6,120/3.6,140/3.6,160/3.6,180/3.6,200/3.6,220/3.6,240/3.6,260/3.6))


library("hextri")
hextri(x=atp_serves_stub$center.x, y=atp_serves_stub$center.y,
       class=atp_serves_stub$scorer, colours=1:4, nbins = 20, border=TRUE,style="size")

hextri(x=atp_serves_stub$center.x, y=atp_serves_stub$center.y,
       class=atp_serves_stub$serve_classification, 
       colours=c("blue","orange","green","red"), nbins = 20, border=TRUE,style="size")
legend("topleft",fill=c("blue","orange","green","red"),
       legend=c("Ace","In Play","Not Returned","Fault"),bty="n")

hextri(x=sample$pos.x, y=sample$pos.y,
       class=sample$arc, 
       colours=c("blue","orange","green","red"), nbins = 25, border=TRUE,style="size")

hextri(x=sample$pos.x, y=sample$pos.y,
       class=sample$speedbin, 
       colours=1:10, nbins = 25, border=TRUE,style="size")
legend("topleft",fill=1:10,
       legend=1:10,bty="n")
