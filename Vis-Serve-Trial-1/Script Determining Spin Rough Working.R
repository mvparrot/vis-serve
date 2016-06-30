#================================================
#--- Attempt 1 at finding spin
#--- Alwin Wang
#================================================

source("Helper Load Data.R")

test_data <- atp_serves[1,]

row = 1
disp <- rbind(unlist(test_data[row,c("x0.1", "x1.1", "x2.1", "x3.1")]),
              unlist(test_data[row,c("y0.1", "y1.1", "y2.1", "y3.1")]),
              unlist(test_data[row,c("z0.1", "z1.1", "z2.1", "z3.1")]))

velo <- cbind(disp[,2],2*disp[,3],3*disp[,4])

accel <- cbind()

coefx <- unlist(test_data[1,c("x0.1", "x1.1", "x2.1", "x3.1")])
ax <- 2*coefx[2] + 6*coefx[3]

