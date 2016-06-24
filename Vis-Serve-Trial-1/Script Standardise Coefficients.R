#================================================
#--- Standardise Coefficients based on test data
#--- Alwin Wang
#================================================
library(ggplot2)

#--- Select a sample and extract relevant values
test_data <- atp_serves[2,]
a0 <- test_data$raw.x0.1
a1 <- test_data$raw.x1.1
a2 <- test_data$raw.x2.1
a3 <- test_data$raw.x3.1
t <- test_data$start.1

#--- Standarise by exaplanding:
#--- a0 + a1(t-s) + a2(t-s)^2 + a3(t-s)^3
#--- where t is time and s is the starting time
a0dash <- a0 + a1*t + a2*t^2 + a3*t^3
a1dash <- a1 + 2*a2*t + 3*a3*t^3
a2dash <- a2 + 3*a3*t 
a3dash <- a3

#--- Plot each of them to compare
t <- seq(0, 0.4, length.out = 20)
tpoly <- rbind(1, t^1, t^2, t^3)
xdash <- c(a0dash,a1dash,a2dash,a3dash) %*% tpoly

t <- t + test_data$start.1
tpoly <- rbind(1, t^1, t^2, t^3)
x <- c(a0,a1,a2,a3) %*% tpoly
plot(t,xdash)






#--- Select a sample and extract relevant values
a0 <- test_data$raw.y0.1
a1 <- test_data$raw.y1.1
a2 <- test_data$raw.y2.1
a3 <- test_data$raw.y3.1
t <- test_data$start.1

#--- Standarise by exaplanding:
#--- a0 + a1(t-s) + a2(t-s)^2 + a3(t-s)^3
#--- where t is time and s is the starting time
a0dash <- a0 + a1*t + a2*t^2 + a3*t^3
a1dash <- a1 + 2*a2*t + 3*a3*t^3
a2dash <- a2 + 3*a3*t 
a3dash <- a3

#--- Plot each of them to compare
t <- seq(0, 0.4, length.out = 20)
tpoly <- rbind(1, t^1, t^2, t^3)
ydash <- c(a0dash,a1dash,a2dash,a3dash) %*% tpoly

t <- t + test_data$start.1
tpoly <- rbind(1, t^1, t^2, t^3)
y <- c(a0,a1,a2,a3) %*% tpoly
plot(t,ydash)





#--- Select a sample and extract relevant values
a0 <- test_data$raw.z0.1
a1 <- test_data$raw.z1.1
a2 <- test_data$raw.z2.1
a3 <- test_data$raw.z3.1
t <- test_data$start.1

#--- Standarise by exaplanding:
#--- a0 + a1(t-s) + a2(t-s)^2 + a3(t-s)^3
#--- where t is time and s is the starting time
a0dash <- a0 + a1*t + a2*t^2 + a3*t^3
a1dash <- a1 + 2*a2*t + 3*a3*t^3
a2dash <- a2 + 3*a3*t 
a3dash <- a3

#--- Plot each of them to compare
t <- seq(0, 0.4, length.out = 20)
tpoly <- rbind(1, t^1, t^2, t^3)
zdash <- c(a0dash,a1dash,a2dash,a3dash) %*% tpoly

t <- t + test_data$start.1
tpoly <- rbind(1, t^1, t^2, t^3)
z <- c(a0,a1,a2,a3) %*% tpoly
plot(t,zdash)



#--- BASIC 3D plots
library("rgl")
plot3d(x,y,z, type='s', size = 2,
       aspect="iso",
       xlim=c(-12,12), ylim=c(-6,6), zlim=c(0,5))
plot3d(x,y,z, type='h', add=TRUE)

