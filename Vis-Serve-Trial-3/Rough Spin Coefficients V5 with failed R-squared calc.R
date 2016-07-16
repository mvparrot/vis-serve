#================================================
#--- Calculating Spin
#--- Alwin Wang
#================================================
#--- Packages Required
require(tidyr)
require(dplyr)
require(purrr)
require(broom)

#--- Parameters
r <- 0.0335     # radius of the ball in m
A <- pi*r^2     # cross-sectional area of a ball
p <- 1.21       # density of air in kg/m^3
g <- 9.81       # acceleration due to gravity
m <- 0.0577     # mass of a tennis ball

#--- Drag FORCE (Cd between 0.55 and 0.84)
# Drag Coefficient
Cd <- function(v,w) 0.55 + 1/((22.5 + 4.2*(v/(r*w))^2.5)^0.4)
# Drag Force
D <- function(v,w)  Cd(v,w) *A*p*v^2/2

#--- Lift FORCE (Cl at most 0.5)
# Lift Coefficient
Cl <- function(v,w) 1/(2+v/(r*w))
# Lift Force
L <- function(v,w)  Cl(v,w) *A*p*v^2/2

#--- Rotation VECTOR (magnitude is rad/s)
# z direction of axis
wz <- function(w,wx,wy) sqrt(w^2 - wx^2 - wy^2)
# Reciprocal of the length of the cross product result
cnorm <- function(wx,wy,wz,vx,vy,vz) 1/sqrt((wy*vz-wz*vy)^2 + (wx*vz-wz*vx)^2 + (wx*vy-wy*vx)^2)

#--- Predicted acceleration
# x direction
ax_p <- function(v,vx,vy,vz,w,wx,wy) {
    (-D(v,w)/v * vx + L(v,w)*cnorm(wx,wy,wz(w,wx,wy),vx,vy,vz)*(wy*vz - wz(w,wx,wy)*vy))/m
}
# y direction
ay_p <- function(v,vx,vy,vz,w,wx,wy) {
    (-D(v,w)/v * vy + L(v,w)*cnorm(wx,wy,wz(w,wx,wy),vx,vy,vz)*(-wx*vz + wz(w,wx,wy)*vx))/m
}
# z direction
az_p <- function(v,vx,vy,vz,w,wx,wy) {
    (-D(v,w)/v * vz + L(v,w)*cnorm(wx,wy,wz(w,wx,wy),vx,vy,vz)*(wx*vy - wy*vx))/m - g
}

#--- Minimise the length of the error vector
# Allowing for some "adjusted" velocity
#   1   2   3
#   w   wx  wy
SSR <- function(data, par) {
    with(data, sum(
        sum((ax_p(v.adj,vx,vy,vz,par[1],par[2],par[3]) - ax)^2) + 
        sum((ay_p(v.adj,vx,vy,vz,par[1],par[2],par[3]) - ay)^2) +
        sum((az_p(v.adj,vx,vy,vz,par[1],par[2],par[3]) - az)^2)
    ))
}

#--- Estimate the spin using optim to minimise the error length
# True data to be estimated
truedata <- values %>% 
    # Group by serveid, arc number
    group_by(serveid,arc) %>%
    # Add in required values for fitting (inc adjusted v) and R^2
    mutate(v.adj = mean(v), SST = sum(
               sum((ax - mean(ax))^2) +
               sum((ay - mean(ay))^2) +
               sum((az - mean(az))^2)))
# Spin model of estimated outputs
spinmodel <- truedata %>%
    # Group by serveid, arc number
    group_by(serveid,arc)  %>%
    # Estimate using optim with inital parameters
    do(optimout = optim(par = c(500,5,5), SSR, data = .))

#--- Tidy up results
# Gather Parameters in a meaningful way
parameters <- spinmodel %>%
    tidy(optimout) %>%
    spread(parameter,value) %>%
    rename("w" = parameter1, "wx" = parameter2, "wy" = parameter3) %>%
    mutate(wz = wz(w, wx,wy),
           w_rpm = w*60/(2*pi))
# R-squared calculation (NOT Valid for non linear models...)
rsquared <- spinmodel %>%
    glance(optimout) %>%
    select(serveid,arc,value) %>%
    rename("SSR" = value)
rsquared <- truedata %>% 
    select(serveid,arc,SST) %>%
    distinct(serveid,arc,.keep_all=TRUE) %>%
    left_join(.,rsquared,by=c("serveid","arc")) %>%
    mutate(r2 = 1 - SSR/SST)

#--- Combine and state estimated results
resultstidy <- values %>%
    left_join(., parameters, by = c("serveid", "arc")) %>%
    left_join(., rsquared, by= c("serveid", "arc")) %>%
    mutate(cd = Cd(v,w), Cl = Cl(v,w),
           ax_p = ax_p(v,vx,vy,vz,w,wx,wy),
           ay_p = ay_p(v,vx,vy,vz,w,wx,wy),
           az_p = az_p(v,vx,vy,vz,w,wx,wy))

hist(rsquared$r2)

data <- PlottingFactors(atp_serves[1:10,])
# data <- PlottingFactors(atp_serves)
coef.df <- StandardiseCoefficients(data,server,start.x, start.y, start.z, center.x, center.y,speed,serve_num,serve_classname)
values <- PlottingValues(coef.df,server,start.x,start.y,speed,serve_num,serve_classname, tstep = 0.1)


resultsalltidy <- spinmodel %>% tidy(optimres) %>% spread(parameter,value)
hist(resultsalltidy$parameter1[resultsalltidy$parameter1<600]*60/(2*3.142))

out <- merge.data.frame(coef.df, resultsalltidy, by = c("serveid", "arc")) %>% 
    filter(parameter1 < 500)
ggplot(out, aes(parameter1*60/(2*pi), fill = factor(serve_num))) + 
    geom_density(alpha = 0.2)
ggplot(out, aes(parameter1*60/(2*pi), fill = factor(arc))) + 
    geom_density(alpha = 0.2)
ggplot(out, aes(parameter1*60/(2*pi), fill = factor(serve_classname))) + 
    geom_density(alpha = 0.2) + facet_wrap(~arc)
ggplot(out, aes(parameter1*60/(2*pi), fill = factor(sign(parameter2)))) + 
    geom_density(alpha = 0.2)
ggplot(out, aes(parameter1*60/(2*pi), fill = factor(server))) + 
    geom_density(alpha = 0.2) + facet_wrap(~serve_num)
median(out$parameter1[out$serve_num == "First Serve"])*60/(2*pi)
median(out$parameter1[out$serve_num == "Second Serve"])*60/(2*pi)

keep <- atp_serves %>% select(server, matchid) %>% 
    distinct() %>% count(server, sort=TRUE) %>% filter(n>2)
arc1 <- out %>% filter(arc == 1) %>% filter(server %in% keep$server)
ggplot(arc1, aes(parameter1*60/(2*pi), fill = factor(serve_num))) +
    geom_density(alpha = 0.2) + facet_wrap(~server)
median(arc1$parameter1[arc1$serve_num == "First Serve"])*60/(2*pi)
median(arc1$parameter1[arc1$serve_num == "Second Serve"])*60/(2*pi)
    