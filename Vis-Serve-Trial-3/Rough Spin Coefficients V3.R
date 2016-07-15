#================================================
#--- Calculating Spin Rough Working
#--- Alwin Wang
#================================================
#--- Packages Required
require(tidyr)
require(dplyr)
require(purrr)
require(broom)

data <- PlottingFactors(atp_serves)
coef.df <- StandardiseCoefficients(data,server,start.x, start.y, start.z, center.x, center.y, speed)
coef.df.spin <- coef.df %>% filter(arc == 1 & c3 != 0) 
values <- PlottingValues(coef.df.spin,server,start.x,start.y,speed, tstep = 0.1)

test <- values %>% filter(serveid == "1_01_01_1_194218.trj")

spinmodel <- test %>% 
    select(serveid, arc, speed, ax:az,  vx:vz, vh, v)
r <- 0.0335     # radius of the ball in m
A <- pi*r^2     # cross-sectional area of a ball
p <- 1.21       # density of air in kg/m^3
g <- 9.81       # acceleration due to gravity
m <- 0.0577     # mass of a tennis ball

# Drag FORCE
Cd <- function(v,w) 0.55 + 1/((22.5 + 4.2*(v/(r*w))^2.5)^0.4)
D <- function(v,w) {
    d <- Cd(v,w) *A*p*v^2/2
    return(d)
}

# Lift FORCE (at most 0.5)
Cl <- function(v,w) 1/(2+v/(r*w))
L <- function(v,w){
    l <- Cl(v,w) *A*p*v^2/2
    return(l)
}

# Rotation VECTOR (magnetude is rad/s)
wz <- function(w,wx,wy) sqrt(w^2 - wx^2 - wy^2)
cnorm <- function(wx,wy,wz,vx,vy,vz) 1/sqrt((wy*vz-wz*vy)^2 + (wx*vz-wz*vx)^2 + (wx*vy-wy*vx)^2)

# Predicted acceleration
ax_p <- function(v,vx,vy,vz,w,wx,wy) {
    (-D(v,w)/v * vx + L(v,w)*cnorm(wx,wy,wz(w,wx,wy),vx,vy,vz)*(wy*vz - wz(w,wx,wy)*vy))/m
}
ay_p <- function(v,vx,vy,vz,w,wx,wy) {
    (-D(v,w)/v * vy + L(v,w)*cnorm(wx,wy,wz(w,wx,wy),vx,vy,vz)*(-wx*vz + wz(w,wx,wy)*vx))/m
}
az_p <- function(v,vx,vy,vz,w,wx,wy) {
    (-D(v,w)/v * vz + L(v,w)*cnorm(wx,wy,wz(w,wx,wy),vx,vy,vz)*(wx*vy - wy*vx))/m - g
}

# MAKING THE ASSUMPTION THAT VELOCITY IS CONSTANT (BETTER FIT)
#   1   2   3
#   w   wx  wy
SSR <- function(data, par) {
    with(data, sum(
        (ax_p(speed,vx,vy,vz,par[1],par[2],par[3]) - ax)^2 + 
        (ay_p(speed,vx,vy,vz,par[1],par[2],par[3]) - ay)^2 +
        (az_p(speed,vx,vy,vz,par[1],par[2],par[3]) - az)^2
    ))
}
result <- optim(par = c(10,5,5), SSR, data = spinmodel)


data <- PlottingFactors(atp_serves)
coef.df <- StandardiseCoefficients(data,server,start.x, start.y, start.z, center.x, center.y,speed,serve_num,serve_classname)
values <- PlottingValues(coef.df,server,start.x,start.y,speed,serve_num,serve_classname, tstep = 0.1)

spinmodelall <- values %>% 
    select(serveid, arc, speed,serve_num,serve_classname, ax:az,  vx:vz, vh, v)
resultall <- spinmodelall %>% 
    select(serveid, arc, speed, ax:az,  vx:vz, vh, v) %>%
    group_by(serveid,arc) %>%
    do(optimres = optim(par = c(10,5,5), SSR, data = .))


resultsalltidy <- resultall %>% tidy(optimres) %>% spread(parameter,value)
resultsalltidy %>% rowwise() %>% do(Drag = D(.$parameter1, .$parameter2))
resultsalltidy %>% mutate(Drag = D(40,parameter1)/m)
