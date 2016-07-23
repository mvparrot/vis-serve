#================================================
#--- Calculating Spin
#--- Alwin Wang
#================================================
#--- Packages Required
# require(tidyr)
# require(dplyr)
# require(purrr)
# require(broom)

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
# suppose that w varies with time
w <- function(w0, t, k) w0 - t*(abs(k))
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
        sum((ax_p(v.adj,vx,vy,vz,w(par[1],t,par[4]),par[2],par[3]) - ax)^2) + 
        sum((ay_p(v.adj,vx,vy,vz,w(par[1],t,par[4]),par[2],par[3]) - ay)^2) +
        sum((az_p(v.adj,vx,vy,vz,w(par[1],t,par[4]),par[2],par[3]) - az)^2)
    ))
}

#--- Actual function that will be called.
SpinModel <-  function(values) {
    #--- Estimate the spin using optim to minimise the error length
    # Values to be estimated
    spinmodel <- values %>% 
        # Group by serveid, arc number
        group_by(serveid,arc) %>%
        # Add in required values for fitting (inc adjusted v) and R^2
        mutate(v.adj = (v)) %>%
        # Group by serveid, arc number
        group_by(serveid,arc)  %>%
        # Estimate using optim with inital parameters
        do(optimout = optim(par = c(500,5,5,0.001), SSR, data = .))
    
    #--- Tidy up results
    # Gather Parameters in a meaningful way
    parameters <- spinmodel %>%
        tidy(optimout) %>%
        spread(parameter,value) %>%
        rename("w0" = parameter1, "wx" = parameter2, 
               "wy" = parameter3, "k" = parameter4) %>%
        mutate(wz = wz(w0, wx,wy),
               w_rpm = w0*60/(2*pi),
               k = -abs(k))
    
    #--- Combine and state estimated results
    resultstidy <- values %>%
        left_join(., parameters, by = c("serveid", "arc")) %>%
        mutate(Cd = Cd(v,w(w0,t,k)), Cl = Cl(v,w(w0,t,k)),
               ax_p = ax_p(v,vx,vy,vz,w(w0,t,k),wx,wy),
               ay_p = ay_p(v,vx,vy,vz,w(w0,t,k),wx,wy),
               az_p = az_p(v,vx,vy,vz,w(w0,t,k),wx,wy)) %>%
        group_by(serveid,arc) %>%
        mutate(cor.x = cor(ax,ax_p),
               cor.y = cor(ay,ay_p),
               cor.z = cor(az,az_p),
               usefulness = sign(sign(cor.x) + sign(cor.y) + sign(cor.z) - 2) * abs(cor.x*cor.y*cor.z))
    #--- Return the result
    return(resultstidy)
}

