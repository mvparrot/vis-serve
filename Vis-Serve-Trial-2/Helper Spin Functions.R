#================================================
#--- Functions for modelling spin
#--- Alwin Wang
#================================================

r <- 0.033        # radius of the ball in m
A <- pi*r^2     # cross-sectional area of a ball
p <- 1.21       # density of air in kg/m^3

D <- function(v,vs) {
    d <- (0.55 + 1/((22.5 + 4.2*(v/vs)^2.5)^0.4)) *A*p*v^2/2
    return(d)    
}

L <- function(v,vs){
    l <- (1/(2+v/vs)) *A*p*v^2/2
    return(l)
}

deg2rad <- function(deg) {
    (deg * pi) / (180)
}

Fx <- function(vx,vy,vz,vh,v,vs,phi) {
    -D(v,vs)/v * vx + 1/(v*vh) * (-vy*v*L(v,vs)*sin(deg2rad(phi)) + vx*vz*L(v,vs)*cos(deg2rad(phi)))
}


