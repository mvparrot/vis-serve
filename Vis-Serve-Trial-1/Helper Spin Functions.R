#================================================
#--- Functions for modelling spin
#--- Alwin Wang
#================================================

r <- 3.3        # radius of the ball in cm
A <- pi*r^2     # cross-sectional area of a ball
r <- 1.21       # density of air in kg/m^3

D <- function(v,vs) {
    d <- (0.55 + 1/((22.5 + 4.2*(v/vs)^2.5)^0.4)) *A*p*v^2/2
    return(d)    
}

L <- function(v,vs){
    l <- (1/(2+v/vs)) *A*p*v^2/2
    return(l)
}


