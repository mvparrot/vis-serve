#================================================
#--- Functions for modelling spin (xyz coord)
#--- Alwin Wang
#================================================

r <- 0.0335     # radius of the ball in m
A <- pi*r^2     # cross-sectional area of a ball
p <- 1.29       # density of air in kg/m^3
g <- 9.81       # acceleration due to gravity
m <- 0.0577     # mass of a tennis ball

D <- function(v,vs) {
    d <- (0.55 + 1/((22.5 + 4.2*(v/vs)^2.5)^0.4)) *A*p*v^2/2
    return(d)    
}

L <- function(v,vs){
    l <- (1/(2+v/vs)) *A*p*v^2/2
    return(l)
}

interactionterms <- function(data) {
    data$v.vx <- data$vel * data$vel.x
    data$v.vy <- data$vel * data$vel.y
    data$v.vz <- data$vel * data$vel.z
    return(data)
}

test <- interactionterms(test)

fitx <- lm(formula = acc.x ~ 0 + v.vx + v.vz + v.vy, data=test)
fity <- lm(formula = acc.y ~ 0 + v.vy + v.vx + v.vz, data=test)
fitz <- lm(formula = -acc.z ~ v.vz, data=test)

coefficients(fitx)
coefficients(fity)
coefficients(fitz)
