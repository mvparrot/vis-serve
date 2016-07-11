#================================================
#--- Standarise the Coefficients
#--- Alwin Wang
#================================================

a0dash <- function(a0, a1, a2, a3, s) {
    a0dash <- a0 + a1*s + a2*s^2 + a3*s^3
    return(a0dash)
}
a1dash <- function(a0, a1, a2, a3, s) {
    a1dash <- a1 + 2*a2*s + 3*a3*s^3
    return(a1dash)
}
a2dash <- function(a0, a1, a2, a3, s) {
    a2dash <- a2 + 3*a3*s 
    return(a2dash)
}
a3dash <- function(a0, a1, a2, a3, s) {
    a3dash <- a3
    return(a3dash)
}

standardise <- function(data) {
    # Arc 1
    data$a0.1 <- a0dash(data$raw.x0.1,data$raw.x1.1,data$raw.x2.1,data$raw.x3.1,data$start.1)
    data$a1.1 <- a1dash(data$raw.x0.1,data$raw.x1.1,data$raw.x2.1,data$raw.x3.1,data$start.1)
    data$a2.1 <- a2dash(data$raw.x0.1,data$raw.x1.1,data$raw.x2.1,data$raw.x3.1,data$start.1)
    data$a3.1 <- a3dash(data$raw.x0.1,data$raw.x1.1,data$raw.x2.1,data$raw.x3.1,data$start.1)
    
    data$b0.1 <- a0dash(data$raw.y0.1,data$raw.y1.1,data$raw.y2.1,data$raw.y3.1,data$start.1)
    data$b1.1 <- a1dash(data$raw.y0.1,data$raw.y1.1,data$raw.y2.1,data$raw.y3.1,data$start.1)
    data$b2.1 <- a2dash(data$raw.y0.1,data$raw.y1.1,data$raw.y2.1,data$raw.y3.1,data$start.1)
    data$b3.1 <- a3dash(data$raw.y0.1,data$raw.y1.1,data$raw.y2.1,data$raw.y3.1,data$start.1)
    
    data$c0.1 <- a0dash(data$raw.z0.1,data$raw.z1.1,data$raw.z2.1,data$raw.z3.1,data$start.1)
    data$c1.1 <- a1dash(data$raw.z0.1,data$raw.z1.1,data$raw.z2.1,data$raw.z3.1,data$start.1)
    data$c2.1 <- a2dash(data$raw.z0.1,data$raw.z1.1,data$raw.z2.1,data$raw.z3.1,data$start.1)
    data$c3.1 <- a3dash(data$raw.z0.1,data$raw.z1.1,data$raw.z2.1,data$raw.z3.1,data$start.1)
    
    return(data)
}