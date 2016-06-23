#================================================
#--- Generate X Y Z Variables
#--- Alwin Wang
#================================================

#--- Function to generate coef vector
coefvec <- function(data, var, row) {
    if (var=="x") {coefvec = c(data$x0.1, data$x1.1, data$x2.1, data$x3.1)}
    else if (var=="y") {coefvec = c(data$y0.1, data$y1.1, data$y2.1, data$y3.1)}
    else if (var=="z") {coefvec = c(data$z0.1, data$z1.1, data$z2.1, data$z3.1)}
    else {print("error in coefvec function")}
    return(coefvec)
}

#--- Function to generate t
timevec <- function(duration, res) {
    t <- t(seq(0, duration, length.out = res))
    return(t)
}

#--- Functions to create x, y, z
generate <- function(coef, t) {
    var <- coef[1] + coef[2]*t + coef[3]*t^2 + coef[4]*t^3
    return(var)
}

t <- timevec(0.4,20)

cx <- coefvec(test_data,"x",1)
cy <- coefvec(test_data,"y",1)
cz <- coefvec(test_data,"z",1)

x <- generate(cx,t)
y <- generate(cy,t)
z <- generate(cz,t)