#================================================
#--- Generated Physical Properties
#--- Alwin Wang
#================================================

physics <- function(data) {
    data$vel.h <- sqrt(data$vel.x^2 + data$vel.y^2)
    data$vel <- sqrt(data$vel.x^2 + data$vel.y^2 + data$vel.z^2)
    data$acc <- sqrt(data$acc.x^2 + data$acc.y^2 + data$acc.z^2)
    return(data)
}
