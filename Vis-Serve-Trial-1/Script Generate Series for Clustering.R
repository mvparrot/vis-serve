#================================================
#--- Rough script to create x, y, z coordinates
#--- Alwin Wang
#================================================

test_data <- atp_serves[1:10,]
t_end <- 0.4
t_step <- 0.05

#---
summary_data <- test_data[,c("matchid","serveid","start.x","start.y","start.z",
                            "speed","center.x","center.y","duration.arc1","duration.arc3")]
# Can add more later

t <- seq(0, t_end, by=t_step)
#summary_data$t <- rep(seq(0, t_end, by=t_step),5)

pos.x <- matrix(0,nrow(test_data),length(t))
pos.y <- matrix(0,nrow(test_data),length(t))
pos.z <- matrix(0,nrow(test_data),length(t))

for (row in 1:nrow(test_data)) {
    #-- Generate the time series
    t <- seq(test_data$start.1[row], test_data$start.1[row] + t_end, by=t_step)
    tpoly <- rbind(1, t^1, t^2, t^3)
    #-- Grab the coefficients
    coefx <- unlist(test_data[row,c("raw.x0.1", "raw.x1.1", "raw.x2.1", "raw.x3.1")])
    coefy <- unlist(test_data[row,c("raw.y0.1", "raw.y1.1", "raw.y2.1", "raw.y3.1")])
    if (test_data$raw.x0.1[row]<0) {
        coefx <- -coefx
        coefy <- -coefy
    }
    coefz <- unlist(test_data[row,c("raw.z0.1", "raw.z1.1", "raw.z2.1", "raw.z3.1")])
    #--- Multiply the coefficients
    x <- coefx %*% tpoly
    y <- coefy %*% tpoly
    z <- coefz %*% tpoly
    #--- Write to a matrix
    pos.x[row,] <- x
    pos.y[row,] <- y
    pos.z[row,] <- z
}

summary_data$pos.x <- pos.x
summary_data$pos.y <- pos.y
summary_data$pos.z <- pos.z

pairs(summary_data[,c("pos.x","pos.y","pos.z")])
