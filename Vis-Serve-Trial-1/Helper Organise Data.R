#================================================
#--- Organise Data
#--- Alwin Wang
#================================================

load("atp_serves_raw.RData")
library(dplyr)
library(purrr)

#-- Generate Standardised Coefficients
#--- Standarise by exaplanding:
#--- a0 + a1(t-s) + a2(t-s)^2 + a3(t-s)^3
#--- where t is time and s is the starting time
a0dash <- function(a0,a1,a2,a3,s) {
    a0dash <- a0 + a1*s + a2*s^2 + a3*s^3
    return(a0dash)
}
a1dash <- function(a0,a1,a2,a3,s) {
    a1dash <- a1 + 2*a2*s + 3*a3*s^3
    return(a1dash)
}
a2dash <- function(a0,a1,a2,a3,s) {
    a2dash <- a2 + 3*a3*s
    return(a2dash)
}
a3dash <- function(a0,a1,a2,a3,s) {
    a3dash <- a3
    return(a3dash)
}
a0dash <- Vectorize(a0dash)
a1dash <- Vectorize(a1dash)
a2dash <- Vectorize(a2dash)
a3dash <- Vectorize(a3dash)

standardise <- function(a0,a1,a2,a3,s) {
    data.frame(a0dash(a0,a1,a2,a3,s),a1dash(a0,a1,a2,a3),
               a2dash(a0,a1,a2,a3,s),a3dash(a0,a1,a2,a3))
}

temp <- atp_serves[1:3,] %>%
    by_row(~standardise(.$raw.x0.1,.$raw.x1.1,.$raw.x2.1,.$raw.x3.1,
                        .$start.1), .to = "standard")

#apply(atp_serves,2,a3dash(atp_serves$raw.x0.1,atp_serves$start.1))

