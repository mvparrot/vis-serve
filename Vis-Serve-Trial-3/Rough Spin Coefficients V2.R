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
coef.df <- StandardiseCoefficients(data,server,start.x, start.y, start.z, center.x, center.y)
coef.df.spin <- coef.df %>% filter(arc == 1 & c3 != 0) 
values <- PlottingValues(coef.df.spin,server,start.x,start.y,tstep = 0.1)

base <- ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    coord_fixed()
base + geom_line(aes(x=px,y=py, group=serveid, colour = server), data=values[values$server=="BERDYCH",], alpha = 1)

test <- values %>% filter(serveid == "1_01_01_1_194218.trj")

spinmodel <- test %>% 
    select(serveid, arc, ax:az,  vx:vz, vh, v)
r <- 0.0335     # radius of the ball in m
A <- pi*r^2     # cross-sectional area of a ball
p <- 1.21       # density of air in kg/m^3
g <- 9.81       # acceleration due to gravity
m <- 0.0577     # mass of a tennis ball

# Drag FORCE
D <- function(v,w) {
    d <- (0.55 + 1/((22.5 + 4.2*(v/(r*w))^2.5)^0.4)) *A*p*v^2/2
    return(d)
}

# Lift FORCE (at most 0.5)
L <- function(v,w){
    l <- (1/(2+v/(r*w))) *A*p*v^2/2
    return(l)
}

#   1   2
#   w   phi
SSR <- function(data, par) {
    with(data, sum(
        ((-D(v,par[1])/v * vx + L(v,par[1])/(v*vh) * (-vy*v*sin(par[2]) + vx*vz*cos(par[2]))) - m*ax)^2 + 
        ((-D(v,par[1])/v * vy + L(v,par[1])/(v*vh) * (vx*v*sin(par[2]) + vy*vz*cos(par[2]))) - m*ay)^2 + 
        ((-D(v,par[1])/v * vz + L(v,par[1])/(v*vh) * (vh^2*cos(par[2])) - 9.81) - m*az)^2
    ))
}
result <- optim(par = c(300,1), SSR, data = spinmodel)

spinmodelall <- values %>% 
    select(serveid, arc, ax:az,  vx:vz, vh, v)
resultall <- spinmodelall %>% 
    select(serveid, arc, ax:az,  vx:vz, vh, v) %>%
    group_by(serveid,arc) %>%
    do(optimres = optim(par = c(300,1), SSR, data = .))

resultsalltidy <- resultall %>% tidy(optimres) %>% spread(parameter,value)
resultsalltidy %>% rowwise() %>% do(Drag = D(.$parameter1, .$parameter2))
resultsalltidy %>% mutate(Drag = D(40,parameter1)/m)
