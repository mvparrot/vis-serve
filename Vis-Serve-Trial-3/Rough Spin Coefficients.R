#================================================
#--- Calculating Spin Rough Working
#--- Alwin Wang
#================================================
#--- Packages Required
require(tidyr)
require(dplyr)
require(purrr)
require(broom)

#--- looking at coefficients
coef.df %>% 
    select (serveid, arc, dir, c0:c3) %>%
    order_by(arc,dir)
    group_by(arc,dir)

ax_th <- function(Cg, Cd, Cl, al0, wx,  wy, wz, vx, vy, vz, v) {
    -al0 * Cd * v * vx + (al0 * Cl * v * (vz*wy - vy*wz)/w)
}
ay_th <- function(Cg, Cd, Cl, al0, wx,  wy, wz, vx, vy, vz, v) {
    -al0 * Cd * v * vy + (al0 * Cl * v *(-vz*wx + vx*wz)/w)
}
az_th <- function(Cg, Cd, Cl, al0, wx,  wy, wz, vx, vy, vz, v) {
    -Cg - g - al0 * Cd * v * vz + (al0 * Cl * v * (vy*wx - vx*wy)/w)
}

data <- PlottingFactors(atp_serves)
coef.df <- StandardiseCoefficients(data,server,start.x, start.y, start.z, center.x, center.y)
coef.df.spin <- coef.df %>% 
    filter(arc == 1 & c3 != 0) 
values <- PlottingValues(coef.df.spin,server,start.x,start.y,tstep = 0.1)

base <- ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    coord_fixed()

base + geom_line(aes(x=px,y=py, group=serveid, colour = server), data=values[values$server=="BERDYCH",], alpha = 1)

test <- values %>% filter(serveid == "1_01_01_1_194218.trj")

spinmodel <- test %>% 
    mutate(vvx = v*vx, vvy = v*vy, vvz = v*vz) %>%
    select(serveid, arc, ax:az,  vvx:vvz)

#--- Optim Example
dat=data.frame(x=c(1,2,3,4,5,6), y=c(1,3,5,6,8,12))
min.RSS <- function(data, par) {
    with(data, sum((par[1] + par[2] * x - y)^2))
}
result <- optim(par = c(0, 1), min.RSS, data = dat)

#       1   2   3   4   5   6
# par:  cg  cd  cl  wx  wy  wz
SSR <- function(data, par) {
    with(data, sum(
        ((-par[2]*vvx - par[3]*par[6]*vvy + par[3]*par[5]*vvz) - ax)^2 +
        ((par[3]*par[6]*vvx - par[2]*vvy - par[3]*par[4]*vvz) - ay)^2 +
        ((-9.81 - par[3]*par[5]*vvx + par[3]*par[4]*vvy - par[2]*vvz) - az)^2
    ))
}
result <- optim(par = c(9.81,0.55,0.7,1,1,1), SSR, data = spinmodel)

SST <- function(data) {
    with(data, sum(
        sum(ax - mean(ax))^2 +
        sum(ay - mean(ay))^2 +
        sum(ax - mean(az))^2
    ))
}

SST(test)

test2 <- values
result2 <- test2 %>%
    mutate(vvx = v*vx, vvy = v*vy, vvz = v*vz) %>%
    select(serveid, arc, ax:az,  vvx:vvz) %>%
    group_by(serveid,arc) %>%
    do(result = optim(par = c(9.81,0.55,0.7,1,1,1), SSR, data = .))
    
result2 %>% tidy(result) %>% spread(parameter,value)



#======================= > USELESS < =======================

# Comes out with R^2 = 1, perfect fit for everything (ie. collinearity)!!!
# spinmodel <- values %>% 
#     select(serveid, arc, ax:az,  vx:vz) %>%
#     group_by(serveid,arc) %>%
#     do(spinx = lm(ax ~  0 + vx + vy + vz, data=.),
#        spiny = lm(ay ~  0 + vx + vy + vz, data=.),
#        spinz = lm(az ~  0 + vx + vy + vz, data=.))

spinmodel <- values %>% 
    mutate(vvx = v*vx, vvy = v*vy, vvz = v*vz) %>%
    select(serveid, arc, ax:az,  vvx:vvz) %>%
    group_by(serveid,arc) %>%
    do(spinx = lm(ax ~  0 + vvx + vvy + vvz, data=.),
       spiny = lm(ay ~  0 + vvx + vvy + vvz, data=.),
       spinz = lm(az ~  vvx + vvy + vvz, data=.))

# Coefficients, statistical properties, etc
# spinmodel %>% tidy(spinx)
# spinmodel %>% tidy(spiny)
# spinmodel %>% tidy(spinz)
# spinmodel %>% glance(spinx)
# spinmodel %>% glance(spiny)
# spinmodel %>% glance(spinz)

# Getting out the requried coefficients
spinx <- spinmodel %>%
    tidy(spinx) %>%
    select(serveid, arc, term, estimate) %>%
    mutate(term = paste("x",term,sep=".")) %>%
    spread(term, estimate)
spiny <- spinmodel %>%
    tidy(spiny) %>%
    select(serveid, arc, term, estimate) %>%
    mutate(term = paste("y",term,sep=".")) %>%
    spread(term, estimate)
spinz <- spinmodel %>%
    tidy(spinz) %>%
    select(serveid, arc, term, estimate) %>%
    mutate(term = paste("z",term,sep=".")) %>%
    spread(term, estimate)
spin <- merge(merge(spinx,spiny,by=c("serveid","arc")),spinz,by=c("serveid","arc"))

#======================= ^ USELESS ^ =======================



