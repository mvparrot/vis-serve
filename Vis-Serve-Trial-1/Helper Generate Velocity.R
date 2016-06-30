#================================================
#--- Functions for creating velocity
#--- Alwin Wang
#================================================

velo_coords <- function(start, dir, flip, c0, c1, c2, c3, tm=0) {
    tm <- tm + start
    if (dir != "z") {
        c1 <- flip * c1
        c2 <- flip * c2
        c3 <- flip * c3
    }
    vel <- c1 + 2*c2*tm + 3*c3*tm^2
    return(vel)
}

velo_total <- function(vx,vy,vz) {
    vel <- sqrt(vx^2 + vy^2 + vz^2)
}


GenerateVel <- function(data, ..., arc1=10, arc3=0, plot=FALSE){
    require(tidyr)
    require(dplyr)
    require(purrr)
    
    data <- data %>% mutate(duration = arc1*duration.arc1 + arc3*duration.arc3, flip=sign(raw.x0.1))
    
    dots <- c("serveid", sapply(substitute(list(...))[-1], deparse))
    extravars <- match(dots[dots%in%colnames(data)], colnames(data))
    
    out <- data.frame()
    
    if(arc1!=0){
        arc1p <- data %>% select(extravars, start.1, duration.arc1, flip, raw.x0.1:raw.z3.1) %>%
            gather(coef, value, -(1:length(extravars)), -start.1, -duration.arc1, -flip) %>%
            separate(coef, c("junk1", "coef", "junk2"), sep="\\.") %>%
            select(-junk1, -junk2) %>%
            mutate(dir=substr(coef, 1, 1), coef=substr(coef, 2, 2)) %>%
            spread(coef, value) %>%
            rename("start" = `start.1`, "duration"=`duration.arc1`, "c0"=`0`, "c1"=`1`, "c2"=`2`, "c3"=`3`)
        arc1points <- seq(0,1,length.out=arc1) %>%
            map_df(function(x) mutate(rowwise(arc1p), time=start+duration*x, vel=velo_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x)))
        out <- rbind(out, cbind(arc=1, arc1points))
    }
    
    if(arc3!=0){
        arc3p <- data %>% select(extravars, start.3, duration.arc3, flip, raw.x0.3:raw.z3.3) %>%
            gather(coef, value, -(1:length(extravars)), -start.3, -duration.arc3, -flip) %>%
            separate(coef, c("junk1", "coef", "junk2"), sep="\\.") %>%
            select(-junk1, -junk2) %>%
            mutate(dir=substr(coef, 1, 1), coef=substr(coef, 2, 2)) %>%
            spread(coef, value) %>%
            rename("start" = `start.3`, "duration"=`duration.arc3`, "c0"=`0`, "c1"=`1`, "c2"=`2`, "c3"=`3`)
            arc3points <- seq(0,1, length.out=arc3) %>%
            map_df(function(x) mutate(rowwise(arc3p), time=start+duration*x, vel=velo_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x)))
        out <- rbind(out, cbind(arc=3, arc3points))
    }
    
    if(plot){
        extravars <- match(dots[dots%in%colnames(out)], colnames(out))
        out <- out %>% select(arc, extravars, flip, dir, time, vel) %>%
            spread(dir, vel)
        
        
    }
    
    out$v <- sqrt(out$x^2 + out$y^2 + out$z^2)
    
    return(out)
}



sample <- GenerateVel(atp_serves[1:10,],arc3=10,plot=TRUE)

courtTrace <- data.frame(x = c(-11.89, -11.89, 0, 0, 0, 0, 0, 0, 11.89, 11.89, -11.89, -11.89, 11.89, 11.89, -11.89, -6.4, -6.4, 6.4, 6.4, 6.4, -6.4),
                         y = c(5.49, -5.49, -5.49, -6.5, -6.5, 6.5, 6.5, -5.49, -5.49, 5.49, 5.49, 4.115, 4.115, -4.115, -4.115, -4.115, 4.115, 4.115, -4.115, 0, 0),
                         z = c(0, 0, 0, 0, 1.09, 1.09, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
    geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
    geom_line(aes(x=x,y=y, group=serveid), data=sample)

ggplot() + geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0), size=0.5) + 
    geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='darkgrey') + 
    geom_line(aes(x=x, y=z, group=serveid, colour=time), data=sample)

library(plotly)
plot_ly(sample, x=x, y=y, z=z, group=serveid, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=courtTrace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))

ggplot(sample,aes(x=x,y=v*3.6))
