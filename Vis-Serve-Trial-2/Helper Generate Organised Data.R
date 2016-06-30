#================================================
#--- Functions for creating organised data
#--- Alwin Wang with LOTS of help from Mitchell O'Hara-Wild
#================================================

#--- Generate the coordinates: x,y,z
traj_coords <- function(start, dir, flip, c0, c1, c2, c3, tm=0) {
    tm <- tm + start
    if (dir != "z") {
        c0 <- flip * c0
        c1 <- flip * c1
        c2 <- flip * c2
        c3 <- flip * c3
    }
    pos <- c0 + c1*tm + c2*tm^2 + c3*tm^3
    return(pos)
}

#--- Generate the velocities: vx, vy, vz
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

#--- Generate the accelerations: ax, ay, az
acce_coords <- function(start, dir, flip, c0, c1, c2, c3, tm=0) {
    tm <- tm + start
    if (dir != "z") {
        c2 <- flip * c2
        c3 <- flip * c3
    }
    acc <- 2*c2 + 6*c3*tm
    return(acc)
}

#--- Genreate Summary Data
GeneratePoints <- function(data, ..., arc1=10, arc3=0, plot=FALSE){
    require(tidyr)
    require(dplyr)
    require(purrr)
    
    #arc2*(start.3 - (start.1+duration.arc1))
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
            map_df(function(x) mutate(rowwise(arc1p), time=start+duration*x, 
                                      pos=traj_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x),
                                      vel=velo_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x),
                                      acc=acce_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x)))
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
            map_df(function(x) mutate(rowwise(arc3p), time=start+duration*x,  
                                      pos=traj_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x),
                                      vel=velo_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x),
                                      acc=acce_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x)))
        out <- rbind(out, cbind(arc=3, arc3points))
    }
    
    
#    tidy <- out %>% select(extravars,pos,vel,acc) %>%
#        gather(out, type, value, -(1:length(extravars)))
    
    out <- gather(out,type,value,-(1:11))
    
    
    if(plot){
        extravars <- match(dots[dots%in%colnames(out)], colnames(out))
        out <- out %>% select(arc, extravars, flip, dir, time, type, value) %>%
            unite("type",c(type,dir),sep=".") %>% spread(type, value)
    }
    
    return(out)
}

#sample <- GeneratePoints(atp_serves[1:10,],arc3=10,plot=TRUE)

print("generate organised data functions loaded")