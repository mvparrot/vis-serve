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
    
    # Adds a duration column and a flip column
    data <- data %>% mutate(duration = arc1*duration.arc1 + arc3*duration.arc3, flip=sign(raw.x0.1))
    
    # Selects the serve id and the additional columsn parsed in for the function
    #   if empty, it will be "serveid" "..."
    dots <- c("serveid", sapply(substitute(list(...))[-1], deparse))
    # Gets the column numbers of the "serveid" and "..." columns
    extravars <- match(dots[dots%in%colnames(data)], colnames(data))
    
    # Empty data frame
    out <- data.frame()
    
    # Case if it's arc 1
    if(arc1!=0){
    # arc1p generates a dataframe of:
    # serve id | ...extravars... | start | duration | flip | dir | c0 | c1 | c2| c3 
        # Selects the columns that are extracars, start.1, duraction.arc1, flip, rawx0.1 to rawz3.1
        #   ie all other columns are discarded
        arc1p <- data %>% select(extravars, start.1, duration.arc1, flip, raw.x0.1:raw.z3.1) %>%
            # gathers raw.x0.1 to raw.z3.1 in a new column called "coef".
            #   gathers the values of raw.x0.1 to raw.z3.1 in a new column called "value"
            #   all the other columns are retained.
            gather(coef, value, -(1:length(extravars)), -start.1, -duration.arc1, -flip) %>%
            # separates coef names into three separate ones
            #   e.g. raw.x0.1 --> raw   x0   1
            separate(coef, c("junk1", "coef", "junk2"), sep="\\.") %>%
            # removes the first and last junk columns from the arc1p dataframe
            select(-junk1, -junk2) %>%
            # takes the first character and keeps it in "dir" column
            #   takes the second character and keeps it in the "coef" column
            #   e.g. x0 --> coef = 0   dir = x
            mutate(dir=substr(coef, 1, 1), coef=substr(coef, 2, 2)) %>%
            # spreads the "value" column into  separate 0,1,2,3 columns
            #   i.e. uses value in "coef" column to spread out the value data
            spread(coef, value) %>%
            # renames each of the current column names into better ones
            rename("start" = `start.1`, "duration"=`duration.arc1`, "c0"=`0`, "c1"=`1`, "c2"=`2`, "c3"=`3`)
        # Creates an empty sequence from 0 to 1 with the length of ac1 (input)
        arc1points <- seq(0,1,length.out=arc1) %>%
            # maps it out as a dara frame
            map_df(function(x) mutate(rowwise(arc1p), time=start+duration*x, 
                                      pos=traj_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x),
                                      vel=velo_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x),
                                      acc=acce_coords(start, dir, flip, c0, c1, c2, c3, tm=duration*x)))
        out <- rbind(out, cbind(arc=1, arc1points))
    }
    
    # I might need a for loop... 
    #   for each iteration it creates a time vector then is mutated to be a dataframe including time, pos, vel, acc
    
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
    
    
#    out <- out %>% select((1:length(extravars)+1),pos,vel,acc) %>%
#        gather(type, value, -(1:length(extravars)+1))
    
    out <- gather(out,type,value,-(1:(10+length(extravars))))
    
    
    if(plot){
        extravars <- match(dots[dots%in%colnames(out)], colnames(out))
        out <- out %>% select(arc, extravars, flip, dir, time, type, value) %>%
            unite("type",c(type,dir),sep=".") %>% spread(type, value)
    }
    
    return(out)
}

#sample <- GeneratePoints(atp_serves[1:10,],arc3=10,plot=TRUE)

print("generate organised data functions loaded")