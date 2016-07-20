#================================================
#--- Generate Values for Plotting
#--- Alwin Wang
#================================================
#--- Packages Required
# require(tidyr)
# require(dplyr)
# require(purrr)

PlottingValues <- function(coef.df, ..., tstep = 0.08) {
    # Gets the column numbers of the "serveid" and "..." columns
    extravars <- 1:(match("arc", colnames(coef.df)) - 1)
    
    # Calculate the position, velocity and acceleration at any point in time
    out <- coef.df %>% 
        # Select only the important variables
        select(extravars, arc, start, duration, dir, c0:c3) %>%
        # Group data by serve id
        group_by(serveid) %>% 
        # Perform the operatoin by rows (important!)
        rowwise() %>% 
        # Create individual sequences for arcs. These will be filled out rowwise
        do(data.frame(., t = c(seq(0,.$duration,by=tstep),.$duration))) %>%
        # Calculate the individual position, velocity, acceleration values
        mutate(p=c0 + c1*t + c2*t^2 + c3*t^3, v=c1 + 2*c2*t + 3*c3*t^2, a=2*c2 + 6*c3*t)
    
    # Clear up the data into a more manageable form for plotting
    out <- out %>%
        # Select only important columns
        select(1:length(extravars),arc,start,duration,t,dir,p,v,a) %>% 
        # Gather p, v, a values in "value" and name in "type"
        gather(type, value, -(1:(length(extravars)+5))) %>%
        # Combine the type and direction in "type"
        unite("type",c(type,dir),sep="") %>% 
        # Spread out the "value" based on the new "type" 
        spread(type, value) %>%
        # Calculate the horizontal, velocity, acceleration
        mutate(vh = sqrt(vx^2 + vy^2),
               v = sqrt(vx^2 + vy^2 + vz^2),
               a = sqrt(ax^2 + ay^2 + az^2)) %>%
        # Create the 'real' times for arc 3
        mutate(t = t + start) %>%
        # Rearrange columns to suit my OCD
        select(1:(length(extravars)),arc,start,t,px,py,pz,vx,vy,vz,vh,v,ax,ay,az,a)
}
