#================================================
#--- Generate Values for Plotting
#--- Alwin Wang
#================================================
#--- Packages Required
require(tidyr)
require(dplyr)
require(purrr)

PlottingValues <- function(coef.df, ..., tstep = 0.05) {
    # Selects the serveid and the additional columns parsed in for the function
    dots <- c("serveid", sapply(substitute(list(...))[-1], deparse))
    # Gets the column numbers of the "serveid" and "..." columns
    extravars <- match(dots[dots%in%colnames(coef.df)], colnames(coef.df))
    
    # Initialise empty data frame
    out <- data.frame()

    # Create a prgressbar to make sure my code is working!!
    pb <- txtProgressBar(min = 0, max = nrow(coef.df), style = 3)
    
    # Generate values for each row
    for (row in 1:nrow(coef.df)) {
        # Generate a time vector of given step size AND end time
        t <- c(seq(from = 0, to = coef.df$duration[row], by = tstep),coef.df$duration[row])
        # Output for each individual row
        rowout <- t %>% 
            # Apply each of the functions rowwise to generate a data frame
            map_df(function(t) mutate(rowwise(coef.df[row,]), 
                      # Time column
                          t=t,
                      # Position column
                          p=c0 + c1*t + c2*t^2 + c3*t^3,
                      # Velocity column
                          v=c1 + 2*c2*t + 3*c3*t^2,
                      # Acceleration
                          a=2*c2 + 6*c3*t))
        # Add row output to the total output
        out <- rbind(out,rowout)
        
        # Update the progressbar
        setTxtProgressBar(pb,row)
    }
    # Close the progressbar
    close(pb)
    
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
        # Rearrange columns to suit my OCD
        select(1:(length(extravars)+4),px,py,pz,vx,vy,vz,vh,v,ax,ay,az,a)
}

#--- Testing
load("atp_serves.RData")
data <- PlottingFactors(atp_serves)
coef.df <- StandardiseCoefficients(data[1:100,],server,start.x, start.y, start.z, center.x, center.y)
values <- PlottingValues(coef.df,server,start.x,start.y,tstep = 0.1)
