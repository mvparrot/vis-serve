#================================================
#--- Generate Values for Plotting
#--- Alwin Wang
#================================================
#--- Packages Required
require(tidyr)
require(dplyr)
require(purrr)

StandardiseCoefficients <- function(data, ..., plzwork=TRUE) {
    # Finds the coefficients that weren't flipped
    data <- data %>% mutate(flip=sign(raw.x0.1))
    # Selects the serveid and the additional columns parsed in for the function
    dots <- c("serveid", sapply(substitute(list(...))[-1], deparse))
    # Gets the column numbers of the "serveid" and "..." columns
    extravars <- match(dots[dots%in%colnames(data)], colnames(data))
    
    # Generate a data frame of interested columns and standardised coefficients
    coef.df <- data %>% 
        # Select only interested columns, important times, flip and raw coefficients
        select(extravars, start.1, start.3, duration.arc1, duration.arc3, flip, raw.x0.1:raw.z3.3) %>%
        # Gather coef names into "coef" and their values into "value". Preserve all others
        #   1:length(extravars) are the extravars, the next 5 columns are start.1&3, duration.arc1&3, flip
        gather(coef, value, -(1:(length(extravars) + 5))) %>%
        # separates coef names into three separate ones
        #   e.g. raw.x0.1 --> raw   x0   1
        separate(coef, c("raw", "coef", "arc"), sep="\\.") %>%
        # Make arc number a number format
        mutate(arc = as.numeric(arc)) %>%
        # Put the first character in "dir" and second character in "coef"
        mutate(dir=substr(coef, 1, 1), coef=substr(coef, 2, 2)) %>%
        # Spread the "values" by the "coef"
        spread(coef, value) %>%
        # Create a start time and duration time
        mutate(start = start.1*(arc == 1) + start.3*(arc == 3), duration = duration.arc1*(arc == 1) + duration.arc3*(arc == 3)) %>%
        # Create standarised coefficients by explanding `a0 + a1(t-s) + a2(t-s)^2 + a3(t-s)^3`
        mutate(c0 = `0` + `1`*start + `2`*start^2 + `3`*start^3,
               c1 = `1` + 2*`2`*start + 3*`3`*start^2,
               c2 = `2` + 3*`3`*start,
               c3 = `3`) %>%
        # Account for if the coefficients need to be flipped
        mutate(c0 = c0*as.numeric(dir != "z")*flip + c0*as.numeric(dir == "z"),
               c1 = c1*as.numeric(dir != "z")*flip + c1*as.numeric(dir == "z"),
               c2 = c2*as.numeric(dir != "z")*flip + c2*as.numeric(dir == "z"),
               c3 = c3*as.numeric(dir != "z")*flip + c3*as.numeric(dir == "z")) %>%
        # Standardise start time
        mutate(start = (start.3-start.1)*(arc == 3)) %>%
        # Select only the columns that are important
        select(1:length(extravars), arc, start, duration, dir, c0:c3)
    return(coef.df)
}

PlottingValues <- function(coef.df, ..., tstep = 0.05) {
    # Selects the serveid and the additional columns parsed in for the function
    dots <- c("serveid", sapply(substitute(list(...))[-1], deparse))
    # Gets the column numbers of the "serveid" and "..." columns
    extravars <- match(dots[dots%in%colnames(coef.df)], colnames(coef.df))
    
#--- Original slow code   
#     # Initialise empty data frame
#     out <- data.frame()
#     
#     # Create a prgressbar to make sure my code is working!!
#     pb <- txtProgressBar(min = 0, max = nrow(coef.df), style = 3)
#     
#     # Generate values for each row
#     for (row in 1:nrow(coef.df)) {
#         # Generate a time vector of given step size AND end time
#         t <- c(seq(from = 0, to = coef.df$duration[row], by = tstep),coef.df$duration[row])
#         # Output for each individual row
#         rowout <- t %>% 
#             # Apply each of the functions rowwise to generate a data frame
#             map_df(function(t) mutate(rowwise(coef.df[row,]), 
#                                       # Time column
#                                       t=t,
#                                       # Position column
#                                       p=c0 + c1*t + c2*t^2 + c3*t^3,
#                                       # Velocity column
#                                       v=c1 + 2*c2*t + 3*c3*t^2,
#                                       # Acceleration
#                                       a=2*c2 + 6*c3*t))
#         # Add row output to the total output
#         out <- rbind(out,rowout)
#         
#         # Update the progressbar
#         setTxtProgressBar(pb,row)
#     }
#     # Close the progressbar
#     close(pb)
    
#--- FASTTT Code
    # Calculate the position, velocity and acceleration at any point in time
    out <- coef.df %>% 
        # Group data by serve id
        group_by(serveid) %>% 
        # Perform the operatoin by rows (important!)
        rowwise() %>% 
        # Create individual sequences for arcs. These will be filled out rowwise
        do(data.frame(., t = c(seq(0,.$duration,by=0.1),.$duration))) %>%
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
        # Rearrange columns to suit my OCD
        select(1:(length(extravars)+4),px,py,pz,vx,vy,vz,vh,v,ax,ay,az,a)
}

#--- Testing
load("atp_serves.RData")
data <- atp_serves[1:1000,]
# testing on smaller samples are okay. Larger ones are nowhere near as fast when using my for loop.
coef.df <- StandardiseCoefficients(data,server,start.x, start.y, start.z, center.x, center.y)
values <- PlottingValues(coef.df,server,start.x,start.y,tstep = 0.1)