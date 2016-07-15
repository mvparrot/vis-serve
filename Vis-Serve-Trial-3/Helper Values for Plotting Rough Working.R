# Initialise empty data frame
out <- data.frame()

# Create a prgressbar to make sure my code is working!!
pb <- txtProgressBar(min = 0, max = nrow(coef.df), style = 3)


testFunc <- function(a,b) a + b
dat <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6))
apply(dat[,c('x','z')], 1, function(y) testFunc(y['z'],y['x']))


RowFunc <- function(duration,tstep) {
    t <- c(seq(from = 0, to = duration, by = tstep), duration)
}

apply(coef.df[,c("duration")], 1, function(x) RowFunc(x["duration"],0.1)) # NOT WORKING

# From googling "explanding sequence in data frame" and
# Learning how to change ddply into the newer dplyr package
# ddply(presidents, "name", summarise, year = seq(from,to))

# Sample Data frame
# L3 <- LETTERS[1:3]
# fac <- sample(L3, 10, replace = TRUE)
# (d <- data.frame(x = 1, y = 1:10, fac = fac))
# d %>% rowwise() %>% do(data.frame(y=.$y,time=seq(.$x,.$y)))
# to keep ALL other columns
# d %>% group_by(fac) %>% rowwise %>% do(data.frame(., time = seq(.$x, .$y)))

coef.exp <- coef.df %>% 
    group_by(serveid) %>% 
    rowwise() %>% 
    do(data.frame(., t = c(seq(0,.$duration,by=0.1),.$duration))) %>%
    mutate(p=c0 + c1*t + c2*t^2 + c3*t^3, v=c1 + 2*c2*t + 3*c3*t^2, a=2*c2 + 6*c3*t) 

# Clear up the data into a more manageable form for plotting
coef.exp <- coef.exp %>%
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

# Test:
coef.exp[,6:17] - values[,8:19]

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