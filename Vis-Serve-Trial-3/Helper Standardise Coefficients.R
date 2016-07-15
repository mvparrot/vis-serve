#================================================
#--- Generate Standardised Coefficients
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
    # Order the data set in a more logical way
    coef.df <- coef.df[order(coef.df$serveid,coef.df$arc,coef.df$dir),]  
    return(coef.df)
}

#--- Testing
# data <- PlottingFactors(atp_serves)
# coef.df <- StandardiseCoefficients(data[1:10,],server,start.x, start.y, start.z, center.x, center.y)
