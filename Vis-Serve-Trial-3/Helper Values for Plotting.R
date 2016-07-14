#================================================
#--- Values for Plotting
#--- Alwin Wang
#================================================
# Packages Required
require(tidyr)
require(dplyr)
require(purrr)

    # Finds the coefficients that weren't flipped
    data <- data %>% mutate(flip=sign(raw.x0.1))
    # Selects the serveid and the additional columns parsed in for the function
        # dots <- c("serveid", sapply(substitute(list(...))[-1], deparse))
    dots <- c("serveid", sapply(substitute(list(server))[-1], deparse))
    # Gets the column numbers of the "serveid" and "..." columns
    extravars <- match(dots[dots%in%colnames(data)], colnames(data))
    
    # Empty data frame
    out <- data.frame()
    
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
        # Remove "raw" column as it has no information
        select(-raw) %>% 
        