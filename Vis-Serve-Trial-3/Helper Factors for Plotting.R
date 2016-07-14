#================================================
#--- Generate meaningful names and values
#--- Alwin Wang
#================================================

PlottingFactors <- function(data) {
    # Add in speed in kmph
    data$speedkmph <- data$speed * 3.6
    # Make a factor for first and second serves (will help with plotting)
    data$serve_num <- factor(data$serve.x, levels = 1:2, label = c("First Serve","Second Serve"))
    # Make a factor for serve classification (will help with plotting)
    data$serve_classname <- factor(data$serve_classification, levels= 0:3, label=c("Ace", "Returned", "Not Returned","Fault"))
    # Make a factor for scorer of the point (will help with plotting)
    data$scorername <- factor(data$scorer, levels= -1:1, label=c("Receiver", "No Winner", "Server"))
    # Output the data with additional columns
    return(data)
}

#--- Testing
# data <- PlottingFactors(atp_serves)