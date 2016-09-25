#================================================
#--- Generate meaningful names and values
#--- Alwin Wang
#================================================


importancedf <- data.frame(
  server_score = c(rep(c("00", "15", "30", "40"), each = 4), 40, "AD"),
  receiver_score = c(rep(c("00", "15", "30", "40"), times = 4), "AD", 40),
  importance = c(0.25, 0.34, 0.38, 0.28,
                 0.19, 0.31, 0.45, 0.45,
                 0.11, 0.23, 0.45, 0.73,
                 0.04, 0.10, 0.27, 0.45,
                 0.73, 0.27),
  stringsAsFactors = FALSE
)

spread(importancedf, receiver_score, importance)

PlottingFactors <- function(data) {
    # Add in speed in kmph
    data$speedkmph <- data$speed * 3.6
    # Add a speed classification
    data$speed_class <- cut(data$speedkmph,c(100,120,140,160,180,200,220,240,260))
    # Make a factor for first and second serves (will help with plotting)
    data$serve_num <- factor(data$serve.x, levels = 1:2, label = c("First Serve","Second Serve"))
    # Make a factor for serve classification (will help with plotting)
    data$serve_classname <- factor(data$serve_classification, levels= 0:3, label=c("Ace", "Returned", "Not Returned","Fault"))
    # Make a factor for scorer of the point (will help with plotting)
    data$scorername <- factor(data$scorer, levels= -1:1, label=c("Receiver", "No Winner", "Server"))
    
    # Make a factor for the importance
    data$server_score <- as.character(data$server_score)
    data$receiver_score <- as.character(data$receiver_score)
    data <- merge(data, importancedf, by = c("server_score", "receiver_score")) %>%
      arrange(matchid, serveid) %>%
      .[c(colnames(data), "importance")]
    data$server_score <- as.factor(data$server_score)
    data$receiver_score <- as.factor(data$receiver_score)
    # Output the data with additional columns
    return(data)
}