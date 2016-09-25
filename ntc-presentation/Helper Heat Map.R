#================================================
#--- Contour Limits
#--- Alwin Wang
#================================================
HeatMap <- function(plot_perserve, argument1="serve_num", 
                    argument2="server", min_games="4", 
                    argument3="speedkmph",
                    bins=15, plot=TRUE, progress=FALSE) {
  #--- Initial factors for the for loops
  # Get a list of the factor 'levels' of argument1
  factor1 <- as.data.frame(plot_perserve %>% ungroup %>%
                             select_(interp(argument1)) %>% distinct())
  # List of interested servers
  if (argument2=="server") {
    if (is.numeric(min_games)) {
      # Get the top n players we're interested in
      factor2 <- MultipleGames(min_games, perserve = TRUE, server = TRUE)
    } else {
      factor2 <- min_games
    }
  } else {
    factor2 <- as.data.frame(plot_perserve %>% ungroup %>%
                               select_(interp(argument2)) %>% distinct())
  }
  # Sides for limits
  factor3 <- c("Ad", "Deuce")
  
  #--- Loading Bar
  b = 0
  if (progress == TRUE) {
    pb <- txtProgressBar(min=0, max=nrow(factor1)*nrow(factor2)*2, 
                         initial=0, style=3)
  }
  
  #--- For loop to generate data
  # Initialise an empty dataframe
  plotall <- court_service
  # For each factor of the passed in argument
  for (i in 1:nrow(factor1)) {
    # For each factor of server (can be generalised later)
    for (j in 1:nrow(factor2)) {
      # For each side
      for (s in 1:2) {
        # Set the current iternation factors as f1, f2, f3
        f1 = factor1[i, 1]
        f2 = factor2[j, 1]
        f3 = factor3[s]
        # Filter the data set for the factors
        out <- plot_perserve %>%
          filter_(interp(quote(x %in% y),
                         x = as.name(argument1),
                         y = f1)) %>%
          filter_(interp(quote(x %in% y),
                         x = as.name(argument2),
                         y = f2)) %>%
          filter(side %in% f3)
        
        # Skip if out has zero length (i.e error)
        if (nrow(out) == 0) {
          b = b + 1
          if (progress == TRUE) {
            setTxtProgressBar(pb,b)
          }
          next
        }
          
        out[["z"]] = out[[argument3]]
        stat1 <- ggplot_build(ggplot() +
          stat_summary_2d(data = out, aes(x=center.x, y=center.y, z=z),
                          bins = bins,
                          fun = function(z) mean(z)))
        stat1 <- as.data.frame(stat1[[1]]) %>% 
          select(xbin, ybin, x, y, value)
        # Count and density per bin
        stat2 <- ggplot_build(ggplot() +
          stat_bin_2d(data = out, aes(x=center.x, y=center.y),
                      bins = bins))
        stat2 <- as.data.frame(stat2[[1]]) %>% 
          select(xbin, ybin, x, y, count, density)
        # Join them together
        out <-
          left_join(stat1, stat2, by=c("xbin", "ybin", "x", "y"))
        # Add plotting factors
        out[[argument1]] = f1
        out[[argument2]] = f2
        out[["side"]] = f3
        
        # Add the iteration result to the output plot
        plotall <- plotall +
          geom_point(data=out, aes(x=x, y=y, size=count, colour=value))
        #--- Update the Progress Bar
        b = b + 1
        if (progress == TRUE) {
          setTxtProgressBar(pb,b)
        }
      }
    }
  }
  
  if (progress == TRUE) {
    close(pb)
  }
  
  #--- Plot the output
  if (plot==TRUE) {
    plotall + facet_grid(interp("argument1~argument2", 
                                argument1 = as.name(argument1),
                                argument2 = as.name(argument2))) +
      scale_size_continuous(range = c(1,4.5), breaks = seq(1,4.5,by=0.5))
  }
  else {
    return(plotall)
  }
}

#--- Tests
# test1 <- HeatMap(plot_perserve, argument1 = "serve_num", argument2 = "serve_classname",
#                  bins=10, plot = FALSE, progress = TRUE)
# test1 + scale_size_continuous(range = c(0.5,4))
# 
# test2 <- HeatMap(plot_perserve, argument1 = "serve_num", min_games = 4,
#                  bins=5, plot = TRUE, progress = TRUE)
# test2 + scale_size_continuous(range = c(0.5,4))
# 
# HeatMap(plot_perserve, argument1 = "serve_num",
#          argument2 = "serve_classname", argument3 = "speedkmph",
#          bins=10, plot=TRUE, progress=TRUE)
# 
# HeatMap(plot_perserve, argument1 = "serve_num", 
#         argument2 = "serve_classname", argument3 = "start.z",
#         bins=10, plot=TRUE, progress=TRUE)
# plot_gg <- plot_perserve %>% 
#   filter(serve_classname != "Fault", serve_num == "First Serve")
# HeatMap(plot_gg, argument1 = "serve_num", 
#         argument2 = "side", argument3 = "speedkmph",
#         bins=10, plot=TRUE, progress=TRUE)
