#================================================
#--- Contour Limits
#--- Alwin Wang
#================================================

# RoundDensity <- function(density) {
#   if (density >= 0.01) {density}
#   else {0}
# }

HeatMap <- function(plot_perserve, argument1, min_games, plot=TRUE, progress=FALSE) {
  #--- Initial factors for the for loops
  # Get a list of the factor 'levels' of argument1
  factor1 <- as.data.frame(plot_perserve %>% ungroup %>%
                             select_(interp(argument1)) %>% distinct())
  # List of interested servers
  if (is.numeric(min_games)) {
    # Get the top n players we're interested in
    factor2 <- MultipleGames(min_games, perserve = TRUE, server = TRUE)
  }
  else {
    factor2 <- min_games
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
          filter(server %in% f2, side %in% f3)
        # Impose limits if its on the Ad side
        if (f3 == "Ad") {
          out <- kde2d(out$center.x,
                       out$center.y,
                       lims = c(-6.4, 0, 0, 4.115))
        }
        # Impose limits if it's on the Deuce side
        if (f3 == "Deuce") {
          out <- kde2d(out$center.x,
                       out$center.y,
                       lims = c(-6.4, 0,-4.115, 0))
        }
        # Expand it so it can be plotted later
        out <-
          data.frame(expand.grid(center.x = out$x, center.y = out$y),
                     density = as.vector(out$z)) %>%
          # Round the v small values so they aren't plotted
          filter(density >= 0.025)
        # Add plotting factors
        out[[argument1]] = f1
        out[["server"]] = f2
        out[["side"]] = f3
        
        # Add the iteration result to the output plot
        plotall <- plotall +
          # geom_point(aes(x = center.x, y = center.y, alpha = density*4), data=out)
          geom_point(aes(x = center.x, y = center.y, 
                         size = density, colour = density),
                    data=out)
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
    plotall + facet_grid(interp("argument1~server", argument1 = as.name(argument1)))
  }
  else {
    return(plotall)
  }
}

#--- Tests
HeatMap(plot_perserve, argument1 = "serve_num", min_games = 4, plot = TRUE)  +
    scale_size_continuous(range = c(0,2), breaks=seq(0,0.2,by=0.02))
  #  scale_size_area(max_size = 1.5)
# ContourLimits(filter(plot_perserve, serve_classname != "Fault"), 
#               argument1 = "serve_classname", min_games = 4, plot = TRUE)
# 
stat1 <- ggplot() +
        stat_summary_2d(data = plot_gg, aes(x=center.x, y=center.y, z=speedkmph),
                        bins = 25,
                        fun = function(z) mean(z),alpha = 0.7)
stat1 <- ggplot_build(stat1)
asdf1 <- as.data.frame(stat1[[1]]) # %>% select(x, y, value)
names(asdf1)


stat2 <- ggplot() +
  stat_bin_2d(data = plot_gg, aes(x=center.x, y=center.y),
                  bins = 25, alpha = 0.7)
stat2 <- ggplot_build(stat2)
asdf2 <- as.data.frame(stat2[[1]]) # %>% select(x, y, value)
names(asdf2)
