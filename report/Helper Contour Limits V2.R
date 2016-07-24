#================================================
#--- Contour Limits
#--- Alwin Wang
#================================================

ContourLimits <- function(plot_perserve, argument1, min, plot=TRUE) {
  #--- Initial factors for the for loops
  # Get a list of the factor 'levels' of argument1
  factor1 <- as.data.frame(plot_perserve %>% ungroup %>%
                             select_(interp(argument1)) %>% distinct())
  # Get the top n players we're intereste in
  factor2 <- MultipleGames(min, perserve = TRUE, server = TRUE)
  # Sides for limits
  factor3 <- c("Ad", "Deuce")
  
  #--- Loading Bar
  b = 0
  pb <- txtProgressBar(min=0, max=nrow(factor1)*nrow(factor2)*2, 
                       initial=0, style=3)
  
  #--- For loop to generate data
  # Initialise an empty dataframe
  contour_lim <- data.frame()
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
                     z = as.vector(out$z))
        # Add plotting factors
        out[[argument1]] = f1
        out[["server"]] = f2
        out[["side"]] = f3
        # Add a unique factor so it can be split later
        out[["unique"]] = paste(f1, f2, f3, sep = "-")
        # Add the iteration result to total result
        contour_lim <- rbind(contour_lim, out)
        
        #--- Update the Progress Bar
        b = b + 1
        setTxtProgressBar(pb,b)
      }
    }
  }
  
  close(pb)
  
  #--- Create the plot variable
  # Split up the results as they need to be indivially plotted in geom_contour
  plot_split <- split(contour_lim, contour_lim$unique)
  # Loop across each of the unividual contour case
  plotall <- court_service
  for (p in 1:length(plot_split)) {
    plotall <- plotall +
      geom_contour(aes(x = center.x, y = center.y, z = z), data = plot_split[[p]])
  }
  
  #--- Plot the output
  if (plot==TRUE) {
    plotall + facet_grid(interp("argument1~server", argument1 = as.name(argument1)))
  }
  else {
    return(plotall)
  }
}

ContourLimits(plot_perserve, argument1 = "serve_num", min = 2, plot = TRUE)