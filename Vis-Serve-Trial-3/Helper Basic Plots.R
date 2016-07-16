#================================================
#--- Basic Plots
#--- Alwin Wang
#================================================
#--- Packages Required
require(ggplot2)
require(plotly)

#--- Outline of the court
court_trace <- data.frame(x = c(-11.89, -11.89, 0, 0, 0, 11.89, 11.89, -11.89, -11.89, 11.89, 11.89, -11.89, -6.4, -6.4, 6.4, 6.4, 6.4, -6.4),
                          y = c(5.49, -5.49, -5.49, 5.49, -5.49, -5.49, 5.49, 5.49, 4.115, 4.115, -4.115, -4.115, -4.115, 4.115, 4.115, -4.115, 0, 0),
                          z = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
net_trace <- data.frame(x = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                        y = c(-5.49,-5.49, -6.4, -6.4, -5.49, 0, 5.49, 6.4, 6.4, 5.49, 5.49),
                        z = c(1.07, 0, 0, 1.07, 1.07, 0.914, 1.07, 1.07, 0, 0, 1.07))

#--- Top down court view
court_topdown <- ggplot() + 
    labs(x = "x direction", y = "y direction") + 
    geom_path(data = court_trace, aes(x = x, y = y), color = 'black', size = 1.5) +
    geom_path(data = net_trace, aes(x = x, y = y), color = 'darkgrey', size=1.5, lineend = 'round') +
    coord_fixed()

#--- Side on court view
court_sideon <- ggplot() + 
    labs(x = "x direction", y = "z direction") + 
    geom_path(data = court_trace, aes(x = x, y = z), color = 'black', size = 1.5) +
    geom_path(data = net_trace, aes(x = x, y = z), color = 'darkgrey', size=1.5, lineend = 'round') +
    coord_fixed()

#--- Behind court view
court_behind <- ggplot() + 
    labs(x = "y direction", y = "z direction") + 
    geom_path(data = net_trace, aes(x = y, y = z), color = 'darkgrey', size = 1.5) +
    geom_path(data = court_trace, aes(x = y, y = z), color = 'black', size = 1.5) +
    coord_fixed()

plot_ly(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=net_trace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))