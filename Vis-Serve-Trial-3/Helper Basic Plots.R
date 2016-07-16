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
axis_labels <- data.frame(x.break = c(-21.89:-11.89, -6.4, 0, 6.4, 11.89),
                          x.label = c("-10m","-9m","-8m","-7m","-6m","-5m","-4m","-3m","-2m","-1m",
                                      "Baseline","Service Line","Net","Service Line","Baseline"),
                          y.break = c(-5.49,-4.115,0,4.115,5.49),
                          y.label = c("Doubles", "Singles","Centre","Singles","Doubles"),
                          z.break = c(0,0.992,2,3,4),
                          z.label = c("Ground", "Net", "2m", "3m", "4m"))

#--- Top down court view
court_topdown <- ggplot() + 
    labs(x = "x direction", y = "y direction") + 
    scale_x_continuous(breaks = axis_labels$x.break,
                       labels = axis_labels$x.label) +
    scale_y_continuous(breaks = axis_labels$y.break,
                       labels = axis_labels$y.label) +
    geom_path(data = court_trace, aes(x = x, y = y), color = 'black', size = 1.5, alpha = 0.75) +
    geom_path(data = net_trace, aes(x = x, y = y), color = 'blue', size = 1.5, lineend = 'round') +
    coord_fixed()

#--- Side on court view
court_sideon <- ggplot() + 
    labs(x = "x direction", y = "z direction") + 
    scale_x_continuous(breaks = axis_labels$x.break,
                       labels = axis_labels$x.label) +
    scale_y_continuous(breaks = axis_labels$z.break,
                       labels = axis_labels$z.label) +
    geom_path(data = court_trace, aes(x = x, y = z), color = 'black', size = 1.5) +
    geom_path(data = net_trace, aes(x = x, y = z), color = 'blue', size=1.5, lineend = 'round') +
    coord_fixed()

#--- Behind court view
court_behind <- ggplot() + 
    scale_x_continuous(breaks = axis_labels$y.break,
                       labels = axis_labels$y.label) +
    scale_y_continuous(breaks = axis_labels$z.break,
                       labels = axis_labels$z.label) +
    labs(x = "y direction", y = "z direction") +
    geom_path(data = court_trace, aes(x = y, y = z), color = 'black', size = 1.5) +
    geom_path(data = net_trace, aes(x = y, y = z), color = 'blue', size = 1.0) +
    coord_fixed()

#--- 3D court view
court_3d <- plot_ly(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=net_trace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))