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

importancetable <- spread(importancedf, receiver_score, importance) %>%
  rename(`server\receiver` = server_score)


plot_gg <- filter(plot_perserve, server %in% c("FEDERER", "RAONIC") )
ggplot(plot_gg) +
  geom_point(aes(x = center.y, y = speedkmph, colour = importance, shape = side))


plot_gg <- filter(plot_perserve, server %in% c("FEDERER", "RAONIC") )
court_service +
  geom_point(aes(x = center.x, y = center.y, colour = importance),
             data = plot_gg) + 
  scale_colour_gradientn(colours = brewer.pal(7, "Oranges"), 
                         guide = "colourbar",
                         name = "Importance") +
  facet_grid(~server)
