factor1 <- as.data.frame(levels(plot_perserve$serve_num))
factor2 <- MultipleGames(4,perserve = TRUE, server = TRUE)
factor3 <- c("Ad", "Deuce")

contour_lim <- data.frame()
for (i in 1:nrow(factor1)) {
  for (j in 1:nrow(factor2)) {
    for (s in 1:2) {
      f1 = factor1[i,1]; f2 = factor2[j,1]; f3 = factor3[s]
      out <- plot_perserve %>%
        filter(serve_num %in% f1, server %in% f2, side %in% f3)
      if (f3 == "Ad") {
        out <- kde2d(out$center.x, out$center.y,
                     lims = c(-6.4, 0, 0, 4.115))
      }
      if (f3 == "Deuce") {
        out <- kde2d(out$center.x, out$center.y,
                     lims = c(-6.4, 0, -4.115, 0))
      }
      out <- data.frame(expand.grid(center.x = out$x, center.y = out$y),
                        z = as.vector(out$z)) %>%
        mutate(serve_num = f1, server = f2, side = f3)
      contour_lim <- rbind(contour_lim, out)
    }
  }
}
contour_lim <- contour_lim %>% 
  distinct(.keep_all = TRUE) %>% # I get duplicates for some reason
  mutate(unique = paste(serve_num, server, side,sep = "."))

plot_split <- split(contour_lim, contour_lim$unique)

plotall <- court_service 
for (p in 1:length(plot_split)) {
  plotall <- plotall + 
    geom_contour(aes(x=center.x, y=center.y,z=z), data = plot_split[[p]])
}
plotall + facet_grid(serve_num~server)
