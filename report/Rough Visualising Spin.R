#================================================
#--- Visualising Spin
#--- Alwin Wang
#================================================
#--- Proper method
SpinVisual <- function(w0,wx,theta) {
    visual <- data.frame(bottom = seq(-pi/2,pi/2, length.out = 51), 
                  top = seq(pi/2,3*pi/2, length.out = 51)) %>%
    gather(key,t) %>%
    mutate(x = wx*cos(t) * cos(theta) - w0*sin(t) * sin(theta),
           y = wx*cos(t) * sin(theta) + w0*sin(t) * cos(theta))
    visual$arrow <-  c(rep("a0",52),
                          rep("a1",8),rep("a2",8),rep("a3",8),
                          rep("a4",8),rep("a5",8),rep("a6",8),
                          rep("a7",2))
    return(visual)
}

court_ball <- data.frame(t = seq(0,2*pi,length.out=51)) %>%
  mutate(x = r*100*cos(t), y = r*100*sin(t)) %>%
  select(-t)



# geom_path has an arrow option!! path follows the order, line follows the x axis

ggplot() + geom_path(data=asdf, aes(x=x, y=y)) + coord_fixed()

textvisual <- plot_perserve %>% select(serveid, w0_arc1, wx_arc1, wy_arc1,wz_arc1)

# Max RPM around 500 approx 5000
hist(textvisual$w0_arc1)
hist(textvisual$wx_arc1)
hist(textvisual$wy_arc1)
hist(textvisual$wz_arc1)
hist(atan(textvisual$wz_arc1/textvisual$wy_arc1))


plot_gg <- plot_perserve %>% filter(w_rpm_arc1 < 5500)
ggplot(plot_gg, aes(w_rpm_arc1, colour=serve_num)) + 
  geom_density() + 
  facet_wrap(~serve_classname)

plot_multiple_games <- MultipleGames(4)
plot_gg <- plot_perserve %>% filter(server %in% plot_multiple_games$server,
                                    w_rpm_arc1 < 5500)
plot_gg <- plot_gg %>% filter(w_rpm_arc1 < 5500)
ggplot(plot_gg, aes(w_rpm_arc1, colour=serve_num)) + 
  geom_density() + 
  facet_grid(side ~ server, scales = "free_y")


plot_multiple_games <- MultipleGames(4)
plot_gg <- plot_perserve %>% filter(server %in% plot_multiple_games$server,
                                    w_rpm_arc1 < 5500)
plot_gg <- plot_gg %>% filter(w_rpm_arc1 < 5500)
ggplot(plot_gg, aes(speedkmph, colour=serve_num)) + 
  geom_density() + 
  facet_grid(side ~ server, scales = "free_y")



visual <- SpinVisual(5,-2,1)
ggplot() +
  geom_path(data = filter(visual, key == "bottom"),
            aes(x = x, y = y),
                colour="blue", linetype=2,
                arrow=arrow(angle=15,type="closed")) +
  geom_polygon(data = court_ball,
            aes(x = x, y = y), fill="green") +
  geom_jitter(data = court_ball,
            aes(x = x, y = y), colour="green") + 
  geom_path(data = filter(visual, key == "top"), 
            aes(x = x, y = y), 
                colour="red", linetype=1,
                arrow=arrow(angle=15,type="closed")) +
  geom_line(data = filter(visual, arrow=="a2"),
            aes(x = x, y = y), linetype = 0,
            arrow=arrow(angle=15,type="closed")) +
  geom_line(data = filter(visual, arrow=="a3"),
            aes(x = x, y = y), linetype = 0,
            arrow=arrow(angle=15,type="closed")) +
  geom_line(data = filter(visual, arrow=="a4"),
            aes(x = x, y = y), linetype = 0,
            arrow=arrow(angle=15,type="closed")) +
  geom_line(data = filter(visual, arrow=="a5"),
            aes(x = x, y = y), linetype = 0,
            arrow=arrow(angle=15,type="closed")) +
  coord_fixed()


ggplot() +
  geom_path(data = visual, 
            aes(x = x, y = y, 
                colour=key)) +
  coord_fixed()
