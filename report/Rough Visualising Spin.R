#================================================
#--- Visualising Spin
#--- Alwin Wang
#================================================
#--- Proper method
visual <- function(w0,wx,theta) {
    data.frame(top = seq(-pi,pi, length.out = 50), 
                     bottom = seq(pi,-pi, length.out = 50)) %>%
    gather(key,t) %>%
    mutate(x = wx*cos(t) * cos(theta) - w0*sin(t) * sin(theta),
           y = wx*cos(t) * sin(theta) + w0*sin(t) * cos(theta))
}

asdf <- visual(20,10,pi/4)

ggplot() + geom_path(data=asdf, aes(x=x, y=y)) + coord_fixed()

textvisual <- plot_perserve %>% select(serveid, w0_arc1, wx_arc1, wy_arc1,wz_arc1)

# Max RPM around 500 approx 5000
hist(textvisual$w0_arc1)
hist(textvisual$wx_arc1)
hist(textvisual$wy_arc1)
hist(textvisual$wz_arc1)



ggplot(data = visual, aes(x=x, y=y)) +
    geom_path() + 
    coord_fixed()



