#Mitchell O'Hara-Wild
traj_coords <- function(start, dir, flip, c0, c1, c2, c3, tm=0) {
  tm <- tm + start
  if (dir != "z") {
    c0 <- flip * c0
    c1 <- flip * c1
    c2 <- flip * c2
    c3 <- flip * c3
  }
  pos <- c0 + c1*tm + c2*tm^2 + c3*tm^3
  return(pos)
}

GeneratePoints <- function(data, arc1 = 10, arc3 = 0, wide = FALSE, ...) {
  require(tidyr)
  require(dplyr)
  require(purrr)

  data <- data %>% mutate(duration = arc1 * duration.arc1 + arc3 * duration.arc3,
                          flip = sign(raw.x0.1))

  dots <- c("serveid", sapply(substitute(list(...))[-1], deparse))
  extravars <- match(dots[dots%in%colnames(data)], colnames(data))

  out <- data.frame()

  if (arc1 > 0) {
    arc1p <- data %>%
      select(extravars, start.1, duration.arc1, flip, raw.x0.1:raw.z3.1) %>%
      gather(coef, value, -(1:length(extravars)), -start.1, -duration.arc1, -flip) %>%
      separate(coef, c("junk1", "coef", "junk2"), sep="\\.") %>%
      select(-junk1, -junk2) %>%
      mutate(dir=substr(coef, 1, 1), coef=substr(coef, 2, 2)) %>%
      spread(coef, value) %>%
      rename("start" = `start.1`, "duration"=`duration.arc1`, "c0"=`0`,
             "c1"=`1`, "c2"=`2`, "c3"=`3`)
      arc1points <- seq(0,1,length.out=arc1) %>%
        map_df(function(x) mutate(rowwise(arc1p), time=start + duration * x,
                                  pos=traj_coords(start, dir, flip, c0, c1, c2, c3,
                                                  tm = duration * x))) %>%
        arrange(serveid, pos, time)
      out <- rbind(out, cbind(arc=1, arc1points))
  }

  if (arc3 > 0) {
    arc3p <- data %>% select(extravars, start.3, duration.arc3, flip, raw.x0.3:raw.z3.3) %>%
      gather(coef, value, -(1:length(extravars)), -start.3, -duration.arc3, -flip) %>%
      separate(coef, c("junk1", "coef", "junk2"), sep="\\.") %>%
      select(-junk1, -junk2) %>%
      mutate(dir=substr(coef, 1, 1), coef=substr(coef, 2, 2)) %>%
      spread(coef, value) %>%
      rename("start" = `start.3`, "duration"=`duration.arc3`, "c0"=`0`, "c1"=`1`,
             "c2"=`2`, "c3"=`3`)
    arc3points <- seq(0, 1, length.out=arc3) %>%
      map_df(function(x) mutate(rowwise(arc3p), time = start + duration * x,
                                pos = traj_coords(start, dir, flip, c0, c1, c2, c3,
                                                tm=duration * x))) %>%
      arrange(serveid, pos, time)

    out <- rbind(out, cbind(arc=3, arc3points))
  }

  if (wide) {
    extravars <- match(dots[dots%in%colnames(out)], colnames(out))
    out <- out %>% select(arc, extravars, flip, dir, time, pos) %>%
      spread(dir, pos)
  }

  return(out)
}

#DEMO

courtTrace <- data.frame(x = c(-11.89, -11.89, 0, 0, 0, 0, 0, 0, 11.89, 11.89, -11.89, -11.89, 11.89, 11.89, -11.89, -6.4, -6.4, 6.4, 6.4, 6.4, -6.4),
                         y = c(5.49, -5.49, -5.49, -6.5, -6.5, 6.5, 6.5, -5.49, -5.49, 5.49, 5.49, 4.115, 4.115, -4.115, -4.115, -4.115, 4.115, 4.115, -4.115, 0, 0),
                         z = c(0, 0, 0, 0, 1.09, 1.09, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

load("atp_serves.RData")
sample <- GeneratePoints(atp_serves[1:10,], arc3=10, wide=TRUE)
sample <- GeneratePoints(atp_serves, arc3=10, wide=TRUE)
# Drop out players with too few games, only for looking at players
keep <- atp_serves %>% select(server, matchid) %>%
  distinct() %>% count(server, sort=TRUE) %>% filter(n>2)
atp_serves_sub <- atp_serves %>% filter(server %in% keep$server)
sample <- GeneratePoints(atp_serves_sub, arc3=10, wide=TRUE)

library(ggplot2)
ggplot() + geom_path(data = courtTrace, aes(x = x, y = y), color = 'black', size = 0.5) +
  geom_segment(aes(x= 0, xend= 0, y= -6.5, yend= 6.5), size = 0.5, color = 'darkgrey', lineend = 'round') +
  geom_line(aes(x=x,y=y, group=serveid), data=sample)

ggplot() + geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0), size=0.5) +
  geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='darkgrey') +
  geom_line(aes(x=x, y=z, group=serveid, colour=time), data=sample)

library(plotly)
plot_ly(sample, x=x, y=y, z=z, group=serveid, type="scatter3d", mode="lines") %>%
  add_trace(x=x, y=y, z=z, data=courtTrace, type="scatter3d", mode="lines") %>%
  layout(scene=list(aspectmode="data"))

# Add the serve characteristics
theme_blank <- theme_bw()
theme_blank$line <- element_blank()
#theme_blank$strip.text <- element_blank()
theme_blank$axis.text <- element_blank()
#theme_blank$plot.title <- element_blank()
theme_blank$axis.title <- element_blank()
#theme_blank$panel.border <- element_rect(
#  colour = "grey90", size=1, fill=NA)

all <- merge(sample, atp_serves_sub, by="serveid")
all <- merge(sample, atp_serves, by="serveid")
# Plot x-y
ggplot() +
  geom_path(data = courtTrace, aes(x = x, y = y),
            color = 'grey90', size = 0.5) +
  geom_segment(data = courtTrace, aes(x= 0, xend= 0, y= -6.5, yend= 6.5),
               size = 0.5, color = 'darkgrey', lineend = 'round') +
  geom_line(data=filter(all, scorer!=0), aes(x=x, y=y, group=serveid,
        colour=factor(scorer)), alpha=0.1) +
  facet_wrap(~side) +
  coord_equal() +
  theme_blank +
  theme(legend.position="bottom")

ggplot() +
  geom_path(data = courtTrace, aes(x = x, y = y),
            color = 'grey90', size = 0.5) +
  geom_segment(data = courtTrace, aes(x= 0, xend= 0, y= -6.5, yend= 6.5),
               size = 0.5, color = 'darkgrey', lineend = 'round') +
  geom_line(data=filter(all, scorer!=0), aes(x=x, y=y, group=serveid,
                                             colour=factor(scorer)), alpha=0.1) +
  facet_grid(serve_classification~side) +
  coord_equal() +
  theme_blank +
  theme(legend.position="bottom")

# Plot x-z
ggplot(data=filter(all, scorer!=0)) +
  geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0),
               color='grey90', size=0.5) +
  geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='grey90') +
  geom_line(data=filter(all, scorer!=0), aes(x=x, y=z, group=serveid,
                          colour=factor(scorer)), alpha=0.1) +
  facet_wrap(~side) +
  theme_blank +
  theme(legend.position="bottom")
ggplot(data=filter(all, scorer!=0)) +
  geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0),
               color='grey90', size=0.5) +
  geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='grey90') +
  geom_line(data=filter(all, scorer!=0), aes(x=x, y=z, group=serveid,
                          colour=factor(scorer)), alpha=0.1) +
  facet_grid(serve_classification~side) +
  theme_blank +
  theme(legend.position="bottom")

# Look at a sample of players
keep <- as.character(unique(all$server)[sample(1:33, 5)])
ggplot(data=filter(all, server %in% keep)) +
  geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0),
               color='grey90', size=0.5) +
  geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='grey90') +
  geom_line(data=filter(all, server %in% keep),
            aes(x=x, y=z, group=serveid,
                          colour=scorer), alpha=0.1) +
  facet_grid(server~side) +
  theme_blank +
  theme(legend.position="bottom")

# Summarise trajectories by other variables
sub <- all %>% select(serveid, arc, flip, time, x, y, z, server,
                      receiver, scorer, side, serve_classification) %>%
  arrange(serveid, arc, time)
sub$timeindx <- rep(1:20, 2000)
sub_summary <- sub %>% group_by(side, serve_classification,
                                scorer, timeindx) %>%
  summarise(xm=mean(x, trim=0.1, na.rm=T),
            ym=mean(y, trim=0.1, na.rm=T),
            zm=mean(z, trim=0.1, na.rm=T),
            xs=sd(x, na.rm=T), ys=sd(y, na.rm=T), zs=sd(z, na.rm=T))

ggplot(data=filter(sub_summary, scorer != 0)) +
  geom_path(data = courtTrace, aes(x = x, y = y),
            color = 'grey90', size = 0.5) +
  geom_segment(data = courtTrace, aes(x= 0, xend= 0, y= -6.5, yend= 6.5),
               size = 0.5, color = 'darkgrey', lineend = 'round') +
  geom_line(data=filter(sub_summary, scorer != 0), aes(x=xm, y=ym, group=scorer,
                                             colour=factor(scorer))) +
  facet_grid(serve_classification~side) +
  coord_equal() +
  theme_blank +
  theme(legend.position="bottom")

ggplot(data=filter(sub_summary, scorer != 0)) +
  geom_segment(aes(x=-11.89, xend=11.89, y=0, yend=0),
               color='grey90', size=0.5) +
  geom_segment(aes(x=0,xend=0,y=0,yend=1.07), color='grey90') +
  geom_line(data=filter(sub_summary, scorer != 0),
            aes(x=xm, y=zm, group=scorer,
                colour=factor(scorer))) +
  facet_grid(serve_classification~side) +
  theme_blank +
  theme(legend.position="bottom")
