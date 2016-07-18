#================================================
#--- Tentative Plotting
#--- Alwin Wang
#================================================
#--- Packages Required
require(tidyr)
require(dplyr)
require(purrr)
require(broom)
require(ggplot2)
require(plotly)
require(hextri)

#--- Calling initial functions
data <- PlottingFactors(atp_serves)
coef.df <- StandardiseCoefficients(data,matchid,server,speedkmph,speed_class,serve_num,serve_classname,side,scorername,
                                   start.x, start.y, start.z, center.x, center.y)
values <- PlottingValues(coef.df,tstep = 0.08)
resultstidy <-  SpinModel(values)

#--- Remove identified outliers
# Serveid of identified outliers
serve_outliers <- data.frame(serveid = c(
    "1_09_02_1_191947.trj", "2_04_01_1_172646.trj", "2_06_02_1_214102.trj",
    "3_01_03_2_181341.trj", "3_04_03_1_205313.trj", "3_06_08_2_174140.trj",
    "4_02_02_1_180400.trj", "4_03_05_1_190717.trj", "4_05_01_1_213913.trj"))
# Filter out outliers
plot_sample <- resultstidy %>% 
    filter(!(serveid %in% serve_outliers$serveid))
plot_sample$speed_class <- cut(plot_sample$speedkmph,c(100,120,140,160,180,200,220,240,260))

# Data per serve
plot_perserve <- plot_sample  %>% 
    select(-(t:a), -(ax_p:az_p)) %>%
    group_by(serveid, arc) %>%
    mutate(Cd_ave = mean(Cd), Cl_ave = mean(Cl)) %>%
    distinct(.keep_all = TRUE) %>%
    gather(key, value, -(1:(match("arc", colnames(.))))) %>%
    unite(key, c(key,arc), sep="_arc") %>%
    spread(key, value) %>%
    distinct(.keep_all = TRUE)
plot_perserve$speed_class <- cut(plot_perserve$speedkmph,c(100,120,140,160,180,200,220,240,260))

#--- Select players with multiple games
MultipleGames <- function(min) {
    multiple_games <- atp_serves %>% 
        select(server, matchid) %>% distinct() %>% 
        count(server, sort=TRUE) %>% filter(n >= min)
    plot_sample %>%
        filter(server %in% multiple_games$server)
}
# Filter by more than two games
plot_multiple_games <- MultipleGames(2)

#--- Plot all serves
# Top Down
plot_gg <- plot_sample
court_topdown + geom_path(aes(x=px,y=py, group=serveid, colour = server), data=plot_gg, alpha = 0.25)
# Side On
court_sideon + geom_path(aes(x=px,y=pz, group=serveid, colour = server), data=plot_gg, alpha = 0.25)
# From Behind
court_behind + geom_path(aes(x=py,y=pz, group=serveid, colour = server), data=plot_gg, alpha = 0.25)
# In 3D
#--- Plot the court in 3D
plot_ly(plot_gg, x=px, y=py, z=pz, group=serveid, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=net_trace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))

#--- Plot top modeled serves
BestFit <- function(resultstidy) {
    serve_best_fit <- resultstidy %>% 
        filter(serve_classname != "Fault") %>%
        select(serveid,arc,usefulness) %>%
        distinct(.keep_all = TRUE) %>%
        group_by(serveid) %>%
        filter(!is.na(sum(usefulness))) %>%
        mutate(overall = prod(usefulness)) %>% # if I use .$usefulness, it becomes everything!
        filter(overall == max(.$overall,na.rm = TRUE)) %>% 
        distinct()
    return(serve_best_fit$serveid)
}

resultstidy %>% filter(side == "Ad", px < 0, py <0, !is.na(sum(usefulness))) %>% 
    select(serveid,arc,px,py,pz,usefulness) %>%
    group_by(serveid) %>%
    mutate(overall = prod(usefulness)) %>%
    filter(overall > 0.8) %>% 
    distinct(overall) %>%
    head(arrange(-overall), n = 20)

BestFits <- function(resultstidy, num = 2000) {
    serve_best_fits <- resultstidy %>% 
        filter(serve_classname != "Fault") %>%
        filter(side == "Ad" & center.y < 1) %>% 
#        filter(center.y > -2 & center.y <2) %>%
        select(serveid,arc,usefulness) %>%
        distinct(.keep_all = TRUE) %>%
        group_by(serveid) %>%
        filter(!is.na(sum(usefulness))) %>%
        mutate(overall = prod(usefulness)) %>% # if I use .$usefulness, it becomes everything!
        distinct(overall) %>%
        arrange(-overall) %>%
        head(., n = num)
    return(serve_best_fits)
}

bests <- BestFits(plot_sample,10)
plot_gg <- plot_sample %>% filter(serveid %in% bests$serveid)
# Top Down
court_topdown + geom_path(aes(x=px,y=py, group=serveid, colour = serveid), data=plot_gg, alpha = 1)
# Side On
court_sideon + geom_path(aes(x=px,y=pz, group=serveid, colour = server), data=plot_gg, alpha = 1)
# From Behind
court_behind + geom_path(aes(x=py,y=pz, group=serveid, colour = server), data=plot_gg, alpha = 1)
# In 3D
#--- Plot the court in 3D
plot_ly(plot_gg, x=px, y=py, z=pz, group=serveid, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=net_trace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))

#--- Plot single serve
serve_coolarc <- c("5_02_02_1_190119.trj","3_01_02_1_171036.trj")
plot_gg <- plot_sample %>% filter(serveid %in% serve_coolarc)
asdf <- plot_gg %>% select(-(start:az_p)) %>% distinct(.keep_all = TRUE)
# Top Down
court_topdown + geom_path(aes(x=px,y=py, group=serveid, colour = server), data=plot_gg, alpha = 1)
# Side On
court_sideon + geom_path(aes(x=px,y=pz, group=serveid, colour = server), data=plot_gg, alpha = 1)
# From Behind
court_behind + geom_path(aes(x=py,y=pz, group=serveid, colour = server), data=plot_gg, alpha = 1) + 
    geom_path(data = net_trace, aes(x = y, y = z), color = 'blue', size = 1.0)
# In 3D
#--- Plot the court in 3D
plot_ly(plot_gg, x=px, y=py, z=pz, group=serveid, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=net_trace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))

#--- Hextri Plots
# Landing position coloured by scorer
hextri(x=plot_perserve$center.x, y=plot_perserve$center.y,
       class=plot_perserve$scorername, colours=c("red","orange","green"), nbins = 20, border=TRUE,style="size")
legend("topleft",fill=c("green","orange","red"),
       legend=c("Server", "No Winner", "Receiver"),bty="n")

# Alternate form
rval<-hextri(x=plot_perserve$center.x, y=plot_perserve$center.y,
             class=plot_perserve$scorername, colours=c("red","orange","green"), nbins = 20, border=TRUE,style="size")
plot(y~x,data=rval,type="n")
with(rval, polygon(x,y,col=col,border=NA))
legend("topleft",fill=c("green","orange","red"),
       legend=c("Server", "No Winner", "Receiver"),bty="n")

# Landing position coloured by classification
hextri(x=plot_perserve$center.x, y=plot_perserve$center.y,
       class=plot_perserve$serve_classname, 
       colours=c("green","orange","yellow","red"), nbins = 20, border=TRUE,style="size")
legend("topleft",fill=c("green","yellow","orange","red"),
       legend=c("Ace","Not Returned","In Play","Fault"),bty="n")

# All positions coloured by first or second serve
hextri(x=plot_sample$px, y=plot_sample$py,
       class=plot_sample$serve_num, 
       colours=c("blue","orange","green","red"), nbins = 25, border=TRUE,style="size")

# All positions coloured by speed class
hextri(x=plot_sample$px, y=plot_sample$py,
       class=plot_sample$speed_class, 
       colours=c("grey10","grey20","grey30","grey40","grey50","grey60","grey70","grey80"), nbins = 25, border=TRUE,style="size")
legend("topleft",fill=1:10,
       legend=1:10,bty="n")

#--- Speed investigation
# Played at least 2 games servers
plot_multiple_games <- MultipleGames(2)
plot_gg <- plot_sample %>% filter(server %in% plot_multiple_games$server)
# First vs second serve
ggplot(data = plot_gg, aes(speedkmph, fill=factor(serve_num))) + 
    geom_density(alpha = 0.2) + facet_wrap(~serve_num)

# First vs second serve broken by type
ggplot(data = plot_gg, aes(speedkmph, fill=scorername)) + 
    geom_density(alpha = 0.2) + facet_wrap(~serve_num)

ggplot(data = plot_gg, aes(speedkmph, fill=factor(speed_class))) + 
    geom_density(alpha = 0.2)+ facet_wrap(~serve_classname)

ggplot(data = plot_gg, aes(speedkmph, fill=factor(side))) + 
    geom_density(alpha = 0.2)+ facet_wrap(~server)

ggplot(data = plot_gg, aes(speedkmph, fill=factor(serve_num))) + 
    geom_density(alpha = 0.2)+ facet_wrap(~server)

#--- Landing spot investigation
plot_arc1 <- plot_sample %>% 
    filter(arc == 1) %>%
    group_by(serveid) %>%
    top_n(4,t) %>%
    top_n(2,-t)
plot_arc3 <- plot_sample %>% 
    filter(arc == 3) %>%
    group_by(serveid) %>%
    top_n(4,-t) %>%
    top_n(2,t)
plot_gg <- plot_sample

court_topdown + geom_point(data = plot_gg, aes(x=center.x, y=center.y, colour = serve_classname), alpha=0.5)

court_topdown + geom_density2d(data = plot_gg, aes(x=center.x, y=center.y, colour = serve_classname), size=0.5)

court_topdown + 
    geom_point(data = filter(plot_gg, serve_classname != "Fault"), aes(x=center.x, y=center.y, colour = scorername), alpha=0.5)

court_topdown + 
    geom_point(data = filter(plot_gg, serve_classname != "Fault"), aes(x=center.x, y=center.y, colour = scorername), alpha=0.5) +
    geom_path(data = filter(plot_arc1, serve_classname != "Fault"), aes(x=px,y=py, group=serveid, colour = scorername), alpha = 0.5) +
    geom_path(data = filter(plot_arc3, serve_classname != "Fault"), aes(x=px,y=py, group=serveid, colour = scorername), alpha = 0.5)

#--- Single server
plot_gg <- plot_sample %>% filter(server == "RAONIC")
plot_ly(plot_gg, x=px, y=py, z=pz, type="surface") %>%
    add_trace(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
    add_trace(x=x, y=y, z=z, data=net_trace, type="scatter3d", mode="lines") %>%
    layout(scene=list(aspectmode="data"))
