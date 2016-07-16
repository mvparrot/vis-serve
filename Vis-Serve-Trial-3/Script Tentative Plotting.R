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
coef.df <- StandardiseCoefficients(data,matchid,server,speedkmph,serve_num,serve_classname,side,scorername,
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

