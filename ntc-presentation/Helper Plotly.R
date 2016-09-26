plot_ly(plot_sample, x=px, y=py, z=pz, group=serveid, type="scatter3d", mode="lines") %>%
  add_trace(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
  layout(scene=list(aspectmode="data"))

serve_coolarc <- c("5_02_02_1_190119.trj","3_01_02_1_171036.trj")
plot_gg <- plot_sample %>% filter(serveid %in% serve_coolarc)
plot_ly(plot_gg, x=px, y=py, z=pz, group=serveid, type="scatter3d", mode="lines") %>%
  add_trace(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
  layout(scene=list(aspectmode="data"))