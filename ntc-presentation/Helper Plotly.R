plot_ly(plot_sample, x=px, y=py, z=pz, group=serveid, type="scatter3d", mode="lines") %>%
  add_trace(x=x, y=y, z=z, data=court_trace, type="scatter3d", mode="lines") %>%
  layout(scene=list(aspectmode="data"))