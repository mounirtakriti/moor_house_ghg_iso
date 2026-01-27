



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")

fluxes <- fluxes %>% 
  group_by(Chamber) %>% 
  mutate(CH4_flux_scaled = scale(CH4_flux))




# Figure S3 -----------------------------------------------------------------------------------

ch4_time_series_scaled <- ggplot(fluxes, aes(Date, CH4_flux_scaled, colour = Chamber)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  okabe_ito_custom +
  ylab(expression(CH[4] ~ ("standard scores"))) +
  theme(legend.position = "bottom") +
  labs(colour='Location') +
  vert_x_axis +
  ts_scale_x_date




# Export figure -------------------------------------------------------------------------------

pdf("Figure_S3_CH4_scaled.pdf", width = 8, height = 5)
ch4_time_series_scaled
dev.off()


