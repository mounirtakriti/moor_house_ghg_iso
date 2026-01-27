



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")




# Figure S4 -----------------------------------------------------------------------------------

co2_time_series <- ggplot(fluxes, aes(Date, CO2_flux, colour = Chamber)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  scale_y_flux +
  okabe_ito_custom +
  ylab(expression(CO[2] ~ (µmol~m^-2 ~ h^-1))) +
  theme(legend.position = "bottom") +  
  labs(colour='Location') +
  vert_x_axis +
  ts_scale_x_date




# Export figure -------------------------------------------------------------------------------

pdf("Figure_S4_CO2_time_series.pdf", width = 8, height = 5)
co2_time_series
dev.off()


