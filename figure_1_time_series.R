



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")




# Figure 1a -----------------------------------------------------------------------------------

ch4_time_series <- ggplot(fluxes, aes(Date, CH4_flux, colour = Chamber)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  scale_y_flux +  
  okabe_ito_custom +
  ylab(expression(CH[4] ~ (µmol~m^-2 ~ h^-1))) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(0.73, 0.1),
    legend.direction = "horizontal") +
  labs(colour='Location') +
  ts_scale_x_date




# Figure 1b -----------------------------------------------------------------------------------

temp_time_series <- ggplot(joined_data, aes(x = Timestamp, y = STMP30)) +
  geom_line(aes(y = DRYTMP, colour = "air_temp"), alpha = 0.7) +
  scale_colour_manual(values = c("orangered3", "black"), 
                      labels=c("Air", "Soil"),
                      guide = guide_legend(label.hjust = 0.5)) +
  geom_line(aes(colour = "soil_temp_30")) +
  ylab("Temperature (°C)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    legend.title=element_blank(),
    legend.position = c(0.89, 0.15),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "transparent")) +
  ts_scale_x




# Figure 1c -----------------------------------------------------------------------------------

watertable_time_series <- ggplot(hydrology, aes(x = Timestamp,
                                              y = Water_table_cm)) +
  geom_line(colour = "cornflowerblue") +
  xlab("Date") +
  ylab("WTD (cm)") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  ts_scale_x




# Figure 1d -----------------------------------------------------------------------------------

ci_z <- 1.959964  # Z-value for 95% confidence interval

d13c_time_series <- ggplot(isotope_models, aes(x = Date, y = Slope_d13C)) +
  geom_point(colour = "forestgreen") +
  geom_errorbar(aes(ymin = Slope_d13C - ci_z * Slope_d13C_err,
                    ymax = Slope_d13C + ci_z * Slope_d13C_err), 
                width = 0.3, colour = "forestgreen") +
  ylab(expression(delta^13*"C")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  ts_scale_x_date




# Figure 1e -----------------------------------------------------------------------------------

d2h_time_series <- ggplot(isotope_models, aes(x = Date, y = Slope_d2H)) +
  geom_point(colour = "dodgerblue4") +
  geom_errorbar(aes(ymin = Slope_d2H - ci_z * Slope_d2H_err,
                    ymax = Slope_d2H + ci_z * Slope_d2H_err), 
                width = 0.3, colour = "dodgerblue4") +
  ylab(expression(delta^2*"H")) +
  vert_x_axis +
  ts_scale_x_date




# Export figure -------------------------------------------------------------------------------

pdf("Figure_1_time_series.pdf", width = 7, height = 10)
  ch4_time_series /
  temp_time_series /
  watertable_time_series /
  d13c_time_series /
  d2h_time_series +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights = c(2, 1, 1, 1, 1))
dev.off()


