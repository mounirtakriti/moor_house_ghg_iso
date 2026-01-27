



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")




# Figure S7 -----------------------------------------------------------------------------------

CH4_water_table <- 
  joined_data %>% 
  filter(!is.na(CH4_flux)) %>% 
  ggplot(aes(Water_table_cm, CH4_flux, colour = Chamber)) +
  geom_point(size = 1.5) +
  scale_y_flux +
  okabe_ito_custom +
  xlab("Water tabel (cm)") +
  ylab(expression(CH[4] ~ (µmol~m^-2 ~ h^-1))) +
  theme(legend.position = "bottom") +  
  labs(colour='Location')




# Export figure -------------------------------------------------------------------------------

pdf("Figure_S7_CH4_water_table.pdf", width = 8, height = 5)
CH4_water_table
dev.off()


