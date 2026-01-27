



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")




# Figure 2 ------------------------------------------------------------------------------------

(rmc_ihsCH4_temp <- rmcorr(dataset = joined_data, Chamber, STMP30, ihsCH4_flux))
(rmc_ihsCH4_ihsCO2 <- rmcorr(dataset = joined_data, Chamber, ihsCO2_flux, ihsCH4_flux))

ch4_temp <- plot_rmc(rmc_ihsCH4_temp) + 
  okabe_ito_custom +
  ylim(-2, 12) +
  xlab("Soil temperature (°C)") +
  ylab(expression(ihs(CH[4]))) +
  labs(colour='Location')

ch4_co2 <- plot_rmc(rmc_ihsCH4_ihsCO2) + 
  # chcol +
  okabe_ito_custom +
  ylim(-2, 12) +
  xlab(expression(ihs(CO[2]))) +
  labs(colour='Location') 



# Export figure -------------------------------------------------------------------------------

pdf("Figure_2_rmcorr.pdf", width = 8, height = 4)
ch4_temp + ch4_co2 + 
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides = "collect",
              axes = "collect") &  
  theme(legend.position = "bottom")
dev.off()
