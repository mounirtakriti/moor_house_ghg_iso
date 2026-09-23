



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")

fluxes_long <- fluxes %>%
  pivot_longer(cols = c(CH4_flux, CO2_flux), 
               names_to = "GHG", 
               values_to = "Flux",
               values_drop_na = TRUE)




# Figure S9 -----------------------------------------------------------------------------------

CH4_CO2_violin <-
  fluxes_long %>% 
  ggplot(aes(GHG, Flux)) +
  scale_y_flux +
  scale_x_discrete(labels = c(expression(CH[4]), expression(CO[2]))) +
  okabe_ito_custom +
  geom_jitter(aes(colour = Chamber), width = 0.35) +
  geom_violin(, fill = "white", alpha = 0) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0, linewidth = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 23, fill = "grey") +
  ylab(expression(GHG~flux~(µmol~m^-2 ~ h^-1))) +
  xlab("Greenhouse gas") +
  labs(colour='Location') +
  theme(legend.position = "bottom")
  
  





# Export figure -------------------------------------------------------------------------------

pdf("Figure_S10_CH4_CO2_violin.pdf", width = 8, height = 5)
CH4_CO2_violin
dev.off()


