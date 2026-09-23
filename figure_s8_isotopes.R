



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")




# Regression ----------------------------------------------------------------------------------

summary(lm(Slope_d13C~Slope_d2H, data = iso_joined_data))

summary(lm(d13CVPDB~d2HVSMOW, data = isotopes))




# Figure S8 ------------------------------------------------------------------------------------

d13c_d2H <- iso_joined_data %>% 
  ggplot(aes(Slope_d13C, Slope_d2H)) +
  xlab(expression(delta^13*C~of~CH[4])) +
  ylab(expression(delta^2*H~of~CH[4])) +
  geom_point()

d13c_d2H_raw <- isotopes %>% 
  ggplot(aes(d13CVPDB, d2HVSMOW)) +
  xlab(expression(delta^13*C~of~CH[4])) +
  ylab(expression(delta^2*H~of~CH[4])) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point()

d13c_d2H_raw


# Export figure -------------------------------------------------------------------------------

pdf("Figure_S8_isotopes.pdf", width = 5, height = 4)
d13c_d2H
dev.off()
