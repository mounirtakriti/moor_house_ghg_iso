



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")

library(ggpubr)




# Regressions ---------------------------------------------------------------------------------

summary(lm(Slope_d13C~Mean_ihsCH4_flux, data = iso_joined_data))
summary(lm(Slope_d2H~Mean_ihsCH4_flux, data = iso_joined_data))




# Figure 3 ------------------------------------------------------------------------------------

d13c_flux <- iso_joined_data %>% 
  ggplot(aes(x = Mean_ihsCH4_flux, y = Slope_d13C)) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "firebrick4") +
  stat_regline_equation(label.y = -74, 
                        aes(label = after_stat(eq.label)), 
                        formula = y ~ poly(x, 1, raw = T)) +
  stat_regline_equation(label.y = -75.4, aes(label = after_stat(rr.label)),
                        formula = y ~ poly(x, 1, raw = T)) +
  geom_point(color = "forestgreen") +
  xlab(expression(ihs(C*H[4]))) +
  ylab(expression(delta^13*C)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
  
d2h_flux <- iso_joined_data %>% 
  ggplot(aes(x = Mean_ihsCH4_flux, y = Slope_d2H)) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "firebrick4") +
  stat_regline_equation(label.y = -328, 
                        aes(label = after_stat(eq.label)), 
                        formula = y ~ poly(x, 1, raw = T)) +
  stat_regline_equation(label.y = -336, aes(label = after_stat(rr.label)),
                        formula = y ~ poly(x, 1, raw = T)) +
  geom_point(color = "dodgerblue4") +
  xlab(expression(ihs(C*H[4]))) +
  ylab(expression(delta^2*H))




# Export figure -------------------------------------------------------------------------------

pdf("Figure_3_isotopes.pdf", width = 4, height = 7)
d13c_flux / d2h_flux + plot_annotation(tag_levels = "a") 
dev.off()
