



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")




# Figure S5 -----------------------------------------------------------------------------------

 soil_moisture <- sensors %>% 
   ggplot(aes(x = Timestamp, y = Soil_moist_18cm, colour = "18 cm")) +
   geom_line() +
   geom_line(aes(y = Soil_moist_7cm, colour = "  7 cm")) +
   scale_colour_okabe_ito() +
   xlab("Date") +
   labs(colour = "Depth") +
   ylab("Volumetric water content (% v/v)") +
   theme(legend.position = "bottom") + 
   vert_x_axis +
   ts_scale_x
 
 
 

# Export figure -------------------------------------------------------------------------------

pdf("Figure_S5_soil_moisture.pdf", width = 8, height = 5)
soil_moisture
dev.off()


