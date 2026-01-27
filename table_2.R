



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")

library(scipub)




# Table 2 -------------------------------------------------------------------------------------

iso_vars <- c(
  "Slope_d13C",
  "Slope_d2H",
  "Mean_ihsCH4_flux", 
  "STMP30",
  "Water_table_cm",
  "Soil_moist_18cm",
  "DOC_10cm",
  "COLOUR_10cm",
  "PH_10cm")

iso_table <- correltable(iso_joined_data,
                         method = "pearson",
                         vars = iso_vars,
                         html = FALSE)

iso_table$table
