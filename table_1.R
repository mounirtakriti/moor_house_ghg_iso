



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")

library(scipub)




# Table 1 -------------------------------------------------------------------------------------

flux_joined_data <- joined_data %>% 
  filter(!is.na(CH4_flux) | !is.na(CO2_flux))

vars <- c("ihsCH4_flux",
          "ihsCO2_flux", 
          "STMP30",
          "Water_table_cm",
          "Soil_moist_18cm",
          "DOC_10cm",
          "COLOUR_10cm",
          "PH_10cm")

# Repeated measures correlation table for replicated data
table_rmc <- rmc_cor_table(df = flux_joined_data, 
              vars = vars,
              group_var = "Chamber",
              stars = T,
              decimal = 2)
table_rmc <- table_rmc[startsWith(rownames(table_rmc), "ihs"), ]

# Pearson correlation table for non-replicated data
table_pearson <- correltable(data = filter(flux_joined_data, Chamber == 1),
                             method = "pearson",
                             vars = vars[!grepl("ihs", vars)],
                             html = FALSE)

table_rmc
table_pearson$table

