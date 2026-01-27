



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")

library(scipub)




# Table S2 ------------------------------------------------------------------------------------

flux_joined_data <- joined_data %>% 
  filter(!is.na(CH4_flux) | !is.na(CO2_flux))

vars2 <- c("ihsCH4_flux",
          "ihsCO2_flux", 
          "STMP10",
          "STMP30",
          "Water_table_cm",
          "Soil_moist_7cm",
          "Soil_moist_18cm",
          "DOC_10cm",
          "DOC_50cm",
          "COLOUR_10cm",
          "COLOUR_50cm",
          "PH_10cm",
          "PH_50cm"
          )

# Repeated measures correlation table for replicated data
table_rmc2 <- rmc_cor_table(df = flux_joined_data, 
              vars = vars2,
              group_var = "Chamber",
              stars = T,
              decimal = 2)
table_rmc2 <- table_rmc2[startsWith(rownames(table_rmc2), "ihs"), ]

# Pearson correlation table for non-replicated data
table_pearson2 <- correltable(data = filter(flux_joined_data, Chamber == 1),
                             method = "pearson",
                             vars = vars2[!grepl("ihs", vars2)],
                             html = FALSE)

table_rmc2
table_pearson2$table
