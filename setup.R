



#' Primary data from Moor House are available at https://doi.org/10.5281/ZENODO.18391367
#' 
#' ECN data are available from  https://doi.org/10.5285/fc9bcd1c-e3fc-4c5a-b569-2fe62d40f2f5 
#'                              https://doi.org/10.5285/b330d395-68f2-47f1-8d59-3291dc02923b 
#'                              
#' In ECN data, SITECODE "T04" corresponds to observations from Moor House.




# Packages and functions ----------------------------------------------------------------------

library(ggokabeito)
library(patchwork)
library(IsoplotR)
library(imputeTS)
library(tidyverse)

Sys.setlocale("LC_TIME", 'en_GB.UTF-8') 

source("functions.R")





# Import data ---------------------------------------------------------------------------------

fluxes <- read_csv("fluxes.csv")
hydrology <- read_csv("hydrology.csv")
isotopes <- read_csv("isotopes.csv")
incubation <- read_csv("incubation.csv")

ecn_ma_files <- list.files(pattern = "ecn_ma")  # ECN meteorology data, check file names 
ecn_ma <- do.call(rbind, lapply(ecn_ma_files, read_csv))

ecn_ss <- read_csv("ECN_SS1.csv")  # ECN soil solution data, check file names

fluxes$Date <- as.Date(fluxes$Timestamp)
fluxes$Chamber <- as.factor(fluxes$Chamber)
isotopes$Chamber <- as.factor(isotopes$Chamber)
fluxes$ihsCH4_flux <- ihs(fluxes$CH4_flux)
fluxes$ihsCO2_flux <- ihs(fluxes$CO2_flux)




# Data selection and interpolation ------------------------------------------------------------

ecn_ma <- ecn_ma %>% 
  filter(SITECODE == "T04" | SITECODE == "TO4") %>% 
  mutate(Timestamp = dmy_h(paste(SDATE, SHOUR)),
         SDATE = as.Date(Timestamp)) %>% 
  pivot_wider(names_from = FIELDNAME,
              values_from = VALUE)

ecn_ss <- ecn_ss %>%
  filter(SITECODE == "T04" | SITECODE == "TO4",
         RID != "ALL") %>% 
  mutate(Depth = str_sub(RID, -1),
         SDATE = dmy(SDATE),
         Depth = case_when(Depth == "S" ~ "10cm",
                           Depth == "D" ~ "50cm")) %>%
  group_by(SDATE, Depth, FIELDNAME) %>%
  summarise(VALUE = mean(VALUE, na.rm = TRUE)) %>% 
  pivot_wider(names_from = FIELDNAME,
              values_from = VALUE) %>% 
  select(SDATE, Depth, PH, DOC, COLOUR) %>% 
  pivot_wider(names_from = Depth,
              values_from = c(PH, DOC, COLOUR)) %>% 
  ungroup() %>% 
  complete(SDATE = seq.Date(as.Date("2015-01-07"), max(SDATE), by = "day")) %>% 
  arrange(SDATE) %>% 
  mutate(across(PH_10cm:COLOUR_50cm, ~na_interpolation(.x, option = "stine"))) 
 
  


# Isotope models ------------------------------------------------------------------------------

isotopes <- isotopes[order(isotopes$Date, isotopes$Chamber), ]

# Product of δ-value and CH4 concentration for Miller-Tans plots
isotopes$d13C_x_ppm <- isotopes$d13CVPDB * isotopes$GC_ppm
isotopes$d2H_x_ppm <- isotopes$d2HVSMOW * isotopes$GC_ppm

# Gaussian error propagation
isotopes$d13C_x_ppm_error <- abs(
  isotopes$d13C_x_ppm * sqrt((isotopes$GC_error / isotopes$GC_ppm)^2 +
                               (isotopes$d13C_error / isotopes$d13CVPDB)^2))
isotopes$d2H_x_ppm_error <- abs(
  isotopes$d2H_x_ppm * sqrt((isotopes$GC_error / isotopes$GC_ppm)^2 +
                              (isotopes$d2H_error / isotopes$d2HVSMOW)^2))

# Data frame for isotope model outputs
isotope_models <- data.frame(Date = unique(isotopes$Date),
                             Slope_d13C = NA,
                             Slope_d13C_err = NA,
                             Intercept_d13C = NA,
                             Intercept_d13C_err = NA,
                             Slope_d2H = NA,
                             Slope_d2H_err = NA,
                             Intercept_d2H = NA,
                             Intercept_d2H_err = NA,
                             n_13C = NA,
                             n_2H = NA
)

# York regressions
for (i in unique(isotopes$Date)) {
  
  # Subset isotope data for each date
  d13c_data <- isotopes %>% filter(Date == i) %>%
    select(GC_ppm, GC_error, d13C_x_ppm, d13C_x_ppm_error)
  d2h_data <- isotopes %>% filter(Date == i) %>% 
    select(GC_ppm, GC_error, d2H_x_ppm, d2H_x_ppm_error)
  
  #  Correlation coefficients for each date
  d13c_data$mt_13C_cor <- cor(d13c_data$GC_ppm, d13c_data$d13C_x_ppm, use = "complete.obs")
  d2h_data$mt_2H_cor <- cor(d2h_data$GC_ppm, d2h_data$d2H_x_ppm, use = "complete.obs")
  
  # Number of valid measurements per date
  isotope_models$n_13C[isotope_models$Date == i] <- sum(!is.na(d13c_data$d13C_x_ppm))
  isotope_models$n_2H[isotope_models$Date == i] <- sum(!is.na(d2h_data$d2H_x_ppm))
  
  # Only compute 13C model if n > 2 measurements
  if (isotope_models$n_13C[isotope_models$Date == i] > 2) {
    d13C_york <- york(as.data.frame(d13c_data))
  }
  # Extract regression parameters
  isotope_models$Slope_d13C[isotope_models$Date == i] <- d13C_york$b[1]
  isotope_models$Intercept_d13C[isotope_models$Date == i] <- d13C_york$a[1]
  
  isotope_models$Slope_d13C_err[isotope_models$Date == i] <- d13C_york$b[2]
  isotope_models$Intercept_d13C_err[isotope_models$Date == i] <- d13C_york$a[2]
  
  # Only compute 2H model if n > 2 measurements
  if (isotope_models$n_2H[isotope_models$Date == i] > 2) {
    d2H_york <- york(as.data.frame(d2h_data))
    
    # Extract regression parameters
    isotope_models$Slope_d2H[isotope_models$Date == i] <- d2H_york$b[1]
    isotope_models$Slope_d2H_err[isotope_models$Date == i] <- d2H_york$b[2]
    
    isotope_models$Intercept_d2H[isotope_models$Date == i] <- d2H_york$a[1]
    isotope_models$Intercept_d2H_err[isotope_models$Date == i] <- d2H_york$a[2]
    
  }
}




# Join data for analysis and plotting ---------------------------------------------------------

joined_data <- left_join(hydrology, left_join(ecn_ma, ecn_ss)) %>% 
  filter(Timestamp >= "2015-01-01")

joined_data <- left_join(joined_data, fluxes)

iso_flux <- left_join(isotopes, fluxes) %>% 
  filter(!is.na(CH4_flux))

iso_flux <- iso_flux %>% 
  group_by(Date) %>% 
  mutate(
         Mean_ihsCH4_flux_13C = mean(ihsCH4_flux[!is.na(d13CVPDB)]),
         Mean_ihsCH4_flux_2H = mean(ihsCH4_flux[!is.na(d2HVSMOW)])
         )

iso_joined_data <- joined_data %>% 
  group_by(Date) %>% 
  mutate(Mean_ihsCH4_flux = mean(ihsCH4_flux, na.rm = TRUE),
         Mean_ihsCO2_flux = mean(ihsCO2_flux, na.rm = TRUE)) %>% 
  left_join(isotope_models) %>% 
  slice(1) %>% 
  filter(!is.na(Slope_d13C | !is.na(Slope_d2H))) %>% 
  ungroup() %>% 
  left_join(iso_flux %>% 
              select(Date,
                     Mean_ihsCH4_flux_13C,
                     Mean_ihsCH4_flux_2H) %>% 
              slice(1))



# Plotting parameters -------------------------------------------------------------------------

theme_set(theme_test()) 

okabe_ito_custom <- scale_colour_okabe_ito(order = c(1, 2, 3, 6, 5))

min_date <- min(fluxes$Timestamp)
max_date <- max(fluxes$Timestamp)

ts_scale_x <- scale_x_datetime(date_breaks = "1 month", 
                               date_labels = "%b %Y",
                               limits = c(min_date, max_date))

ts_scale_x_date <- scale_x_date(date_breaks = "1 month",
                                date_labels = "%b %Y",
                                limits = c(as.Date(min_date), as.Date(max_date)))

scale_y_flux <- scale_y_continuous(trans ='asinh', breaks=c(-1, 0, 1, 10, 100, 1000, 10000)) 
  
vert_x_axis <- theme(axis.text.x = element_text(angle = 90,
                                                   vjust = 0.5,
                                                   hjust = 1))
