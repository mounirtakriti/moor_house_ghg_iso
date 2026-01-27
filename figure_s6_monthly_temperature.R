



# See setup.R for details on data and dependencies --------------------------------------------

# This figure requires ECN meteorology data from 1992 onwards

source("setup.R")

monthly_temp <- ecn_ma %>% 
  mutate(Year = year(Timestamp),
         Month = month(Timestamp)) %>%
  group_by(Year, Month) %>% 
  summarise(Monthly_air_temp = mean(DRYTMP, na.rm = TRUE),
            Monthly_30cm_temp = mean(STMP30, na.rm = TRUE)) %>% 
  mutate(oneyear_date = as.Date(paste0("1900-", Month, "-01"), format = "%Y-%m-%d")) 

lt_monthly_temp <- monthly_temp %>% 
  group_by(Month) %>% 
  summarise(LT_monthly_air_temp = mean(Monthly_air_temp, na.rm = TRUE))

monthly_temp <- monthly_temp %>% 
  left_join(lt_monthly_temp) %>% 
  filter(Year %in% c(2015, 2016, 2017)) %>% 
  mutate(Year = as.factor(Year))
  



# Figure S6 -----------------------------------------------------------------------------------

okabe_ito_custom_year <- scale_colour_okabe_ito(order = c(1, 3, 5, 6))
ylim_temp <- ylim(0, 12.5)

mm_air_temp <- monthly_temp %>% 
  ggplot(aes(x = oneyear_date, 
             y = Monthly_air_temp, 
             colour = Year)) +
  geom_line() +
  geom_line(aes(x = oneyear_date,
                y = LT_monthly_air_temp, 
                colour = "1992-2014"),
            linetype = "dashed") +
  ylab("Air temperature (°C)") +
  okabe_ito_custom_year +
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

mm_soil_temp <- monthly_temp %>% 
  ggplot(aes(x = oneyear_date,
             y = Monthly_30cm_temp, 
             colour = Year)) +
  geom_line() +
  okabe_ito_custom_year + 
  ylim_temp +
  xlab("Month") +
  ylab("Soil temperature (°C)") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") +
  guides(colour = "none")




# Export figure -------------------------------------------------------------------------------

pdf("Figure_S6_monthly_temperature.pdf", width = 8, height = 7)
mm_air_temp / mm_soil_temp +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

