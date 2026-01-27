



# See setup.R for details on data and dependencies --------------------------------------------

source("setup.R")




# Figure 4 ------------------------------------------------------------------------------------

theme_incubation <- theme(legend.title = element_text(size=14),
                          legend.text = element_text(size = 12), 
                          legend.key.size = unit(2, 'lines'),
                          axis.text = element_text(size = 12),
                          axis.title = element_text(size = 14),
                          panel.grid.minor = element_blank(),
                          strip.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0, size = 15))

incubation_production <- incubation %>% 
  filter(Production_Oxidation == "P") %>% 
  ggplot(aes(x = Flux, y = Depth)) +
  geom_point() +
  scale_x_continuous(trans ='asinh', breaks=c(0, 1, 10, 100)) +
  scale_y_reverse(limits = c(100, 0), breaks=c(0, 25, 50, 75, 100), expand = c(0, 0)) +
  facet_grid(. ~ Date, labeller = labeller(Date = function(x) format(as.Date(x), "%b-%d"))) +
  xlab(expression(paste("CH"[4], " (", "nmol", " g dw"^-1, " h"^-1, ")"))) +
  ylab("Depth (cm)") +
  ggtitle(expression(paste("CH"[4], " production potential"))) +
  theme_incubation +
  theme_bw() 

incubation_oxidation <- incubation %>% 
  filter(Production_Oxidation == "O") %>%
  ggplot(aes(x = Flux, y = Depth)) +
  geom_point() +
  scale_y_reverse(limits = c(100, 0), breaks=c(0, 25, 50, 75, 100), expand = c(0, 0)) +
  facet_grid(. ~ Date, labeller = labeller(Date = function(x) format(as.Date(x), "%b-%d"))) +
  xlab(expression(paste("CH"[4], " (", "nmol", " g dw"^-1, " h"^-1, ")"))) +
  ylab("Depth (cm)") +
  ggtitle(expression(paste("CH"[4], " oxidation potential"))) +
  theme_incubation +
  theme_bw()





# Export figure -------------------------------------------------------------------------------

pdf("Figure_4_incubation.pdf", height = 9, width = 10)
incubation_production / incubation_oxidation + plot_annotation(tag_levels = "a")
dev.off()
