



# See setup.R for details and dependencies ----------------------------------------------------

source("setup.R")

library(multcomp)




# Statistics ----------------------------------------------------------------------------------

# Scale fluxes 
incubation <- incubation %>% 
  group_by(Production_Oxidation, Date) %>% 
  mutate(Flux_scale = scale(Flux),
         Depth = as.factor(Depth))

# Subset treatments
production <- incubation %>% filter(Production_Oxidation == "P")
oxidation <- incubation %>% filter(Production_Oxidation == "O")

# ANOVA
anova_prod <- aov(Flux_scale ~ Depth, data = production)
anova_oxi <- aov(Flux_scale ~ Depth, data = oxidation)

# Post hoc analysis
tukey_prod <- glht(anova_prod, linfct = mcp(Depth = "Tukey"))
tukey_oxi <- glht(anova_oxi, linfct = mcp(Depth = "Tukey"))




# Figure S8 ------------------------------------------------------------------------------------

production_boxplot <- cld(tukey_prod)
production_boxplot$yname <- expression(paste("CH"[4], " production (standard scores)"))
production_boxplot$xname <- "Depth (cm)"

oxidation_boxplot <- cld(tukey_oxi)
oxidation_boxplot$yname <- expression(paste("CH"[4], " oxidation (standard scores)"))
oxidation_boxplot$xname <- "Depth (cm)"




# Export figure -------------------------------------------------------------------------------

pdf("Figure_S8_incubation.pdf", width = 10, height = 10)
opar <- par(mai=c(1,1,1.2,1), mfrow = c(2, 1))
plot(production_boxplot)
mtext(line = 4, cex = 1.3, adj = -0.1, "a")
plot(oxidation_boxplot)
mtext(line = 4, cex = 1.3, adj = -0.1, "b")
par(opar)
dev.off()
