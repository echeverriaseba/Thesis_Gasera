
####################################################### Thesis Paper 2 - GHG Statistics #####################################################

library(dplyr)
library(vioplot)
library(ggplot2)

# 1. Research question and hypotheses ####

# a. Is there a GS Treat effect on C-CH4 and N-N2O emitted during the growing season (GS)
#    H0: mean(GS_GHG_flux) = 0
#    H1: mean(GS_GHG_flux) ≠ 0
# b. Is there a GS Treat effect on C-CH4 and N-N2O emitted during the fallow season (FS)
#    H0: mean(FS_GHG_flux) = 0
#    H1: mean(FS_GHG_flux) ≠ 0

# 2. Data validation ####
# According to:
#    - Analyzing the impact of multiple stressors in aquatic biomonitoring data: A ‘cookbook’ with applications in R - Feld et al., 2016. 
#    - A protocol for data exploration to avoid common statistical problems - Zuur et al., 2010.

## 2.1. Check for outliers ####

names(Master_GHG_2023_phys)
vars <- c("Gasera_CCH4_flux_mgm2h_cor", "Gasera_NN2O_flux_mgm2h_cor", "Gasera_CCO2_flux_mgm2h_cor", "Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected",
              "Chrom_CO2_flux_corrected", "Water_level_corr", "Temp_soil", "Rice_cover_prop", "Env_temp_initial", "Env_temp_final", "Conduct_microS_cm", 
              "Temp_10_cm", "pH_soil", "Redox_pot", "Water_temp", "O2_percent", "O2_mg_l", "Salinity", "pH_water") # selecting variables to analyze

# Exploratory plots: histogram, boxplot, vioplot  
for (i in vars) {hist(Master_GHG_2023_phys[,i], main = i)
                      boxplot(Master_GHG_2023_phys[,i], main = i)
                      vioplot(Master_GHG_2023_phys[,i], main = i) # vioplot: gives info about where points are concentrated (e.g. 2 peaks)
                 } 
  
# Notes:
# - check what's wrong with water temp values.
# - check outliers for:
#   Salinity: min (1 value) / O2_mg_l: max (1 value) / O2 percent: max (several) / redox_pot: max (1 value) / pH_soil: min (1 value) 

# Identifying outliers looping through each variable to identify outliers
for (i in vars) {stats <- boxplot.stats(Master_GHG_2023_phys[[i]]) # Get the boxplot statistics
                 outliers <- stats$out # Extract outliers
                 cat("Variable:", i, "\n") # Print the outliers for each variable
                  
                 if (length(outliers) > 0) {cat("Outliers:", outliers, "\n\n")} 
                 else {cat("No outliers found.\n\n")}
                 }

# Deal with identified outliers





# 3. Test residuals' homogeneity of variance (homoscedasticity) ####
# Notes: 
# - For this it's necessary to fit a model first, plot residuals and check. Fit GLMM, or MANOVA, assuming "Chrom_CH4_flux_corrected" and
# "Chrom_N2O_flux_corrected" as dependent variables. 
# - Conduct a PCA to explore variability associated to Season and water management effect on "Chrom_CH4_flux_corrected" and
# "Chrom_N2O_flux_corrected" (use Maite's Neglecting as a reference)
# - Check if for this data and research question it is necessary to fit GLMM or lm serves already the purpose. Define first research question and hypothesis. 


