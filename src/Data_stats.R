
####################################################### Thesis Paper 2 - Statistics #####################################################

library(dplyr)
library(vioplot)
library(ggplot2)

# 1. Data validation ####
# According to:
#    - Analyzing the impact of multiple stressors in aquatic biomonitoring data: A ‘cookbook’ with applications in R - Feld et al., 2016. 
#    - A protocol for data exploration to avoid common statistical problems - Zuur et al., 2010.

## 1.1. Check for outliers ####

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
# - check outliers: using loop for min and max calculations
#   Salinity: min (1 value) / O2_mg_l: max (1 value) / O2 percent: max (several) / redox_pot: max (1 value) / pH_soil: min (1 value) 

# Identifying outliers looping through each variable to identify outliers
for (i in vars) {stats <- boxplot.stats(Master_GHG_2023_phys[[i]]) # Get the boxplot statistics
                 outliers <- stats$out # Extract outliers
                 cat("Variable:", i, "\n") # Print the outliers for each variable
                  
                 if (length(outliers) > 0) {cat("Outliers:", outliers, "\n\n")
                                           } else {cat("No outliers found.\n\n")
                                                    }
                                                  }
