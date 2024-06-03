
####################################################### Thesis Paper 2 - GHG Statistics #####################################################

library(dplyr)
library(vioplot)
library(ggplot2)
library(vegan)

# 1. Research question and hypotheses ####

# a. Is there a GS Treat effect on C-CH4 and N-N2O emitted during the growing season (GS)?
#    potential_model: GS_GHG_flux ~ Treat*Season
#    H0: mean(GS_GHG_flux) = 0
#    H1: mean(GS_GHG_flux) ≠ 0

# b. Is there a GS Treat effect on C-CH4 and N-N2O emitted during the fallow season (FS)?
#    potential_model: FS_GHG_flux ~ Treat*Season
#    H0: mean(FS_GHG_flux) = 0
#    H1: mean(FS_GHG_flux) ≠ 0

# c. Is there one or more physicochemial parameter acting as emission driver (for GS and FS)?
#    potential_models: GS_GHG_flux ~ Treat*Season + c(physchem_vars); and FS_GHG_flux ~ Treat*Season + c(physchem_vars)
#    H0: mean(GS_GHG_flux) = 0; and H0: mean(FS_GHG_flux) = 0
#    H1: mean(GS_GHG_flux) ≠ 0; and H0: mean(FS_GHG_flux) = 0

# 2. Data exploration ####

# According to:
#    - Analyzing the impact of multiple stressors in aquatic biomonitoring data: A ‘cookbook’ with applications in R - Feld et al., 2016. 
#    - A protocol for data exploration to avoid common statistical problems - Zuur et al., 2010.

# First we create a data frame that considers only sampling dates for chromatography
Master_GHG_2023_phys_noNA <- subset(Master_GHG_2023_phys, is.na(Chrom_CH4_flux_corrected) == FALSE) 

## 2.1. Check for outliers ####

names(Master_GHG_2023_phys_noNA)
vars <- c("Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected",
          "Chrom_CO2_flux_corrected", "Water_level_corr", "Temp_soil", "Rice_cover_prop", "Env_temp_initial", "Env_temp_final", "Conduct_microS_cm", 
          "Temp_10_cm", "pH_soil", "Redox_pot", "Water_temp", "O2_percent", "O2_mg_l", "Salinity", "pH_water") # selecting variables to analyze

# Exploratory plots: histogram, boxplot, vioplot  
for (i in vars) {hist(Master_GHG_2023_phys_noNA[,i], main = i)
                 boxplot(Master_GHG_2023_phys_noNA[,i], main = i)
                 vioplot(Master_GHG_2023_phys_noNA[,i], main = i)
                 } 
  
# Identifying outliers looping through each variable
for (i in vars) {stats <- boxplot.stats(Master_GHG_2023_phys_noNA[[i]]) # Get the boxplot statistics
                 outliers <- stats$out # Extract outliers
                 cat("Variable:", i, "\n") # Print the outliers for each variable
                  
                 if (length(outliers) > 0) {cat("Outliers:", outliers, "\n\n")} 
                 else {cat("No outliers found.\n\n")}
                 }

# Deal with identified outliers:
# Salinity: min (1 value, asked Raul) / O2_mg_l: max (1 value, 126 should be a 12.6, changed in data sheet) / 
# O2 percent: max (several, asked Raul)) / redox_pot: max (1 value, asked Raul) / pH_soil: min (1 value, corrected from -6.9 to 6.9) 

## 2.2. Data transformation and standardization #### 

# Feld, 2016: Data transformation aims to approach normal distribution of continuous data in that the influence of high values of a given variable is 
# downweighed . Usual transformations are to calculate the square-root or logarithm. Standardisation of explanatory variables means to recalculate all 
# variables to a comparable numerical range. For example, pH values range from 0–14, nitrate concentration may range from 0–300 mg/L, while proportional 
# land use ranges 0–100%. The variables would reveal different effect sizes just because of their different numerical scaling. "If transformation is applied 
# to a variable too, standardisation should follow transformation."

# Note: no need for data transformation as GLMM modelling does not assume data normality.

## 2.3. Independence of explanatory variables: collinearity and variance inflation ####

# Notes:
# - Usually we would test residuals' homogeneity of variance (homoscedasticity) and data's normal distribution but we will be fitting GLMMs to test the effect of
#   GS Treat on GS and FS GHG emissions, plus the effect of physchem. parameterrs as covariates and account for random factors. 
#   Both of these requirements are not within models' assumptions. 
#   Data has to be independently distributed though.  
# - Feld, 2016: Generalised linear modelling requires explanatory variables to be independent, i.e. the variables must not highly correlate with each other. 
#   Collinearity addresses this issue and can be easily quantified for continuous variables using Pearson's or Spearman's correlation coefficients

### 2.3.1. Correlation plot ####

cor_matrix_GHG <- Master_GHG_2023_phys_noNA[,vars] %>% 
                  na.omit

corr_GHG <- round(cor(cor_matrix_GHG, method =  "spearman"), 1)

pdf("outputs/CERESTRES_results/Chromat_results/Corr_plot_GHG.pdf", width = 11)
corrplot::corrplot.mixed(corr_GHG,
                          order = 'hclust',
                          addrect = 2,
                          tl.col = "black", # Set text color to black
                          tl.srt = 45,      # Rotate text by 45 degrees
                          number.cex = 0.7, # Determine size of text for correlation values
                          mar = c(1, 1, 1, 1), # Adjusting the margin
                          tl.pos = "lt"     # Set text label position to "left-top"
                        )
dev.off()

### 2.3.1. Variance Inflation ####



## 2.4. Are there lots of zeros in the data? ####








# PCA:

# Conduct a PCA to explore variability associated to Season and water management effect on "Chrom_CH4_flux_corrected" and
# "Chrom_N2O_flux_corrected" (use Maite's Neglecting as a reference)

PCA <- rda(Nut, scale = TRUE) # Correspondence Analysis and Redundancy Analysis. Scale = TRUE: Defines Correlations instead of Covariance.

biplot(PCA) # prints the PCA as a plot. Plots in red variables and in black the sites.

