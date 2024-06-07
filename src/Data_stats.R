
####################################################### Thesis Paper 2 - GHG Statistics #####################################################

library(dplyr)
library(vioplot)
library(ggplot2)
library(vegan)
library(usdm) # for vif()
library(Hmisc) # for varclus()
library(dendextend) # solves issues with as.dendrogram()
library(RColorBrewer) # for brewer.pal()
# install.packages('TMB', type = 'source')
library(glmmTMB) # for glmmTMB()

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
#    - A protocol for data exploration to avoid common statistical problems - Zuur et al., 2010.

# First we create a data frame that considers only sampling dates for chromatography
Master_GHG_2023_phys_noNA <- subset(Master_GHG_2023_phys, is.na(Chrom_CH4_flux_corrected) == FALSE) 

# Creating a "Sampling" column that assigns a number to each unique Sampling_date. This way we can have "Sampling" as a model variable. 
Master_GHG_2023_phys_noNA <- Master_GHG_2023_phys_noNA %>%
                 arrange(Sampling_date) %>%
                 mutate(Sampling = match(Sampling_date, unique(Sampling_date)))

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

# Method: "One strategy for addressing this problem is to sequentially drop the covariate with the highest VIF, recalculate the VIFs and repeat this process
# until all VIFs are smaller than a pre- selected threshold" (Zuur et al, 2010)
# We exclude variables with VIF > 5 stepwise, starting with the variable that has the highest VIF
# A VIF > 8 is applied in Feld et al., 2016. We lower the threshold to 5.

sel_vars1 <- c("Water_level_corr", "Temp_soil", "Rice_cover_prop", "Env_temp_initial", "Env_temp_final",  "Conduct_microS_cm",
               "Temp_10_cm", "pH_soil", "Redox_pot","Water_temp", "O2_percent", "O2_mg_l",  "Salinity", "pH_water")
sel_data1 <- (Master_GHG_2023_phys_noNA[, sel_vars1])
usdm::vifstep(sel_data1, th = 5)

# vifstep: calculates VIF for all variables, excludes the one with the highest VIF (if it is greater than the threshold), 
# repeat the procedure until no variables with a VIF greater than th remains.
# Result: 2 variables from the 14 input variables have collinearity problem: O2_percent Env_temp_initial

# Data frame after removing coviariates after VIF analysis:  

sel_vars2 <- c("Water_level_corr", "Temp_soil", "Rice_cover_prop", "Env_temp_final",  "Conduct_microS_cm",
               "Temp_10_cm", "pH_soil", "Redox_pot","Water_temp", "O2_mg_l",  "Salinity", "pH_water")
sel_data2 <- (Master_GHG_2023_phys_noNA[, sel_vars2])

### 2.3.2. Dendrogram with independent variables (covariates) ####

# Dend 1: Considering all variables (without VIF removal)
# Variable clustering:

similarity="pearson"
vclust_1 <- varclus(x=as.matrix(sel_data1),
                    similarity=similarity,
                    type="data.matrix", 
                    method="complete",
                    na.action=na.retain,trans="abs")
dend_1 <- as.dendrogram(vclust_1)

# Random colors and plot dendrogram:
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- brewer.pal(n=8,"Paired")
dend_1 <- color_labels(dend_1, h=1-0.7,col=col_vector)
dend_1 <- color_branches(dend_1, h=1-0.7,col=col_vector)

cairo_pdf("outputs/CERESTRES_results/Chromat_results/Covars_dend_1.pdf",width=7,height=4)
par(mar=c(5,2,4,17)+0.1)
plot(dend_1,horiz = TRUE,xlab="",axes = FALSE)
axis(1,at=1-seq(0,1,0.2),labels=seq(0,1,0.2))
dev.off()

# From this complete dendrogram, collinearity is also identified for Temp_soil and Temp_10_cm. We decide removing Temp_soil due to less data available.

sel_vars3 <- c("Water_level_corr", "Rice_cover_prop", "Env_temp_final",  "Conduct_microS_cm",
               "Temp_10_cm", "pH_soil", "Redox_pot","Water_temp", "O2_mg_l",  "Salinity", "pH_water")
sel_data3 <- (Master_GHG_2023_phys_noNA[, sel_vars3])

# Dend 2: Considering only covariates after VIF analysis and removal
# Variable clustering:

similarity="pearson"
vclust_2 <- varclus(x=as.matrix(sel_data3),
                    similarity=similarity,
                    type="data.matrix", 
                    method="complete",
                    na.action=na.retain,trans="abs")
dend_2 <- as.dendrogram(vclust_2)

# Random colors and plot dendrogram:
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- brewer.pal(n=8,"Paired")
dend_2 <- color_labels(dend_2, h=1-0.7,col=col_vector)
dend_2 <- color_branches(dend_2, h=1-0.7,col=col_vector)

cairo_pdf("outputs/CERESTRES_results/Chromat_results/Covars_dend_2.pdf",width=7,height=4)
par(mar=c(5,2,4,17)+0.1)
plot(dend_2,horiz = TRUE,xlab="",axes = FALSE)
axis(1,at=1-seq(0,1,0.2),labels=seq(0,1,0.2))
dev.off()

## 2.4. Relationships between Y and X variables?

# sel_vars3 <- c("Water_level_corr", "Rice_cover_prop", "Env_temp_final",  "Conduct_microS_cm",
# "Temp_10_cm", "pH_soil", "Redox_pot","Water_temp", "O2_mg_l",  "Salinity", "pH_water")

custom_ylabs <- c( # Defining specific ylabs for scatterplots
                    expression("Water level (cm)"),
                    expression("Rice cover (%)"),
                    expression("Air temperature (ºC)"),
                    expression(paste("Soil electrical conductivity (μS cm"^"-1",")")),
                    expression("Soil temperature at 10 cm (ºC)"),
                    expression("Soil pH"),
                    expression("Redox potential"),
                    expression("Water temperature (ºC)"),
                    expression(paste("Oxigen (mg l"^"-1",")")),
                    expression(paste("Salinity (mg l"^"-1",")")),
                    expression("Water pH")
                    )

custom_xlabs <- c( # Defining specific xlabs for scatterplots
                    expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")")),
                    expression(paste(N[2], "O flux (mg ", m^-2, " ", h^-1, ")")))

custom_main <- c( # Defining specific xlabs for scatterplots
                    expression("Growing Season"),
                    expression("Fallow Season"))

# Scaterplots: GHG emissions vs covariates for both seasons: According to Zuur et al., 2010.

sel_vars4 <-  c("Water_level_corr", "Rice_cover_prop", "Env_temp_final",  "Conduct_microS_cm",
                "Temp_10_cm", "pH_soil", "Redox_pot")

seasons <- unique(Master_GHG_2023_phys_noNA$Season)
GHG <- c("Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected")

pdf("outputs/CERESTRES_results/Chromat_results/Covars_scat.pdf", width = 20, height = 10)
par(mfrow = c(3, 6), mar = c(5, 6, 2, 0)) # Adjust the margin to reduce space

for (q in seq_along(GHG)) { # Loop through each GHG ("Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected")
      Gas <- GHG[q]

      for (p in seq_along(unique(Master_GHG_2023_phys_noNA$Season))) { # Loop through each season (GS and FS)
            sea <- seasons[p]
      
            if (sea == "GS") { # This if else separates GS and FS as they don't share same ind vars, during FS only soil physchem parameters were recorded.
        
                for (i in seq_along(sel_vars3)) { # Loop through each independent variable and create a scatterplot with custom y-axis label
                                    var <- sel_vars3[i]
                                    season_data <- subset(Master_GHG_2023_phys_noNA, Season == sea)
                                    plot(season_data[[Gas]], season_data[[var]], 
                                      ylab = "",
                                      xlab =  custom_xlabs[q],
                                      main = custom_main[p])
                                    mtext(custom_ylabs[i], side = 2, line = 3)}
        
      } else { 
        
                for (h in seq_along(sel_vars4)) { # Loop through each independent variable and create a scatterplot with custom y-axis label
                                    var <- sel_vars4[h]
                                    season_data <- subset(Master_GHG_2023_phys_noNA, Season == sea)
                                    plot(season_data[[Gas]], season_data[[var]], 
                                         ylab = "",
                                         xlab = custom_xlabs[q],
                                         main = custom_main[p])
                                    mtext(custom_ylabs[h], side = 2, line = 3)
}}}}

par(mfrow = c(1, 1))
dev.off()





# PCA:

# Conduct a PCA to explore variability associated to Season and water management effect on "Chrom_CH4_flux_corrected" and
# "Chrom_N2O_flux_corrected" (use Maite's Neglecting as a reference)

PCA <- rda(Nut, scale = TRUE) # Correspondence Analysis and Redundancy Analysis. Scale = TRUE: Defines Correlations instead of Covariance.

biplot(PCA) # prints the PCA as a plot. Plots in red variables and in black the sites.






# 3. Statistical Modeling ####

# According to 
#    - Analyzing the impact of multiple stressors in aquatic biomonitoring data: A ‘cookbook’ with applications in R - Feld et al., 2016.

# Testing for linear or quadratic relation between response vars (CH4 and N2O fluxes) and Sampling 

pdf("outputs/CERESTRES_results/Chromat_results/Sampling_resp.pdf", width = 20, height = 10)
par(mfrow = c(2, 2), mar = c(5, 5, 2, 0)) # Adjust the margin to reduce space

for (q in seq_along(GHG)) { # Loop through each GHG ("Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected")
      Gas <- GHG[q]
  
      for (p in seq_along(unique(Master_GHG_2023_phys_noNA$Season))) { # Loop through each season (GS and FS)
            sea <- seasons[p]
            season_data <- subset(Master_GHG_2023_phys_noNA, Season == sea)
            plot(season_data$Sampling, season_data[[Gas]], 
                 ylab = custom_xlabs[q],
                 xlab = "Sampling",
                 main = custom_main[p])
            
      }}

par(mfrow = c(1, 1))
dev.off()

# From these plots a quadratic relation between flux and Sampling can only be observed for CH4 during GS, but not for FS and not for N2O across both seasons.
# The quadratic term of Sampling is, therefore, not to be consider further in models.

GS_data <- subset(Master_GHG_2023_phys_noNA, Season == "GS")
FS_data <- subset(Master_GHG_2023_phys_noNA, Season == "PH")

## 3.1. Initial model and link function  ####
 
# Family: Gaussian
# Interacting independent variables: Treat*Sampling
# Additional independent variables: Previously selected sel_vars3 for CH4 and sel_vars4 for N2O
# Random effect: Rep

# CH4 - GS:
CH4_mod_GS_tot <- glmmTMB(data = GS_data, Chrom_CH4_flux_corrected ~ Treat*Sampling + Water_level_corr + Rice_cover_prop + Env_temp_final + 
                          Conduct_microS_cm + Temp_10_cm + pH_soil + Redox_pot + Water_temp + O2_mg_l + Salinity + pH_water + (1|Rep) , family = "gaussian")

# CH4 - FS:
CH4_mod_FS_tot <-  glmmTMB(data = FS_data, Chrom_CH4_flux_corrected ~ Treat*Sampling + Water_level_corr + Rice_cover_prop + Env_temp_final +
                          Conduct_microS_cm + Temp_10_cm + pH_soil + Redox_pot + (1|Rep) , family = "gaussian")

# N2O - GS:
N2O_mod_GS_tot <- glmmTMB(data = GS_data, Chrom_N2O_flux_corrected ~ Treat*Sampling + Water_level_corr + Rice_cover_prop + Env_temp_final + 
                          Conduct_microS_cm + Temp_10_cm + pH_soil + Redox_pot + Water_temp + O2_mg_l + Salinity + pH_water + (1|Rep) , family = "gaussian")

# N2O - GS:
N2O_mod_FS_tot <- glmmTMB(data = FS_data, Chrom_N2O_flux_corrected ~ Treat*Sampling + Water_level_corr + Rice_cover_prop + Env_temp_final +
                          Conduct_microS_cm + Temp_10_cm + pH_soil + Redox_pot + (1|Rep) , family = "gaussian")

# Model diagnostics:
Tot_models <- c(CH4_mod_FS_tot, CH4_mod_FS_tot, N2O_mod_GS_tot, N2O_mod_FS_tot)

for (a in seq_along(Tot_models)) {

                DHARMa::simulateResiduals(a, plot = T)
                summary(a)
                car::Anova(a)
                performance::r2(a)
                performance::check_collinearity(a)
                performance::check_singularity(a)
                visreg(a, scale="response") # Plotting conditional residuals
                }

## 3.2. Multi-model inference and model averaging  ####

