
####################################################### Thesis Paper 2 - GHG Statistics #####################################################

library(dplyr)
library(vioplot)
library(ggplot2)
library(vegan)
library(usdm)         # for vif()
library(Hmisc)        # for varclus()
library(dendextend)   # solves issues with as.dendrogram()
library(glmmTMB)      # for glmmTMB()
library(RColorBrewer) # for brewer.pal.info[] in dendrograms
library(emmeans)
library(DHARMa)
library(randomForest) # for check_distribution()
library(visreg)       # Plotting conditional residuals

# 1. Research question and hypotheses ####

# a. Are there Legacy effects of Treat implemented on GS over emissions in FS? 

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

write_xlsx(Master_GHG_2023_phys_noNA, "outputs/CERESTRES_results/Master_GHG_2023_phys_noNA.xlsx") # Excel file with Master_GHG_2023_phys_noNA

## 2.1. Check for outliers ####

names(Master_GHG_2023_phys_noNA)
vars <- c("Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected", "Chrom_CO2_flux_corrected", "Water_level_corr", "Temp_soil", "Rice_cover_prop",
          "Env_temp_initial_cor", "Env_temp_final_cor", "Conduct_microS_cm","Temp_10_cm", "pH_soil", "Redox_pot", "Water_temp", "O2_percent", 
          "O2_mg_l", "Salinity", "pH_water") # selecting variables to analyze

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
# downweighed . Usual transformations are to calculate the square-root or logarithm. Standardization of explanatory variables means to recalculate all 
# variables to a comparable numerical range. For example, pH values range from 0–14, nitrate concentration may range from 0–300 mg/L, while proportional 
# land use ranges 0–100%. The variables would reveal different effect sizes just because of their different numerical scaling. "If transformation is applied 
# to a variable too, standardization should follow transformation."

# Note: no need for data transformation as GLMM modelling does not assume data normality.

## 2.3. Independence of explanatory variables: collinearity and variance inflation ####

# Check for normal distribution of dependent variables:

shapiro.test(Master_GHG_2023_phys_noNA$Chrom_N2O_flux_corrected) # CH4 emissions not normally distributed
shapiro.test(Master_GHG_2023_phys_noNA$Chrom_CH4_flux_corrected) # N2O emissions not normally distributed

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
# A VIF > 8 is applied in Feld et al., 2016. We lower the threshold to 3 due to rank deficiency in complete models (still with VIF < 5) 

sel_vars1 <- c("Water_level_corr", "Temp_soil", "Rice_cover_prop", "Env_temp_initial_cor", "Env_temp_final_cor",  "Conduct_microS_cm",
               "Temp_10_cm", "pH_soil", "Redox_pot","Water_temp", "O2_percent", "O2_mg_l",  "Salinity", "pH_water")
sel_data1 <- (Master_GHG_2023_phys_noNA[, sel_vars1])
usdm::vifstep(sel_data1, th = 3)

# vifstep: calculates VIF for all variables, excludes the one with the highest VIF (if it is greater than the threshold), 
# repeat the procedure until no variables with a VIF greater than th remains.
# Result: 5 variables from the 14 input variables have collinearity problem: O2_percent Env_temp_initial_cor Temp_10_cm pH_water Water_temp

# Data frame removing coviariates after VIF analysis:  

sel_vars2 <- c("Water_level_corr", "Temp_soil", "Rice_cover_prop", "Env_temp_final_cor",  "Conduct_microS_cm",
               "pH_soil", "Redox_pot", "O2_mg_l",  "Salinity")
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

# Dend 2: Considering only covariates after VIF analysis and removal
# Variable clustering:

similarity="pearson"
vclust_2 <- varclus(x=as.matrix(sel_data2),
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

## 2.4. Relationships between Y and X variables? ####

# sel_vars2 <- c("Water_level_corr", "Rice_cover_prop", "Env_temp_final_cor",  "Conduct_microS_cm",
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

sel_vars4 <-  c("Env_temp_final_cor",  "Conduct_microS_cm","pH_soil", "Redox_pot") # Variables for FS, no water physicochemical data, no rice and constant water level

seasons <- unique(Master_GHG_2023_phys_noNA$Season)
GHG <- c("Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected")

pdf("outputs/CERESTRES_results/Chromat_results/Covars_scat.pdf", width = 20, height = 10)
par(mfrow = c(3, 6), mar = c(5, 6, 2, 0)) # Adjust the margin to reduce space

for (q in seq_along(GHG)) { # Loop through each GHG ("Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected")
      Gas <- GHG[q]

      for (p in seq_along(unique(Master_GHG_2023_phys_noNA$Season))) { # Loop through each season (GS and FS)
            sea <- seasons[p]
      
            if (sea == "GS") { # This if else separates GS and FS as they don't share same ind vars, during FS only soil physchem parameters were recorded.
        
                for (i in seq_along(sel_vars2)) { # Loop through each independent variable and create a scatterplot with custom y-axis label
                                    var <- sel_vars2[i]
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

# 3. Statistical Modeling ####

# According to 
#    - Analyzing the impact of multiple stressors in aquatic biomonitoring data: A ‘cookbook’ with applications in R - Feld et al., 2016.

## 3.1. Testing for linear or quadratic relation between response vars (CH4 and N2O fluxes) and Sampling ####

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
# A multi-modal relation can be observed for CH4, considering the complete season.

## 3.2. CH4 - Flux #### 

### 3.2.1. Exploring distributions ####

# Creating a "Sampling" column that assigns a number to each unique Sampling_date. This way we can have "Sampling" as a model variable. 
Master_GHG_2023_phys_noNA <- Master_GHG_2023_phys_noNA %>%
              arrange(Sampling_date) %>%
              mutate(Sampling = match(Sampling_date, unique(Sampling_date)))

# Treat, Season and Rep to be included as Factors in model:
Master_GHG_2023_phys_noNA$Treat <- as.factor(Master_GHG_2023_phys_noNA$Treat)
Master_GHG_2023_phys_noNA$Season <- as.factor(Master_GHG_2023_phys_noNA$Season)
Master_GHG_2023_phys_noNA$Rep <- as.factor(Master_GHG_2023_phys_noNA$Rep)

# Complete model - lm: 
CH4_mod_tot_lm <- lm(data = Master_GHG_2023_phys_noNA, Chrom_CH4_flux_corrected ~ Treat*Season + Water_level_corr + Temp_soil +
                         Rice_cover_prop + Env_temp_final_cor + Conduct_microS_cm + pH_soil + Redox_pot) 
# Salinity and O2_mg_l removed because water physicochemical parameters were not measured in mesocosm  so this leads to Treat*Season combinations without values for 
# these variables, if they are not removed the model cannot be fit: 
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]): 
# contrasts can be applied only to factors with 2 or more levels

# Check for normally distributed residuals:
residuals_lm <- residuals(CH4_mod_tot_lm)
shapiro.test(residuals_lm)

# Check for probable distribution: 
performance::check_distribution(CH4_mod_tot_lm)
hist(Master_GHG_2023_phys_noNA$Chrom_CH4_flux_corrected)

### 3.2.2. Initial model and link function  ####

# Complete model - glmm:
CH4_mod_tot <- glmmTMB(data = Master_GHG_2023_phys_noNA, Chrom_CH4_flux_corrected ~ Treat*Season + Sampling + Water_level_corr + Temp_soil +
                         Rice_cover_prop + Env_temp_final_cor + Conduct_microS_cm + pH_soil + (1|Rep), family = tweedie()) # Redox removed to avoid message (note below)

# Note: When considering Redox_pot, the model results in the Warning message: In (function (start, objective, gradient = NULL, hessian = NULL,  :
# NA/NaN function evaluation
# So the model removing Redox_pot is tested:
# CH4_mod_tot <- glmmTMB(data = Master_GHG_2023_phys_noNA, Chrom_CH4_flux_corrected ~ Treat*Season + Water_level_corr + Temp_soil +
#                          Rice_cover_prop + Env_temp_final_cor + Conduct_microS_cm + pH_soil + (1|Rep), family = tweedie()) 
# Resulting in same significances in ANOVA to model with Redox_pot. We decide to remove the variable.

# Model diagnostics:
DHARMa::simulateResiduals(CH4_mod_tot, plot = T)
summary(CH4_mod_tot)
car::Anova(CH4_mod_tot)
performance::r2(CH4_mod_tot)
performance::check_collinearity(CH4_mod_tot)
performance::check_singularity(CH4_mod_tot)
visreg(CH4_mod_tot, scale="response") # Plotting conditional residuals

### 3.2.3. Pair comparisons  ####

emmeans(CH4_mod_tot, ~Treat , type = "response")
pairs(emmeans(CH4_mod_tot, ~Treat , type = "response"))

emmeans(CH4_mod_tot, ~Treat|Season, type = "response", adjust = "bonferroni")
pairs(emmeans(CH4_mod_tot, ~Treat|Season, type = "response", adjust = "bonferroni"))




summary(pairs(emmeans(CH4_mod_tot, ~Treat|Season, type = "response")), adjust = "none") # removes bonferroni correction



emmeans(CH4_mod_tot, ~Season , type = "response", adjust = "none")
pairs(emmeans(CH4_mod_tot, ~Season , type = "response", adjust = "none"))

emmeans(CH4_mod_tot, ~Season|Treat, type = "response")
pairs(emmeans(CH4_mod_tot, ~Season|Treat, type = "response"))

### 3.2.4. Alt. 1 - Only FS: Assessing only GHG emission events for FS ####

event_dates <- as.Date(c('2023-10-23', '2023-11-03', '2023-11-15'))

PH_events <- Master_GHG_2023_phys_noNA %>% 
              filter(Sampling_date %in% event_dates)

# Initial model and link function  

# Complete model - glmm:
CH4_mod_PH_events <- glmmTMB(data = PH_events, Chrom_CH4_flux_corrected ~ Treat + Sampling + 
                         Env_temp_final_cor + Conduct_microS_cm + pH_soil + Redox_pot + (1|Rep), family = tweedie())

# Note: This model results in the Warning message: In (function (start, objective, gradient = NULL, hessian = NULL,  :
# NA/NaN function evaluation
# So the model removing Redox_pot is tested:
# CH4_mod_tot <- glmmTMB(data = Master_GHG_2023_phys_noNA, Chrom_CH4_flux_corrected ~ Treat*Season + Water_level_corr + Temp_soil +
#                          Rice_cover_prop + Env_temp_final_cor + Conduct_microS_cm + pH_soil + (1|Rep), family = tweedie()) 
# Resulting in same significances in ANOVA to model with Redox_pot. We decide to keep the variable.

# Model diagnostics:
DHARMa::simulateResiduals(CH4_mod_PH_events, plot = T)
summary(CH4_mod_PH_events)
car::Anova(CH4_mod_PH_events)
performance::r2(CH4_mod_PH_events)
performance::check_collinearity(CH4_mod_PH_events)
performance::check_singularity(CH4_mod_PH_events)
visreg(CH4_mod_PH_events, scale="response") # Plotting conditional residuals

# Pair comparisons:
emmeans(CH4_mod_PH_events, ~Treat , type = "response")
pairs(emmeans(CH4_mod_PH_events, ~Treat , type = "response"))

### 3.2.5. Alt. 2 - GS and FS: Assessing only GHG emission events for FS ####

Full_PH_events <- Master_GHG_2023_phys_noNA %>% 
                    filter(Season == "GS" | ((Sampling_date %in% event_dates) & Season == "PH"))

# Complete model - glmm:
CH4_mod_Full_PH_events <- glmmTMB(data = Full_PH_events, Chrom_CH4_flux_corrected ~ Treat*Season + Sampling + Water_level_corr + Temp_soil +
                                    Rice_cover_prop + Env_temp_final_cor + Conduct_microS_cm + pH_soil + Redox_pot + (1|Rep), family = tweedie())

# Note: This model results in the Warning message: In (function (start, objective, gradient = NULL, hessian = NULL,  :
# NA/NaN function evaluation
# So the model removing Redox_pot is tested:
# CH4_mod_tot <- glmmTMB(data = Master_GHG_2023_phys_noNA, Chrom_CH4_flux_corrected ~ Treat*Season + Water_level_corr + Temp_soil +
#                          Rice_cover_prop + Env_temp_final_cor + Conduct_microS_cm + pH_soil + (1|Rep), family = tweedie()) 
# Resulting in same significances in ANOVA to model with Redox_pot. We decide to keep the variable.

# Model diagnostics:
DHARMa::simulateResiduals(CH4_mod_Full_PH_events, plot = T)
summary(CH4_mod_Full_PH_events)
car::Anova(CH4_mod_Full_PH_events)
performance::r2(CH4_mod_Full_PH_events)
performance::check_collinearity(CH4_mod_Full_PH_events)
performance::check_singularity(CH4_mod_Full_PH_events)
visreg(CH4_mod_Full_PH_events, scale="response") # Plotting conditional residuals

# Pair comparisons:
emmeans(CH4_mod_Full_PH_events, ~Treat , type = "response")
pairs(emmeans(CH4_mod_Full_PH_events, ~Treat , type = "response"))

emmeans(CH4_mod_Full_PH_events, ~Treat|Season, type = "response")
pairs(emmeans(CH4_mod_Full_PH_events, ~Treat|Season, type = "response"))

emmeans(CH4_mod_Full_PH_events, ~Season , type = "response")
pairs(emmeans(CH4_mod_Full_PH_events, ~Season , type = "response"))

emmeans(CH4_mod_Full_PH_events, ~Season|Treat, type = "response")
pairs(emmeans(CH4_mod_Full_PH_events, ~Season|Treat, type = "response"))

### 3.2.6. Average flux variation per season and treatment####

Avg_flux_variation <- Master_GHG_2023_phys_noNA %>% 
                        group_by(Season, Treat) %>% 
                        summarise(avg_Chrom_CH4_flux = mean(Chrom_CH4_flux_corrected)) %>% 
                        mutate(CON_flux = avg_Chrom_CH4_flux[Treat == "CON"],
                               var_perc_vs_CON = (-1) * (CON_flux - avg_Chrom_CH4_flux) / CON_flux * 100) 

## 3.3. CH4 - Cumulative emissions #### 

### 3.3.1. CH4 - GS #### 

# Non-parametric Kruskal-Wallis Test is performed due to just having three observations (Plots) per group (Treat).
Acc_CH4_GS_KWdf <- Acc_CHROM_GS_sum %>% 
                    select(Plot, Treat, CCH4_kgha_tot)

KW_CH4_acc_GS <- kruskal.test(CCH4_kgha_tot ~ Treat, data = Acc_CH4_GS_KWdf)
print(KW_CH4_acc_GS)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_CH4_acc_GS<- pairwise.wilcox.test(Acc_CH4_GS_KWdf$CCH4_kgha_tot, Acc_CH4_GS_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_CH4_acc_GS)

Wilcox_CH4_acc_GS <- pairwise.wilcox.test(Acc_CH4_GS_KWdf$CCH4_kgha_tot, Acc_CH4_GS_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_CH4_acc_GS)

Wilcox_CH4_acc_GS <- pairwise.wilcox.test(Acc_CH4_GS_KWdf$CCH4_kgha_tot, Acc_CH4_GS_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_CH4_acc_GS)

Wilcox_CH4_acc_GS <- pairwise.wilcox.test(Acc_CH4_GS_KWdf$CCH4_kgha_tot, Acc_CH4_GS_KWdf$Treat)
print(Wilcox_CH4_acc_GS)

Wilcox_CH4_acc_GS <- pairwise.wilcox.test(Acc_CH4_GS_KWdf$CCH4_kgha_tot, Acc_CH4_GS_KWdf$Treat, p.adjust.method = "none", alternative = "greater")
print(Wilcox_CH4_acc_GS) # test 1 tail

### 3.3.2. CH4 - PH #### 

# Non-parametric Kruskal-Wallis Test is performed due to just having three observations (Plots) per group (Treat).
Acc_CH4_PH_KWdf <- Acc_CHROM_PH_sum %>% 
                    select(Plot, Treat, CCH4_kgha_tot)

KW_CH4_acc_PH <- kruskal.test(CCH4_kgha_tot ~ Treat, data = Acc_CH4_PH_KWdf)
print(KW_CH4_acc_PH)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_CH4_acc_PH <- pairwise.wilcox.test(Acc_CH4_PH_KWdf$CCH4_kgha_tot, Acc_CH4_PH_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_CH4_acc_PH)

Wilcox_CH4_acc_PH <- pairwise.wilcox.test(Acc_CH4_PH_KWdf$CCH4_kgha_tot, Acc_CH4_PH_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_CH4_acc_PH)

Wilcox_CH4_acc_PH <- pairwise.wilcox.test(Acc_CH4_PH_KWdf$CCH4_kgha_tot, Acc_CH4_PH_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_CH4_acc_PH)

Wilcox_CH4_acc_PH <- pairwise.wilcox.test(Acc_CH4_PH_KWdf$CCH4_kgha_tot, Acc_CH4_PH_KWdf$Treat, p.adjust.method = "none", alternative = "greater")
print(Wilcox_CH4_acc_PH) # test 1 tail

### 3.3.3. CH4 - Overal year #### 

# Non-parametric Kruskal-Wallis Test is performed due to just having three observations (Plots) per group (Treat).
Acc_CH4_tot_KWdf <- Acc_CHROM_tot_sum %>% 
  select(Plot, Treat, CCH4_kgha_tot)

KW_CH4_acc_tot <- kruskal.test(CCH4_kgha_tot ~ Treat, data = Acc_CH4_tot_KWdf)
print(KW_CH4_acc_tot)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_CH4_acc_tot <- pairwise.wilcox.test(Acc_CH4_tot_KWdf$CCH4_kgha_tot, Acc_CH4_tot_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_CH4_acc_tot)

Wilcox_CH4_acc_tot <- pairwise.wilcox.test(Acc_CH4_tot_KWdf$CCH4_kgha_tot, Acc_CH4_tot_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_CH4_acc_tot)

Wilcox_CH4_acc_tot <- pairwise.wilcox.test(Acc_CH4_tot_KWdf$CCH4_kgha_tot, Acc_CH4_tot_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_CH4_acc_tot)

Wilcox_CH4_acc_PH <- pairwise.wilcox.test(Acc_CH4_PH_KWdf$CCH4_kgha_tot, Acc_CH4_PH_KWdf$Treat, p.adjust.method = "none", alternative = "greater")
print(Wilcox_CH4_acc_PH) # test 1 tail

## 3.4. GWP - GWPY #### 

### 3.4.1. GS: GWP #### 

# Kruskal-Wallis Test:
GWP_GS_KWdf <- Acc_CHROM_GS_sum %>% 
                    select(Plot, Treat, GWP)

KW_GWP_GS <- kruskal.test(GWP ~ Treat, data = GWP_GS_KWdf)
print(KW_GWP_GS)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_GWP_GS <- pairwise.wilcox.test(GWP_GS_KWdf$GWP, GWP_GS_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_GWP_GS)

Wilcox_GWP_GS <- pairwise.wilcox.test(GWP_GS_KWdf$GWP, GWP_GS_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_GWP_GS)

Wilcox_GWP_GS <- pairwise.wilcox.test(GWP_GS_KWdf$GWP, GWP_GS_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_GWP_GS)

### 3.4.2. GS: GWPY ####

# Kruskal-Wallis Test:
GWPY_GS_KWdf <- Acc_CHROM_GS_sum %>% 
                    select(Plot, Treat, GWPY)

KW_GWPY_GS <- kruskal.test(GWPY ~ Treat, data = GWPY_GS_KWdf)
print(KW_GWPY_GS)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_GWPY_GS <- pairwise.wilcox.test(GWPY_GS_KWdf$GWPY, GWPY_GS_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_GWPY_GS)

Wilcox_GWPY_GS <- pairwise.wilcox.test(GWPY_GS_KWdf$GWPY, GWPY_GS_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_GWPY_GS)

Wilcox_GWPY_GS <- pairwise.wilcox.test(GWPY_GS_KWdf$GWPY, GWPY_GS_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_GWPY_GS)

### 3.4.3. PH: GWP #### 

# Kruskal-Wallis Test:
GWP_PH_KWdf <- Acc_CHROM_PH_sum %>% 
  select(Plot, Treat, GWP)

KW_GWP_PH <- kruskal.test(GWP ~ Treat, data = GWP_PH_KWdf)
                  print(KW_GWP_PH)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_GWP_PH <- pairwise.wilcox.test(GWP_PH_KWdf$GWP, GWP_PH_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_GWP_PH)

Wilcox_GWP_PH <- pairwise.wilcox.test(GWP_PH_KWdf$GWP, GWP_PH_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_GWP_PH)

Wilcox_GWP_PH <- pairwise.wilcox.test(GWP_PH_KWdf$GWP, GWP_PH_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_GWP_PH)

### 3.4.4. PH: GWPY #### 

# Kruskal-Wallis Test:
GWPY_PH_KWdf <- Acc_CHROM_PH_sum %>% 
                  select(Plot, Treat, GWPY)

KW_GWPY_PH <- kruskal.test(GWPY ~ Treat, data = GWPY_PH_KWdf)
print(KW_GWPY_PH)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_GWPY_PH <- pairwise.wilcox.test(GWPY_PH_KWdf$GWPY, GWPY_PH_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_GWPY_PH)

Wilcox_GWPY_PH <- pairwise.wilcox.test(GWPY_PH_KWdf$GWPY, GWPY_PH_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_GWPY_PH)

Wilcox_GWPY_PH <- pairwise.wilcox.test(GWPY_PH_KWdf$GWPY, GWPY_PH_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_GWPY_PH)

### 3.4.5. Both seasons: GWP #### 

# Kruskal-Wallis Test:
GWP_tot_KWdf <- Acc_CHROM_tot_sum %>% 
                  select(Plot, Treat, GWP)

KW_GWP_tot <- kruskal.test(GWP ~ Treat, data = GWP_tot_KWdf)
print(KW_GWP_tot)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_GWP_tot <- pairwise.wilcox.test(GWP_tot_KWdf$GWP, GWP_tot_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_GWP_tot)

Wilcox_GWP_tot <- pairwise.wilcox.test(GWP_tot_KWdf$GWP, GWP_tot_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_GWP_tot)

Wilcox_GWP_tot <- pairwise.wilcox.test(GWP_tot_KWdf$GWP, GWP_tot_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_GWP_tot)

### 3.4.6. Both seasons: GWPY #### 

# Kruskal-Wallis Test:
GWPY_tot_KWdf <- Acc_CHROM_tot_sum %>% 
  select(Plot, Treat, GWPY)

KW_GWPY_tot <- kruskal.test(GWPY ~ Treat, data = GWPY_tot_KWdf)
print(KW_GWPY_tot)

# Post-Hoc Analysis: pairwise Wilcoxon tests
Wilcox_GWPY_tot <- pairwise.wilcox.test(GWPY_tot_KWdf$GWPY, GWPY_tot_KWdf$Treat, p.adjust.method = "bonferroni")
print(Wilcox_GWPY_tot)

Wilcox_GWPY_tot <- pairwise.wilcox.test(GWPY_tot_KWdf$GWPY, GWPY_tot_KWdf$Treat, p.adjust.method = "holm")
print(Wilcox_GWPY_tot)

Wilcox_GWPY_tot <- pairwise.wilcox.test(GWPY_tot_KWdf$GWPY, GWPY_tot_KWdf$Treat, p.adjust.method = "none")
print(Wilcox_GWPY_tot)

