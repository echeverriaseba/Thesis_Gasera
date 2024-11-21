
################################################## Micro Stats ###################################################

library(glmmTMB)        # for glmmTMB()
library(emmeans)        # for estimated marginal means as glmm diagnostics
library(DHARMa)         # for residual diagnostics for hierarchical (multi-level/mixed) regression models
library(car)            # for Levene's Test
library(dplyr)          # for data manipulation
library(microeco)       # for FAPROTAX analisis using GLMM (2.2.)
library("rcompanion")   # for FAPROTAX analisis using GLMM (2.2.)

# 1. Methanotrophs rel ab ####

## 1.1. Stats scanning ####

Metr_lab <- read.csv("data/Metr_relab.csv", fileEncoding="latin1", na.strings=c("","NA")) # from filtering OTUS_FAPROTAX_abundrela_v2.cvs AVS reads file
Metr_lab$Treat <- factor(Metr_lab$Treat, levels = c("CON", "MSD", "AWD"))
factor(Metr_lab$Treat, levels = c("CON", "MSD", "AWD"))

for (i in unique(Metr_lab$Desc2)) {
  subset_i <- subset(Metr_lab, Desc2 == i)
  line_i <- paste(rep("#", 60), collapse = "") 
  line_i <- paste(line_i, i, line_i)  
  print(line_i)
  
  # ANOVA assumptions  
  shap_i <- shapiro.test(subset_i$Rel_ab) # Normality
  print(shap_i)
  leve_i <- leveneTest(Rel_ab ~ Treat, data = subset_i) # Homogeneity of Variances
  print(leve_i)
  
  # Perform tests
  print("ANOVA:")
  anova_result_i <- aov(Rel_ab ~ Treat, data = subset_i)
  print(anova_result_i)
  print(summary(anova_result_i))
  
  print("TukeyHSD:")
  Tuk_i <- TukeyHSD(anova_result_i)
  print(Tuk_i)

  print("Kruskal-Wallis:")
  KW_i <- kruskal.test(Rel_ab ~ Treat, data = subset_i)
  print(KW_i)

  # Post-Hoc Analysis: pairwise Wilcoxon tests
  print("Pairwise Wilcoxon:")
  Wilcox_i <- pairwise.wilcox.test(subset_i$Rel_ab, subset_i$Treat, p.adjust.method = "bonferroni")
  print(Wilcox_i)
}

## 1.2. GLMM Rel_ab from AVS reads data frame ####

Metr_lab2 <- Metr_lab %>% 
  filter(Sampling != 1) # Considering only growing and fallow seasons

Metr_lab2$Stage <- as.factor(Metr_lab2$Stage)

# Check for probable distribution: 
Metr_lm <- lm(data = Metr_lab2, Rel_ab ~ Treat*Stage + Sampling)
performance::check_distribution(Metr_lm)

Metr_lab2$Rel_ab_log <- log(Metr_lab2$Rel_ab) # log transformation

# GLMM:
Metr_glmm <- glmmTMB(data = Metr_lab2, Rel_ab ~ Treat*Stage + Sampling + (1|Rep), family = "gaussian")

# Model diagnostics:
DHARMa::simulateResiduals(Metr_glmm, plot = T)
summary(Metr_glmm)
car::Anova(Metr_glmm)
performance::r2(Metr_glmm)
performance::check_collinearity(Metr_glmm)
performance::check_singularity(Metr_glmm)
visreg(Metr_glmm, scale="response") # Plotting conditional residuals

# Pair comparisons: 

emmeans(Metr_glmm, ~Treat|Stage, type = "response")
pairs(emmeans(Metr_glmm, ~Treat|Stage, type = "response"))

emmeans(Metr_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni")
pairs(emmeans(Metr_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni"))

summary(pairs(emmeans(Metr_glmm, ~Treat|Stage, type = "response")), adjust = "none") # removes bonferroni correction

## 1.3. Methanotrophs alpha - relative ab ####

Metr_alph_lab <- read.csv("data/Metr_alph_relab.csv", fileEncoding="latin1", na.strings=c("","NA")) # from filtering OTUS_FAPROTAX_abundrela_v2.cvs AVS reads file
Metr_alph_lab$Treat <- factor(Metr_alph_lab$Treat, levels = c("CON", "MSD", "AWD"))
# factor(Metr_alph_lab$Treat, levels = c("CON", "MSD", "AWD"))

# GLMM Rel_ab from AVS reads data frame 

Metr_alph_lab2 <- Metr_alph_lab %>% 
  filter(Sampling != 1) # Considering only growing and fallow seasons

Metr_alph_lab2$Stage <- as.factor(Metr_alph_lab2$Stage)

# Check for probable distribution: 
Metr_alph_lm <- lm(data = Metr_alph_lab2, Rel_ab ~ Treat*Stage + Sampling)
performance::check_distribution(Metr_alph_lm)

Metr_alph_lab2$Rel_ab_log <- log(Metr_alph_lab2$Rel_ab) # log transformation

# GLMM:
Metr_alph_glmm <- glmmTMB(data = Metr_alph_lab2, Rel_ab ~ Treat*Stage + Sampling + (1|Rep), family = "gaussian")

# Model diagnostics:
DHARMa::simulateResiduals(Metr_alph_glmm, plot = T)
summary(Metr_alph_glmm)
car::Anova(Metr_alph_glmm)
performance::r2(Metr_alph_glmm)
performance::check_collinearity(Metr_alph_glmm)
performance::check_singularity(Metr_alph_glmm)
visreg(Metr_alph_glmm, scale="response") # Plotting conditional residuals

# Pair comparisons: 

emmeans(Metr_alph_glmm, ~Treat|Stage, type = "response")
pairs(emmeans(Metr_alph_glmm, ~Treat|Stage, type = "response"))

emmeans(Metr_alph_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni")
pairs(emmeans(Metr_alph_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni"))

summary(pairs(emmeans(Metr_alph_glmm, ~Treat|Stage, type = "response")), adjust = "none") # removes bonferroni correction

## 1.4. Methanotrophs gamma - relative ab ####

Metr_gamm_lab <- read.csv("data/Metr_gamm_relab.csv", fileEncoding="latin1", na.strings=c("","NA")) # from filtering OTUS_FAPROTAX_abundrela_v2.cvs AVS reads file
Metr_gamm_lab$Treat <- factor(Metr_gamm_lab$Treat, levels = c("CON", "MSD", "AWD"))
# factor(Metr_gamm_lab$Treat, levels = c("CON", "MSD", "AWD"))

# GLMM Rel_ab from AVS reads data frame 

Metr_gamm_lab2 <- Metr_gamm_lab %>% 
  filter(Sampling != 1) # Considering only growing and fallow seasons

Metr_gamm_lab2$Stage <- as.factor(Metr_gamm_lab2$Stage)

# Check for probable distribution: 
Metr_gamm_lm <- lm(data = Metr_gamm_lab2, Rel_ab ~ Treat*Stage + Sampling)
performance::check_distribution(Metr_gamm_lm)

Metr_gamm_lab2$Rel_ab_log <- log(Metr_gamm_lab2$Rel_ab) # log transformation

# GLMM:
Metr_gamm_glmm <- glmmTMB(data = Metr_gamm_lab2, Rel_ab ~ Treat*Stage + Sampling + (1|Rep), family = "gaussian")

# Model diagnostics:
DHARMa::simulateResiduals(Metr_gamm_glmm, plot = T)
summary(Metr_gamm_glmm)
car::Anova(Metr_gamm_glmm)
performance::r2(Metr_gamm_glmm)
performance::check_collinearity(Metr_gamm_glmm)
performance::check_singularity(Metr_gamm_glmm)
visreg(Metr_gamm_glmm, scale="response") # Plotting conditional residuals

# Pair comparisons: 

emmeans(Metr_gamm_glmm, ~Treat|Stage, type = "response")
pairs(emmeans(Metr_gamm_glmm, ~Treat|Stage, type = "response"))

emmeans(Metr_gamm_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni")
pairs(emmeans(Metr_gamm_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni"))

summary(pairs(emmeans(Metr_gamm_glmm, ~Treat|Stage, type = "response")), adjust = "none") # removes bonferroni correction

# 2. Methanogens rel ab ####

Metg_lab <- read.csv("data/Metg_relab.csv", fileEncoding="latin1", na.strings=c("","NA")) # from filtering OTUS_FAPROTAX_abundrela_v2.cvs AVS reads file
Metg_lab$Treat <- factor(Metg_lab$Treat, levels = c("CON", "MSD", "AWD"))

## 2.1. GLMM Rel_ab from AVS reads data frame ####

Metg_lab2 <- Metg_lab %>% 
  filter(Sampling != 1) # Considering only growing and fallow seasons

Metg_lab2$Stage <- as.factor(Metg_lab2$Stage)

# Check for probable distribution: 
Metg_lm <- lm(data = Metg_lab2, Rel_ab ~ Treat*Stage + Sampling)
performance::check_distribution(Metg_lm)

Metg_lab2$Rel_ab_log <- log(Metg_lab2$Rel_ab) # log transformation

# GLMM:
Metg_glmm <- glmmTMB(data = Metg_lab2, Rel_ab_log ~ Treat*Stage + Sampling + (1|Rep), family = "gaussian")

# Model diagnostics:
DHARMa::simulateResiduals(Metg_glmm, plot = T)
summary(Metg_glmm)
car::Anova(Metg_glmm)
performance::r2(Metg_glmm)
performance::check_collinearity(Metg_glmm)
performance::check_singularity(Metg_glmm)
visreg(Metg_glmm, scale="response") # Plotting conditional residuals

# Pair comparisons: 

emmeans(Metg_glmm, ~Treat|Stage, type = "response")
pairs(emmeans(Metg_glmm, ~Treat|Stage, type = "response"))

emmeans(Metg_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni")
pairs(emmeans(Metg_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni"))

summary(pairs(emmeans(Metg_glmm, ~Treat|Stage, type = "response")), adjust = "none") # removes bonferroni correction

# 3. Methylotrophs rel ab ####

Mety_lab <- read.csv("data/Methyl_relab.csv", fileEncoding="latin1", na.strings=c("","NA")) # from filtering OTUS_FAPROTAX_abundrela_v2.cvs AVS reads file
# Mety_lab$Treat <- as.factor(Mety_lab$Treat)
Mety_lab$Treat <- factor(Mety_lab$Treat, levels = c("CON", "MSD", "AWD"))

## 3.1. GLMM Rel_ab from AVS reads data frame ####

Mety_lab2 <- Mety_lab %>% 
  filter(Sampling != 1) # Considering only growing and fallow seasons

Mety_lab2$Stage <- as.factor(Mety_lab2$Stage)

# Check for probable distribution: 
Mety_lm <- lm(data = Mety_lab2, Rel_ab ~ Treat*Stage + Sampling)
performance::check_distribution(Mety_lm)

Mety_lab2$Rel_ab_log <- log(Mety_lab2$Rel_ab) # log transformation

# GLMM:
Mety_glmm <- glmmTMB(data = Mety_lab2, Rel_ab_log ~ Treat*Stage + Sampling + (1|Rep), family = "gaussian")

# Model diagnostics:
DHARMa::simulateResiduals(Mety_glmm, plot = T)
summary(Mety_glmm)
car::Anova(Mety_glmm)
performance::r2(Mety_glmm)
performance::check_collinearity(Mety_glmm)
performance::check_singularity(Mety_glmm)
visreg(Mety_glmm, scale="response") # Plotting conditional residuals

# Pair comparisons: 

emmeans(Mety_glmm, ~Treat|Stage, type = "response")
pairs(emmeans(Mety_glmm, ~Treat|Stage, type = "response"))

emmeans(Mety_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni")
pairs(emmeans(Mety_glmm, ~Treat|Stage, type = "response", adjust = "bonferroni"))

summary(pairs(emmeans(Mety_glmm, ~Treat|Stage, type = "response")), adjust = "none") # removes bonferroni correction

# 4. GLMM Rel_ab from FAPROTAX elements ####

# Applying GLMM as statistic method for analysis within FAPROTAX using T6 as subset example before running for complete "dataset" element.

## 4.1. Elements and data frames #### 

dataset_sampl <-  clone(dataset)
dataset_sampl$sample_table$data %<>% factor(., levels = c("t1", "t2", "t3", "t4", "t5", "t6")) # order X axis
dataset_t6 <- clone(dataset_sampl)
dataset_t6$sample_table <- subset(dataset_t6$sample_table, data == "t6") # Selecting T6
dataset_t6$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD")) # Defining treatments as factors
dataset_t6$cal_abund()
func6 <- trans_func$new(dataset_t6)
func6$cal_spe_func(prok_database = "FAPROTAX") 
func6$cal_spe_func_perc(abundance_weighted = TRUE)
tmp6 <- list() # use list to prepare data
tmp6$func <- as.data.frame(t(func6$res_spe_func_perc), check.names = FALSE) # transpose res_spe_func_perc to be a data.frame like taxonomic abundance
dataset_t6$taxa_abund <- tmp6 # assign the list as taxa_abund in microtable object

## 4.2. Defining GLMM #### 

# GLMM to be applied to complete "dataset" element:

# func6_glmm <- trans_diff$new(dataset = dataset_t6, method = "glmm",  
#                              taxa_level = "all", formula = "tractament*stages + data + (1|rèplica)") # trans_diff class to perform differential test

# GLMM for this T6 subset example: Because there is just one value for factors "stages" and "data"
 
func6_glmm <- trans_diff$new(dataset = dataset_t6, method = "glmm", group = "tractament", transformation = "log",
                             taxa_level = "all", formula = "tractament + (1|rèplica)") # trans_diff class to perform differential test
func6_glmm_df <- as.data.frame(func6_glmm$res_diff) # data frame with model coefficients for each functional group

save(func6_glmm_df, file = "outputs/Micro/Micro_visual/func6_glmm_df.RData") 

## 4.3. FAPROTAX GLMM plot #### 

# First attempt: ggplot visualization 

# fap_t6_glmm <- func6_glmm$plot_diff_abund(add_sig = T)
# plot_diff_abund function cannot be applied to multi-factor analysis:
# Error in func6_glmm$plot_diff_abund(add_sig = T, color_values = c("#002B5B",  : The function can not be applied to multi-factor analysis!

# Second attempt: heatmap visualization

fap_t6_glmm <- func6_glmm$plot_diff_bar(add_sig = T, color_values = RColorBrewer::brewer.pal(8, "Dark2"))
print(fap_t6_glmm)

# Paired comparisson cannot be applied (using emmeans() and pairs() function), "func6_glmm" model is of class "trans_diff" and not "glmmTMB" 
# summary(pairs(emmeans(func6_glmm$res_diff, ~tractament, type = "response")), adjust = "none") # removes bonferroni correction
# class(func6_glmm)

ggsave("outputs/Micro/Micro_visual/FAP_glmm_samp6.pdf", plot = fap_t6_glmm, width = 10, height = 20, units = "in")  

# 5. Total Abundance ####

Tot_ab_FS <- read.csv("data/BACTOT_mcrA_FS.csv", fileEncoding="latin1", na.strings=c("","NA")) # from filtering OTUS_FAPROTAX_abundrela_v2.cvs AVS reads file
# Mety_lab$Treat <- as.factor(Mety_lab$Treat)
Tot_ab_FS$Treat <- factor(Tot_ab_FS$Treat, levels = c("CON", "MSD", "AWD"))

## 5.1. Total bacterial abundance (BACTOT) ####

# Check for probable distribution: 
Tot_bact_lm <- lm(data = Tot_ab_FS, BACTOT ~ Treat + Sampling)
performance::check_distribution(Tot_bact_lm) # Distribution Probability: neg. binomial (zero-infl.)         56%

# GLMM:
Tot_bact_glmm <- glmmTMB(data = Tot_ab_FS, BACTOT ~ Treat + Sampling + (1|Rep), family = "nbinom2")

# Model diagnostics:
DHARMa::simulateResiduals(Tot_bact_glmm, plot = T)
summary(Tot_bact_glmm)
car::Anova(Tot_bact_glmm)
performance::r2(Tot_bact_glmm)
performance::check_collinearity(Tot_bact_glmm)
performance::check_singularity(Tot_bact_glmm)
visreg(Tot_bact_glmm, scale="response") # Plotting conditional residuals

# Pair comparisons: 

emmeans(Tot_bact_glmm, ~Treat, type = "response")
pairs(emmeans(Tot_bact_glmm, ~Treat, type = "response"))

emmeans(Tot_bact_glmm, ~Treat, type = "response", adjust = "bonferroni")
pairs(emmeans(Tot_bact_glmm, ~Treat, type = "response", adjust = "bonferroni"))

summary(pairs(emmeans(Tot_bact_glmm, ~Treat, type = "response")), adjust = "none") # removes bonferroni correction

## 5.2. Total methanogenic archaea abundance (mcrA) ####

# Check for probable distribution: 
Tot_metg_arch_lm <- lm(data = Tot_ab_FS, mcrA ~ Treat + Sampling)
performance::check_distribution(Tot_metg_arch_lm) # Distribution Probability: neg. binomial (zero-infl.)         56%

# GLMM:
Tot_metg_arch_lm <- glmmTMB(data = Tot_ab_FS, mcrA ~ Treat + Sampling + (1|Rep), family = "nbinom2")

# Model diagnostics:
DHARMa::simulateResiduals(Tot_metg_arch_lm, plot = T)
summary(Tot_metg_arch_lm)
car::Anova(Tot_metg_arch_lm)
performance::r2(Tot_metg_arch_lm)
performance::check_collinearity(Tot_metg_arch_lm)
performance::check_singularity(Tot_metg_arch_lm)
visreg(Tot_metg_arch_lm, scale="response") # Plotting conditional residuals

# Pair comparisons: 

emmeans(Tot_metg_arch_lm, ~Treat, type = "response")
pairs(emmeans(Tot_metg_arch_lm, ~Treat, type = "response"))

emmeans(Tot_metg_arch_lm, ~Treat, type = "response", adjust = "bonferroni")
pairs(emmeans(Tot_metg_arch_lm, ~Treat, type = "response", adjust = "bonferroni"))

summary(pairs(emmeans(Tot_metg_arch_lm, ~Treat, type = "response")), adjust = "none") # removes bonferroni correction
