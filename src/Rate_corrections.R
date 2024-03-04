# Load source script:
source("src/Main.R")

######## 1. Considering only R2 requirement - No alternative models (removing sampling times stepwise as in Chrom. calculations)

Emission_rates_COR_1 <- Gasera_emission_rates_2023 %>% 
                          mutate(CH4_flux_mgm2h_cor = case_when(R2_CH4 < 0.7 ~ 0, TRUE ~ CH4_flux_mgm2h))

######## 2. Considering alternative models (removing sampling times stepwise as in Chrom. calculations)

######## 2.1. Create a dataframe Emission_rates_COR_2 from Emission_rates with all the new model columns ############ 

# Create a dataframe from "Gasera_emission_rates_2023" adding columns for alternative models:
Emission_rates_COR_2 <- Gasera_emission_rates_2023

# Create columns and assign names according to max(length(Time_step)) within all measurements:
max_length <- (3*length(unique(Gasera_results$Time_step))) # See Notes file in /data
  
additional_col_names <- paste0(rep(c("CH4_flux_Alt_", "CH4_R2_Alt_", "CH4_p_Alt_", "N2O_flux_Alt_", 
                        "N2O_R2_Alt_", "N2O_p_Alt_", "CO2_flux_Alt_", "CO2_R2_Alt_", "CO2_p_Alt_"), 
                        length.out = max_length), rep(1:(length(unique(Gasera_results$Time_step))),each = 9)) 
                        # Create column names for the additional columns

new_col_names <- c("CH4_model", "CH4_flux_COR", "CH4_R2_COR", "CH4_p_COR", "N2O_model", "N2O_flux_COR", 
                        "N2O_R2_COR", "N2O_p_COR", "CO2_model", "CO2_flux_COR", "CO2_R2_COR", "CO2_p_COR", 
                        additional_col_names) 
                        # Combine all column names, these are the 12 fix columns to be added to Emission_rates_COR_2, independent of 
                        # length(unique(Gasera_results$Time_step)).

Emission_rates_COR_2[new_col_names] <- NA # Add new columns to Emission_rates_COR_2
  
# Then, if length(Time_step) is different for Date-Rep (“Code”) combinations, the added columns should be the max(length(Time_step)). 
                        # For “Code” with lower length(Time_step), these additional columns are left with NA. This allows having measurements 
                        # (Date-Rep combinations, or "Code") with different amounts of Time_step (T0, T1, T2...).
  
######### 2.2. Loops to fill up new data frame #####################################################################
  
#### 2.2.1 Loops for CH4 ####

    for (l in 1:length(Emission_rates_COR_2$Code)) {
      Code_l <- Emission_rates_COR_2$Code[l]
      Filt_l <- Gasera_results[Gasera_results$Code == Code_l,]

      ## Loop section 1: Fitting alternative "removed-values" models (each one removing one time-step):
      #### Alt_1: excluding concentration from time step 0:
      for (m in 1:length(unique(Gasera_results$Time_step))) {
          Filt_Alt_m <- if(is.na(Filt_l$CH4_byMass_mgm2[m]) == TRUE) {Filt_l} else {Filt_l[-m,]}# Filters excluding one concentration. In case the value to be excluded is NA it doesn't exclude values.
          lm_Alt_m <- lm(CH4_byMass_mgm2~Sample_time_min, data=Filt_Alt_m) # Linear model of these 3 values
          Emission_rates_COR_2[, paste0("CH4_flux_Alt_", m)][m] <- coef(lm_Alt_m)[2]*60 # Returns CH4_flux_mgm2h for each Code.
          Emission_rates_COR_2[, paste0("CH4_R2_Alt_", m)][m] <- summary(lm_Alt_m)$r.squared # Returns R2_CH4 for each Code.
          Emission_rates_COR_2[, paste0("CH4_p_Alt_", m)][m] <- lmp_i(lm_Alt_m) # Returns p-value for each Code.
      }}

# Notes: 
# - This loop has to be extended either to all possible combinations of time step removals (leaving an agreed minimum, e.g. 3 as in chromatography) -> This will surely result in an extensive 
#   code with extremely long processing times...
# - Alternatively, The loop could just try with all possible alternative models in which only one time step is removed. Discuss about if this would worth the effort.

