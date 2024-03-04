n = 1
ñ = 1

for (n in 1:length(Emission_rates_COR$Code)) {
  Code_n <- Emission_rates_COR$Code[n]
  Filt_n <- Gasera_results[Gasera_results$Code == Code_n,]
  # Filt_n <- filter(Gasera_results, Gasera_results$Code == Code_n) # if returned as Time-Series, re-run library(dplyr)
  
  ## Loop section 1: Fitting alternative "removed-values" models (each one removing one time-step):
  #### Alt_1: excluding concentration from time step 0:
  for (ñ in 1:length(unique(Gasera_results$Time_step))) {
    Filt_Alt_ñ <- if(is.na(Filt_n$CH4_byMass_mgm2[ñ]) == TRUE) {Filt_n} else {Filt_n[-ñ,]}# Filters excluding one concentration. In case the value                      to be excluded is NA it doesn't exclude values.
    lm_Alt_ñ <- lm(CH4_byMass_mgm2~Sample_time_min, data=Filt_Alt_ñ) # Linear model of these 3 values
    Emission_rates_COR[, paste0("CH4_flux_Alt_", ñ)][ñ] <- coef(lm_Alt_ñ)[2]*60 # Returns CH4_flux_mgm2h for each Code.
    Emission_rates_COR[, paste0("CH4_R2_Alt_", ñ)][ñ] <- summary(lm_Alt_ñ)$r.squared # Returns R2_CH4 for each Code.
    Emission_rates_COR[, paste0("CH4_p_Alt_", ñ)][ñ] <- lmp_i(lm_Alt_ñ) # Returns p-value for each Code.
  }}