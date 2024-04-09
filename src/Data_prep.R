################################################### Gasera vs Chromatography - CERESTRES 2023 #########################################################

library(tidyverse)
library(ggplot2)
library(zoo)
library(writexl)
library(gridExtra)
library(cowplot)

# 1. Creating Master dataframe: Master_GHG_2023 ####

## Importing Chromatography results from Rproject: Chromatography_results:

load("C:/Users/SECHEVERRIA/R_git/Thesis_Chromatography/outputs/2023/Rates_corrected/Emission_rates_w_corrections_2023.RData")
load("C:/Users/SECHEVERRIA/R_git/Thesis_Gasera/outputs/CERESTRES_results/Gasera_emission_rates_2023.RData") # Importing calculated rates without corrections (not even R2 > 0.7)

## Renaming columns from Emission_rates_w_corrections_2023 dataframe, adding "Chrom_" to differentiate from gasera results:

Emission_rates_w_corrections_2023 <- Emission_rates_w_corrections_2023 %>%
                                      rename_with(.cols = all_of(names(Emission_rates_w_corrections_2023)[7:length(Emission_rates_w_corrections_2023)]),
                                                  .fn = ~ paste ("Chrom_", ., sep = ""))
# Solving date format issues:

# Function to convert date formats to "YYYY-mm-dd"
convert_to_ymd <- function(date_str) {
  parts <- strsplit(date_str, "/")[[1]]
  
  # Check if day part has a single digit
  if (nchar(parts[2]) == 1) {
    # If so, add a leading zero to day part
    parts[2] <- sprintf("%02d", as.numeric(parts[2]))
  }
  
  # Check if month part has a single digit
  if (nchar(parts[1]) == 1) {
    # If so, add a leading zero to month part
    parts[1] <- sprintf("%02d", as.numeric(parts[1]))
  }
  
  # Rearrange the parts to YYYY-mm-dd format
  date <- paste(parts[3], parts[1], parts[2], sep = "-")
  return(date)
}

# Apply the conversion function to the Sampling_date column and convert to Date format:

Emission_rates_w_corrections_2023$Sampling_date <- sapply(Emission_rates_w_corrections_2023$Sampling_date, convert_to_ymd) #Apply only if necessary, depending on Sampling_date format
Emission_rates_w_corrections_2023$Sampling_date <- as.Date(Emission_rates_w_corrections_2023$Sampling_date, format = "%Y-%m-%d")

# Importing piezometer information:

Water_level_2023 <- read.csv("data/Piezo_2023.csv", fileEncoding="latin1", na.strings=c("","NA")) %>% # Import water level (piezometer) data
                    rename(Water_level_piezo = Water_level_cm) 

Water_level_2023$Date <- as.Date(Water_level_2023$Date, format = "%Y-%m-%d")

Master_GHG_2023  <- merge(Emission_rates_w_corrections_2023, Water_level_2023, by.x = c("Sampling_date", "Treat", "Plot", "Rep"), by.y = c("Date", "Treat", "Plot", "Rep"),
                          all.x= TRUE, all.y = TRUE) %>%  # with all.x= TRUE, all.y = TRUE, the resulting dataframe contains as well dates where either only water level or emissions were recorded.
                    select(Sampling_date, Treat, Plot, Rep, Chrom_CH4_flux_corrected, Chrom_N2O_flux_corrected, Chrom_CO2_flux_corrected, Water_level_piezo)

Field_sheet_chrom_2023 <- read.csv("data/Field_sheet_chrom_2023.csv", fileEncoding="latin1", na.strings=c("","NA"))  # Load chromatography field sheet 2023
# rename(Water_level_piezo = Water_level_cm)

Field_sheet_other_factors_2023 <- Field_sheet_chrom_2023 %>% 
                                  group_by(Sampling_date, Plot) %>% 
                                  summarise(Water_level_ruler = mean(Water_level_cm), Temp_soil = mean(Temp_soil), Rice_cover_prop = mean(Rice_cover_prop), 
                                            Env_temp_initial = mean(Env_temp_initial), 
                                            Env_temp_final = mean(Env_temp_final)) # creates dataframe with all factors measured during chromatography campaign dates, besides GHG emissions.

Field_sheet_other_factors_2023$Sampling_date <- as.Date(Field_sheet_other_factors_2023$Sampling_date) # Converting to date format

## Merging Gasera and Chromatography emission rate dataframes:

Master_GHG_2023 <- merge(Gasera_emission_rates_2023, Master_GHG_2023, by.x = c("Date", "Plot", "Treat", "Rep"), by.y = c("Sampling_date", "Plot", "Treat", "Rep"), all.x = TRUE, all.y = TRUE)

colnames(Master_GHG_2023)[colnames(Master_GHG_2023) == "Date"] <- "Sampling_date"

Master_GHG_2023 <- Master_GHG_2023  %>% 
                    left_join(Field_sheet_other_factors_2023, by = c("Sampling_date", "Plot"))
Master_GHG_2023 <- Master_GHG_2023[!(Master_GHG_2023 $Plot %in% c("P10","P11", "P12", "P13", "P14", "P15")), ] #Removes Rep 4 and Rep 5 (only sampled during first dates)
Master_GHG_2023$Row_Nr <- 1:nrow(Master_GHG_2023) # Adding a row number column
Master_GHG_2023$Water_level_corr <- NA # add column "Water_level_corr"
# Master_GHG_2023 <- Master_GHG_2023[, c(13, 1, 2, 3, 4, 5, 6, 7, 8, 14, 9, 10, 11, 12)] # Reorder columns
Master_GHG_2023 <- Master_GHG_2023[!(is.na(Master_GHG_2023$Chrom_CH4_flux_corrected) & is.na(Master_GHG_2023$Chrom_N2O_flux_corrected) & is.na(Master_GHG_2023$CH4_flux_mgm2h) & 
                                       is.na(Master_GHG_2023$N2O_flux_mgm2h) & is.na(Master_GHG_2023$CO2_flux_mgm2h) & 
                                       is.na(Master_GHG_2023$Water_level_piezo)), ] # Removes rows without emissions and piezometer records.

colnames(Master_GHG_2023)[colnames(Master_GHG_2023) == "Rep.x"] <- "Rep"
colnames(Master_GHG_2023)[colnames(Master_GHG_2023) == "CH4_flux_mgm2h"] <- "Gasera_CH4_flux_mgm2h" 
colnames(Master_GHG_2023)[colnames(Master_GHG_2023) == "N2O_flux_mgm2h"] <- "Gasera_N2O_flux_mgm2h" 
colnames(Master_GHG_2023)[colnames(Master_GHG_2023) == "CO2_flux_mgm2h"] <- "Gasera_CO2_flux_mgm2h" 

Master_GHG_2023 <- Master_GHG_2023 %>% 
                  mutate(Gasera_CH4_flux_mgm2h_cor = case_when(R2_CH4 < 0.7 ~ 0, TRUE ~ Gasera_CH4_flux_mgm2h),
                         Gasera_N2O_flux_mgm2h_cor = case_when(R2_N2O < 0.7 ~ 0, TRUE ~ Gasera_N2O_flux_mgm2h),
                         Gasera_CO2_flux_mgm2h_cor = case_when(R2_CO2 < 0.7 ~ 0, TRUE ~ Gasera_CO2_flux_mgm2h)
  ) # Applying R2 < 0.7 ~ 0 rate correction to Gasera rates

Master_GHG_2023 <- Master_GHG_2023 %>% 
                    select(Row_Nr, Sampling_date, Plot, Treat, Code_Nr, Code, Rep, Chamber_type, Gasera_CH4_flux_mgm2h, Gasera_CH4_flux_mgm2h_cor, Gasera_N2O_flux_mgm2h, 
                           Gasera_N2O_flux_mgm2h_cor, Gasera_CO2_flux_mgm2h, Gasera_CO2_flux_mgm2h_cor,
                           Chrom_CH4_flux_corrected, Chrom_N2O_flux_corrected, Chrom_CO2_flux_corrected, Water_level_piezo, Water_level_ruler, Water_level_corr,
                           Temp_soil, Rice_cover_prop, Env_temp_initial, Env_temp_final)

# Including water level measured during Gasera samplings ("Water_level_gasera_ruler")

Gasera_field_2023 <- read.csv("data/Field_records_Gasera.csv", fileEncoding="latin1", na.strings=c("","NA")) %>%  # Field sheet with data collected during Gasera samplings
                      rename(Plot = Rep) %>% 
                      rename(Water_level_ruler_Gasera = Water_level)

Gasera_field_2023$Date <- as.Date(Gasera_field_2023$Date, format = "%Y-%m-%d")

Gasera_field_2023 <- Gasera_field_2023 %>%
                      filter(!(Date == "2023-08-31" & Plot == "P05" & Chamber_type == "DK")) # Removes this row due to no Gasera relults for this DK chamber

Master_GHG_2023 <- merge(Master_GHG_2023, Gasera_field_2023[, c("Date", "Plot", "Chamber_type", "Water_level_ruler_Gasera")], by.x = c("Sampling_date", "Plot", "Chamber_type"), 
                         by.y = c("Date", "Plot", "Chamber_type"), all.x = TRUE, all.y = TRUE)
Master_GHG_2023 <- Master_GHG_2023[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 25, 19, 20, 21, 22, 23, 24)] # Reorder columns

Master_GHG_2023 <- Master_GHG_2023[!(Master_GHG_2023 $Plot %in% c("P10","P11", "P12", "P13", "P14", "P15")), ] #Removes Rep 4 and Rep 5 (only sampled during first dates)

#  2. Water level calculations ####

# Defines water level among measurement method (piezometer or ruler during chrom/gasera samplings) according to specific cases, see each assignment condition below.
# Water level correction for season 2023 differs from 2022 due to the new "Water_level_ruler_Gasera" data collected during Gasera samplings (only Chromatography for 2022 season)

# a.i) Whenever there is a negative piezometer read, take it. This solves dates with non-logical measures (e.g.03.Julty, negative piezo and positive rulers...)
# a.ii) If there is a positive Water_level_ruler_Gasera value, take it.
# b) If Water_level_ruler_Gasera is absent or 0, and Water_level_ruler is positive, take Water_level_ruler.
# c) If both, Water_level_ruler_Gasera and Water_level_ruler, are either absent or 0, and there is a Water_level_piezo value, take Water_level_piezo. (doesn't apply to CON, see d.i)
# d) AWD & MSD: If both, Water_level_ruler_Gasera and Water_level_ruler, are either absent or 0, and Water_level_piezo is absent (if not then it's c), look for the closest negative 
#               Water_level_piezo to the Sampling_date of this row that also has the same Plot value of this row.
# e) CON: If Water_level_ruler is absent or 0 and Water_level_ruler_Gasera is absent or 0: look for the closest negative Water_level_piezo to this Sampling_date of this row, 
#               corresponding to the MSD plot within this repetition (block) 

## This "for in loop" works already to apply conditions to the Water_level_2023, but it takes time to run:

for (i in 1:length(Master_GHG_2023$Row_Nr)) {
              current_plot <- Master_GHG_2023$Plot[i]
              current_rep <- Master_GHG_2023$Rep[i]
              neg_piezo_indices <- which(Master_GHG_2023$Water_level_piezo <= 0 & Master_GHG_2023$Plot == current_plot)
              neg_piezo_indices_CON <- which(Master_GHG_2023$Water_level_piezo <= 0 & Master_GHG_2023$Rep == current_rep & Master_GHG_2023$Treat == "MSD")
              closest_neg_index <- ifelse(Master_GHG_2023$Treat[i] %in% c("AWD", "MSD"), 
                                          neg_piezo_indices[which.min(abs(as.numeric(Master_GHG_2023$Sampling_date[i] - 
                                                                                       Master_GHG_2023$Sampling_date[neg_piezo_indices])))], 1) # the ifelse (... , ... , 1) solves the "replacement of length zero" for cases CON / piezo: NA / ruler: 0
              closest_neg_index_CON <- neg_piezo_indices_CON[which.min(abs(as.numeric(Master_GHG_2023$Sampling_date[i] - Master_GHG_2023$Sampling_date[neg_piezo_indices_CON])))]
  
Master_GHG_2023$Water_level_corr[i] <- case_when(   # Applying conditions for all cases (Opening each ifelse() in different LHS ~ RHS functions):
    # a.i): 
              Master_GHG_2023$Water_level_piezo[i] < 0 ~ Master_GHG_2023$Water_level_piezo[i],
    # a.ii): 
              !is.na(Master_GHG_2023$Water_level_ruler_Gasera[i]) & Master_GHG_2023$Water_level_ruler_Gasera[i] != 0 ~ Master_GHG_2023$Water_level_ruler_Gasera[i],
    # b):
              (is.na(Master_GHG_2023$Water_level_ruler_Gasera[i]) | Master_GHG_2023$Water_level_ruler_Gasera[i] == 0) & Master_GHG_2023$Water_level_ruler[i] != 0 
              ~ Master_GHG_2023$Water_level_ruler[i],
    # c):
              (is.na(Master_GHG_2023$Water_level_ruler_Gasera[i]) | Master_GHG_2023$Water_level_ruler_Gasera[i] == 0) & 
                (is.na(Master_GHG_2023$Water_level_ruler[i]) | Master_GHG_2023$Water_level_ruler[i] == 0) & !is.na(Master_GHG_2023$Water_level_piezo[i]) ~ Master_GHG_2023$Water_level_piezo[i],
    # d.i):
              (is.na(Master_GHG_2023$Water_level_ruler_Gasera[i]) | Master_GHG_2023$Water_level_ruler_Gasera[i] == 0) & 
                (is.na(Master_GHG_2023$Water_level_ruler[i]) | Master_GHG_2023$Water_level_ruler[i] == 0) &  Master_GHG_2023$Treat[i] %in% c("AWD", "MSD") & 
                is.na(closest_neg_index) ~ Master_GHG_2023$Water_level_ruler_Gasera[i],
    # d.ii):
              (is.na(Master_GHG_2023$Water_level_ruler_Gasera[i]) | Master_GHG_2023$Water_level_ruler_Gasera[i] == 0) & 
                (is.na(Master_GHG_2023$Water_level_ruler[i]) | Master_GHG_2023$Water_level_ruler[i] == 0) &  Master_GHG_2023$Treat[i] %in% c("AWD", "MSD") & 
                !is.na(closest_neg_index) ~ Master_GHG_2023$Water_level_piezo[closest_neg_index],
    # e):
              (is.na(Master_GHG_2023$Water_level_ruler_Gasera[i]) | Master_GHG_2023$Water_level_ruler_Gasera[i] == 0) &  
                (is.na(Master_GHG_2023$Water_level_ruler[i]) | Master_GHG_2023$Water_level_ruler[i] == 0) &  
                Master_GHG_2023$Treat[i] %in% "CON" & 
                !is.na(closest_neg_index_CON) ~ Master_GHG_2023$Water_level_piezo[closest_neg_index_CON],
    
    TRUE ~ NA_real_
  )
}

# Adding info for missing water level data for CON plots, for mesocosm set up and for dates with double piezometer measurements (measuring before or after flooding or re-flooding):

# i.- 2023-06-22: Assigns mean level of MSD plots (10.33cm) to CON plots.

Master_GHG_2023 <- Master_GHG_2023 %>%
                    mutate(Water_level_corr = if_else(Sampling_date == "2023-06-22" & Treat == "CON", 10.33, Water_level_corr))

# ii.- 2023-10-23: Corrects to constant 5cm to all plots (which was maintained during the whole PH period).

Master_GHG_2023 <- Master_GHG_2023 %>%
                    mutate(Water_level_corr = if_else(Sampling_date == "2023-10-23", 5.0, Water_level_corr))

# iii.- Dates with double piezometer measurement:2023-06-12, 2023-06-19, 2023-06-19 and 2023-11-08.

Master_GHG_2023$Doub_piez <- 0
Master_GHG_2023 <- Master_GHG_2023 %>%
                    mutate(Doub_piez = if_else(Row_Nr %in% c(47, 53, 57, # 2023-06-12
                                           80, 86, 90, # 2023-06-19
                                           92, 97, 102, # 2023-06-19
                                           499, 506, 510 # 023-11-08
                            ), 1, Doub_piez)) # Adds a "1" to identify those dates with double piezometer measurement


# Column separating Growing Season (GS) and Post-Harvest (PH) - 2023-10-03 mesoosm was implemented, harvest was 2023-10-04 and 2023-10-05.

Master_GHG_2023 <- Master_GHG_2023 %>%
                    mutate(Season = if_else(Sampling_date < as.Date("2023-10-03"), "GS",
                                            if_else(Sampling_date >= "2023-10-03", "PH", NA)))

## Outputs:

write_xlsx(Master_GHG_2023, "outputs/CERESTRES_results/Master_GHG_2023.xlsx") # Excel file with Master_GHG_2023.
save(Master_GHG_2023, file = "outputs/CERESTRES_results/Master_GHG_2023.RData") # Saves the Master_GHG_2023 dataframe to open with other R projects/scripts

#  3. Cumulative emissions & GWP ####

# All previous rates and concentrations in CH4-C, N2O-N and CO2-C. For GWP, units are conventionally: CO2-eq, so rates and concentrations must be transformed (for CH4-C and N2O-N) 
# before calculating GWP. From now onward, rates, cumulative emissions and GWP distinguish in between CCH4 and CH4, N2O and NN2O, and CO2 and CCO2.

# Data frame for GS And Total cumulative emission calculations:

Acc_CHROM <- Master_GHG_2023 %>% # New df with all Chromatography results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
              filter(!is.na(Chrom_CH4_flux_corrected)) %>% 
              select("Sampling_date", "Treat", "Plot", "Rep", "Season", "Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected", "Chrom_CO2_flux_corrected") %>% 
              arrange(Plot, Sampling_date) %>% 
              rename(Chrom_CCH4_flux_corrected = Chrom_CH4_flux_corrected, Chrom_NN2O_flux_corrected = Chrom_N2O_flux_corrected, Chrom_CCO2_flux_corrected = Chrom_CO2_flux_corrected) %>% 
              group_by(Plot) %>% 
              mutate(Days_passed = as.integer(Sampling_date - lag(Sampling_date))) %>% 
              mutate(Hours_passed = Days_passed * 24) %>% 
              mutate(CCH4_mgm2 = Chrom_CCH4_flux_corrected * Hours_passed, # *10.000 from m-2 to ha-1 ; /1.000.000 from mg to kg
                     NN2O_mgm2 = Chrom_NN2O_flux_corrected * Hours_passed,
                     CCO2_mgm2 = Chrom_CCO2_flux_corrected * Hours_passed,
                     CCH4_kgha = CCH4_mgm2 / 100,
                     NN2O_kgha = NN2O_mgm2 / 100,
                     CCO2_kgha = CCO2_mgm2 / 100) %>% 
              mutate(Chrom_CH4_flux_transformed = Chrom_CCH4_flux_corrected / ((12/16)^2),
                     Chrom_N2O_flux_transformed = Chrom_NN2O_flux_corrected / ((28/44) * (14/44)),
                     Chrom_CO2_flux_transformed = Chrom_CCO2_flux_corrected / ((12/44)^2),
                     CH4_mgm2 = Chrom_CH4_flux_transformed * Hours_passed, # *10.000 from m-2 to ha-1 ; /1.000.000 from mg to kg
                     N2O_mgm2 = Chrom_N2O_flux_transformed * Hours_passed,
                     CO2_mgm2 = Chrom_CO2_flux_transformed * Hours_passed,
                     CH4_kgha = CH4_mgm2 / 100,
                     N2O_kgha = N2O_mgm2 / 100,
                     CO2_kgha = CO2_mgm2 / 100)

Acc_CHROM$CCH4_kgha <- ifelse(is.na(Acc_CHROM$CCH4_kgha), 0, Acc_CHROM$CCH4_kgha)
Acc_CHROM$NN2O_kgha <- ifelse(is.na(Acc_CHROM$NN2O_kgha), 0, Acc_CHROM$NN2O_kgha)  
Acc_CHROM$CCO2_kgha <- ifelse(is.na(Acc_CHROM$CCO2_kgha), 0, Acc_CHROM$CCO2_kgha) 
Acc_CHROM$CH4_kgha <- ifelse(is.na(Acc_CHROM$CH4_kgha), 0, Acc_CHROM$CH4_kgha)
Acc_CHROM$N2O_kgha <- ifelse(is.na(Acc_CHROM$N2O_kgha), 0, Acc_CHROM$N2O_kgha)  
Acc_CHROM$CO2_kgha <- ifelse(is.na(Acc_CHROM$CO2_kgha), 0, Acc_CHROM$CO2_kgha)

# Data frame for PH cumulative emission calculations:

Acc_CHROM_PH <- Master_GHG_2023 %>% # New df with all Chromatography results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                filter(!is.na(Chrom_CH4_flux_corrected), Season == "PH") %>% 
                select("Sampling_date", "Treat", "Plot", "Rep", "Season", "Chrom_CH4_flux_corrected", "Chrom_N2O_flux_corrected", "Chrom_CO2_flux_corrected") %>% 
                arrange(Plot, Sampling_date) %>% 
                rename(Chrom_CCH4_flux_corrected = Chrom_CH4_flux_corrected, Chrom_NN2O_flux_corrected = Chrom_N2O_flux_corrected, Chrom_CCO2_flux_corrected = Chrom_CO2_flux_corrected) %>% 
                group_by(Plot) %>% 
                mutate(Days_passed = as.integer(Sampling_date - lag(Sampling_date))) %>% 
                mutate(Hours_passed = Days_passed * 24) %>% 
                mutate(CCH4_mgm2 = Chrom_CCH4_flux_corrected * Hours_passed, # *10.000 from m-2 to ha-1 ; /1.000.000 from mg to kg
                       NN2O_mgm2 = Chrom_NN2O_flux_corrected * Hours_passed,
                       CCO2_mgm2 = Chrom_CCO2_flux_corrected * Hours_passed,
                       CCH4_kgha = CCH4_mgm2 / 100,
                       NN2O_kgha = NN2O_mgm2 / 100,
                       CCO2_kgha = CCO2_mgm2 / 100) %>% 
                mutate(Chrom_CH4_flux_transformed = Chrom_CCH4_flux_corrected / ((12/16)^2),
                       Chrom_N2O_flux_transformed = Chrom_NN2O_flux_corrected / ((28/44) * (14/44)),
                       Chrom_CO2_flux_transformed = Chrom_CCO2_flux_corrected / ((12/44)^2),
                       CH4_mgm2 = Chrom_CH4_flux_transformed * Hours_passed, # *10.000 from m-2 to ha-1 ; /1.000.000 from mg to kg
                       N2O_mgm2 = Chrom_N2O_flux_transformed * Hours_passed,
                       CO2_mgm2 = Chrom_CO2_flux_transformed * Hours_passed,
                       CH4_kgha = CH4_mgm2 / 100,
                       N2O_kgha = N2O_mgm2 / 100,
                       CO2_kgha = CO2_mgm2 / 100) 

Acc_CHROM_PH$CCH4_kgha <- ifelse(is.na(Acc_CHROM_PH$CCH4_kgha), 0, Acc_CHROM_PH$CCH4_kgha)
Acc_CHROM_PH$NN2O_kgha <- ifelse(is.na(Acc_CHROM_PH$NN2O_kgha), 0, Acc_CHROM_PH$NN2O_kgha)  
Acc_CHROM_PH$CCO2_kgha <- ifelse(is.na(Acc_CHROM_PH$CCO2_kgha), 0, Acc_CHROM_PH$CCO2_kgha) 
Acc_CHROM_PH$CH4_kgha <- ifelse(is.na(Acc_CHROM_PH$CH4_kgha), 0, Acc_CHROM_PH$CH4_kgha)
Acc_CHROM_PH$N2O_kgha <- ifelse(is.na(Acc_CHROM_PH$N2O_kgha), 0, Acc_CHROM_PH$N2O_kgha)  
Acc_CHROM_PH$CO2_kgha <- ifelse(is.na(Acc_CHROM_PH$CO2_kgha), 0, Acc_CHROM_PH$CO2_kgha)

# Sum of all plot emission according to treatments:

# i) Summarized df for total cumulative emissions and GWP plots:

Acc_CHROM_tot_sum <- Acc_CHROM %>%
                    group_by(Treat, Plot) %>%
                    summarise(CH4_kgha_tot = sum(CH4_kgha),
                              N2O_kgha_tot = sum(N2O_kgha),
                              CO2_kgha_tot = sum(CO2_kgha),
                              CCH4_kgha_tot = sum(CCH4_kgha),
                              NN2O_kgha_tot = sum(NN2O_kgha),
                              CCO2_kgha_tot = sum(CCO2_kgha)) 

Acc_CHROM_tot_sum$Treat <- factor(Acc_CHROM_tot_sum$Treat, levels = c('CON', 'MSD', 'AWD')) # Treat to factor to reorder ggplot x axis

Acc_CHROM_tot_sum$GWP <- (Acc_CHROM_tot_sum$CH4_kgha_tot * 25) + (Acc_CHROM_tot_sum$N2O_kgha_tot * 298) # Factors from "IPCC, 2007 - The Physical Science Basis"

# averaging previous total cumulative emissions df (to plot averaged GWP per Treat, so only CH4 and N2O are considered):

Avg_Acc_CHROM_tot_sum <- Acc_CHROM_tot_sum %>% 
                          group_by(Treat) %>%
                          summarize(mean_GWP = mean(GWP), se_GWP = sd(GWP) / sqrt(n()),
                                    mean_CH4_kgha_tot = mean(CH4_kgha_tot), se_CH4_kgha_tot = sd(CH4_kgha_tot) / sqrt(n()),
                                    mean_N2O_kgha_tot = mean(N2O_kgha_tot), se_N2O_kgha_tot = sd(N2O_kgha_tot) / sqrt(n()),
                                    mean_CCH4_kgha_tot = mean(CCH4_kgha_tot), se_CCH4_kgha_tot = sd(CCH4_kgha_tot) / sqrt(n()),
                                    mean_NN2O_kgha_tot = mean(NN2O_kgha_tot), se_NN2O_kgha_tot = sd(NN2O_kgha_tot) / sqrt(n()))

# ii) Summarized df for GS cumulative emissions plot:

Acc_CHROM_GS_sum <- Acc_CHROM %>%
                    filter(Season == "GS") %>% 
                    group_by(Treat, Plot) %>%
                    summarise(CH4_kgha_tot = sum(CH4_kgha),
                              N2O_kgha_tot = sum(N2O_kgha),
                              CO2_kgha_tot = sum(CO2_kgha),
                              CCH4_kgha_tot = sum(CCH4_kgha),
                              NN2O_kgha_tot = sum(NN2O_kgha),
                              CCO2_kgha_tot = sum(CCO2_kgha)) 

Acc_CHROM_GS_sum$Treat <- factor(Acc_CHROM_GS_sum$Treat, levels = c('CON', 'MSD', 'AWD')) # Treat to factor to reorder ggplot x axis

Acc_CHROM_GS_sum$GWP <- (Acc_CHROM_GS_sum$CH4_kgha_tot * 25) + (Acc_CHROM_GS_sum$N2O_kgha_tot * 298) # Factors from "IPCC, 2007 - The Physical Science Basis"

# averaging previous total cumulative emissions df (to plot averaged GWP per Treat, so only CH4 and N2O are considered):

Avg_Acc_CHROM_GS_sum <- Acc_CHROM_GS_sum %>% 
                          group_by(Treat) %>%
                          summarize(mean_GWP = mean(GWP), se_GWP = sd(GWP) / sqrt(n()),
                                    mean_CH4_kgha_tot = mean(CH4_kgha_tot), se_CH4_kgha_tot = sd(CH4_kgha_tot) / sqrt(n()),
                                    mean_N2O_kgha_tot = mean(N2O_kgha_tot), se_N2O_kgha_tot = sd(N2O_kgha_tot) / sqrt(n()),
                                    mean_CCH4_kgha_tot = mean(CCH4_kgha_tot), se_CCH4_kgha_tot = sd(CCH4_kgha_tot) / sqrt(n()),
                                    mean_NN2O_kgha_tot = mean(NN2O_kgha_tot), se_NN2O_kgha_tot = sd(NN2O_kgha_tot) / sqrt(n()))

# iv) Summarized df for PH cumulative emissions plot:

Acc_CHROM_PH_sum <- Acc_CHROM_PH %>%
                    group_by(Treat, Plot) %>%
                    summarise(CH4_kgha_tot = sum(CH4_kgha),
                              N2O_kgha_tot = sum(N2O_kgha),
                              CO2_kgha_tot = sum(CO2_kgha),
                              CCH4_kgha_tot = sum(CCH4_kgha),
                              NN2O_kgha_tot = sum(NN2O_kgha),
                              CCO2_kgha_tot = sum(CCO2_kgha)) 

Acc_CHROM_PH_sum$Treat <- factor(Acc_CHROM_PH_sum$Treat, levels = c('CON', 'MSD', 'AWD')) # Treat to factor to reorder ggplot x axis

Acc_CHROM_PH_sum$GWP <- (Acc_CHROM_PH_sum$CH4_kgha_tot * 25) + (Acc_CHROM_PH_sum$N2O_kgha_tot * 298) # Factors from "IPCC, 2007 - The Physical Science Basis"

# averaging previous total cumulative emissions df (to plot averaged GWP per Treat, so only CH4 and N2O are considered):

Avg_Acc_CHROM_PH_sum <- Acc_CHROM_PH_sum %>% 
                          group_by(Treat) %>%
                          summarize(mean_GWP = mean(GWP), se_GWP = sd(GWP) / sqrt(n()),
                                    mean_CH4_kgha_tot = mean(CH4_kgha_tot), se_CH4_kgha_tot = sd(CH4_kgha_tot) / sqrt(n()),
                                    mean_N2O_kgha_tot = mean(N2O_kgha_tot), se_N2O_kgha_tot = sd(N2O_kgha_tot) / sqrt(n()),
                                    mean_CCH4_kgha_tot = mean(CCH4_kgha_tot), se_CCH4_kgha_tot = sd(CCH4_kgha_tot) / sqrt(n()),
                                    mean_NN2O_kgha_tot = mean(NN2O_kgha_tot), se_NN2O_kgha_tot = sd(NN2O_kgha_tot) / sqrt(n()))

# 4. Yield ####

Yield_2023 <- read.csv("data/Yield_2023.csv", fileEncoding="latin1", na.strings=c("","NA")) 
