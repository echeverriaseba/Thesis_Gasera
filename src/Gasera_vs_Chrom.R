
################################################### Gasera vs Chromatography - CERESTRES 2023 #########################################################

library(tidyverse)
library(ggplot2)
library(zoo)
library(writexl)
library(gridExtra)
library(cowplot)

# Preparing data ####

## Importing Chromatography results from Rproject: Chromatography_results:

load("C:/Users/SECHEVERRIA/R_git/Thesis_Chromatography/outputs/2023/Rates_corrected/Emission_rates_w_corrections_2023.RData")
load("C:/Users/SECHEVERRIA/R_git/Thesis_Gasera/outputs/CERESTRES_results/Gasera_emission_rates_2023.RData")

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

Master_GHG_2023  <- merge(Emission_rates_w_corrections_2023, Water_level_2023, by.x = c("Sampling_date", "Treat", "Plot", "Rep"), by.y = c("Date", "Treat", "Plot", "Rep"),
                          all.x= TRUE, all.y = TRUE) %>%  # with all.x= TRUE, all.y = TRUE, the resulting dataframe contains as well dates where either only water level or emissions were recorded.
                          select(Sampling_date, Treat, Plot, Rep, Chrom_CH4_flux_corrected, Chrom_N2O_flux_corrected, Water_level_piezo)

Field_sheet_chrom_2023 <- read.csv("data/Field_sheet_chrom_2023.csv", fileEncoding="latin1", na.strings=c("","NA"))  # Load chromatography field sheet 2023
                          # rename(Water_level_piezo = Water_level_cm)

Field_sheet_other_factors_2023 <- Field_sheet_chrom_2023 %>% 
                                  group_by(Sampling_date, Plot) %>% 
                                  summarise(Water_level_ruler = mean(Water_level_cm), Temp_soil = mean(Temp_soil), Rice_cover_prop = mean(Rice_cover_prop), 
                                            Env_temp_initial = mean(Env_temp_initial), 
                                            Env_temp_final = mean(Env_temp_final)) # creates dataframe with all factors measured during chromatography campaign dates, besides GHG emissions.

Field_sheet_other_factors_2023$Sampling_date <- as.Date(Field_sheet_other_factors_2023$Sampling_date) # Converting to date format

## Merging Gasera and Chromatography emission rate dataframes:
Master_GHG_2023 <- merge(Gasera_emission_rates_2023, Master_GHG_2023, by.x = c("Date", "Plot", "Treat"), by.y = c("Sampling_date", "Plot", "Treat"), all.x = TRUE)

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
                    select(Row_Nr, Sampling_date, Plot, Treat, Code_Nr, Code, Rep, Chamber_type, Gasera_CH4_flux_mgm2h, Gasera_N2O_flux_mgm2h, Gasera_CO2_flux_mgm2h,
                           Chrom_CH4_flux_corrected, Chrom_N2O_flux_corrected, Chrom_N2O_flux_corrected, Water_level_piezo, Water_level_ruler, Water_level_corr,
                           Temp_soil, Rice_cover_prop, Env_temp_initial, Env_temp_final)

# Including water level measured during Gasera samplings ("Water_level_gasera_ruler")

Gasera_field_2023 <- read.csv("data/Field_records_Gasera.csv", fileEncoding="latin1", na.strings=c("","NA")) %>%  # Field sheet with data collected during Gasera samplings
                              rename(Plot = Rep) %>% 
                              rename(Water_level_ruler_Gasera = Water_level)

Master_GHG_2023 <- merge(Master_GHG_2023, Gasera_field_2023[, c("Date", "Plot", "Water_level_ruler_Gasera")], by.x = c("Sampling_date", "Plot"), by.y = c("Date", "Plot"))
Master_GHG_2023 <- Master_GHG_2023[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 21, 16, 17, 18, 19, 20)] # Reorder columns

#  Water level calculations ####

Water_level_2023$Date <- as.Date(Water_level_2023$Date)

Chrom_water_level <- Field_sheet_chrom_2023 %>% 
                                  group_by(Sampling_date, Plot, Rep, Tr1) %>% 
                                  summarise(Water_level_ruler = mean(Water_level_cm)) %>%  # creates dataframe with water level measured with ruler during chrom. samplings.
                                  rename(Treat = Tr1)

Chrom_water_level$Sampling_date <- as.Date(Chrom_water_level$Sampling_date) # Converting to date format

Water_level_2023 <- merge(Water_level_2023, Chrom_water_level[, c("Sampling_date", "Plot",  "Rep", "Treat", "Water_level_ruler")], by.x = c("Date", "Plot", "Rep", "Treat"),
                          by.y = c("Sampling_date", "Plot", "Rep", "Treat"), all.x = TRUE, all.y = TRUE)  
              
Water_level_2023 <- merge(Water_level_2023, Gasera_field_2023[, c("Date", "Plot", "Water_level_ruler_Gasera")], by = c("Date", "Plot"), all = TRUE) %>% 
  select(Date, Plot, Rep, Treat, Water_level_piezo, Water_level_ruler, Water_level_ruler_Gasera) %>% 
  rename(Sampling_date = Date)

Water_level_2023 <- Water_level_2023[!(Water_level_2023 $Plot %in% c("P10","P11", "P12", "P13", "P14", "P15")), ] #Removes Rep 4 and Rep 5 (only sampled during first dates)
Water_level_2023$Water_level_corr <- NA # add column "Water_level_corr"
Water_level_2023$Row_Nr <- 1:nrow(Water_level_2023) # Adding a row number column
Water_level_2023 <- Water_level_2023[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)] # Reorder columns

##  Water level correction ####

# Defines water level among measurement method (piezometer or ruler during chrom/gasera samplings) according to specific cases, see each assignment condition below.
# Water level correction for season 2023 differs from 2022 due to the new "Water_level_ruler_Gasera" data collected during Gasera samplings (only Chromatography for 2022 season)

# a.i) Whenever there is a negative piezometer read, take it. This solves dates with non-logical measures (e.g.03.Julty, negative piezo and positive rulers...)
# a.ii) If there is a positive Water_level_ruler_Gasera value, take it.
# b) If Water_level_ruler_Gasera is absent or 0, and Water_level_ruler is positive, take Water_level_ruler.
# c) If both, Water_level_ruler_Gasera and Water_level_ruler, are either absent or 0, and there is a Water_level_piezo value, take Water_level_piezo. (doesn't apply to CON, see d.i)
# d) AWD & MSD: If both, Water_level_ruler_Gasera and Water_level_ruler, are either absent or 0, and Water_level_piezo is absent (if not then it's c), look for the closest negative Water_level_piezo to the Sampling_date of this row that also has the same Plot value of this row.
# e) CON: 
 # e.i) If Water_level_ruler is 0 and Water_level_ruler_Gasera is absent: look for the closest negative Water_level_piezo to this Sampling_date of this row, corresponding to the MSD plot within this repetition (block) 
 # e.ii) If Water_level_ruler is absent and Water_level_ruler_Gasera is 0: look for the closest negative Water_level_piezo to this Sampling_date of this row, corresponding to the MSD plot within this repetition (block)
 # e.iii) If both, Water_level_ruler_Gasera and Water_level_ruler, are absent: NA. This applies only to 2023-06-22, when (according to piezo data) all plots where avg. 12 cm.CON < 0 is from 2023-06-26 to 2023-06-28 is due to herbicide application.  

## This "for in loop" works already to apply conditions to the Water_level_2023, but it takes time to run:

for (i in 1:length(Water_level_2023$Row_Nr)) {
                current_plot <- Water_level_2023$Plot[i]
                current_rep <- Water_level_2023$Rep[i]
                neg_piezo_indices <- which(Water_level_2023$Water_level_piezo <= 0 & Water_level_2023$Plot == current_plot)
                neg_piezo_indices_CON <- which(Water_level_2023$Water_level_piezo <= 0 & Water_level_2023$Rep == current_rep & Water_level_2023$Treat == "MSD")
                closest_neg_index <- ifelse(Water_level_2023$Treat[i] %in% c("AWD", "MSD"), 
                                      neg_piezo_indices[which.min(abs(as.numeric(Water_level_2023$Sampling_date[i] - 
                                      Water_level_2023$Sampling_date[neg_piezo_indices])))], 1) # the ifelse (... , ... , 1) solves the "replacement of length zero" for cases CON / piezo: NA / ruler: 0
                closest_neg_index_CON <- neg_piezo_indices_CON[which.min(abs(as.numeric(Water_level_2023$Sampling_date[i] - Water_level_2023$Sampling_date[neg_piezo_indices_CON])))]
  
# Applying conditions for all cases (Opening each ifelse() in different LHS ~ RHS functions):
  
Water_level_2023$Water_level_corr[i] <- case_when(
  # a.i): 
            Water_level_2023$Water_level_piezo[i] < 0 ~ Water_level_2023$Water_level_piezo[i],
  # a.ii): 
            !is.na(Water_level_2023$Water_level_ruler_Gasera[i]) & Water_level_2023$Water_level_ruler_Gasera[i] != 0 ~ Water_level_2023$Water_level_ruler_Gasera[i],
  # b):
            (is.na(Water_level_2023$Water_level_ruler_Gasera[i]) | Water_level_2023$Water_level_ruler_Gasera[i] == 0) & Water_level_2023$Water_level_ruler[i] != 0 
            ~ Water_level_2023$Water_level_ruler[i],
  # c):
            (is.na(Water_level_2023$Water_level_ruler_Gasera[i]) | Water_level_2023$Water_level_ruler_Gasera[i] == 0) & 
              (is.na(Water_level_2023$Water_level_ruler[i]) | Water_level_2023$Water_level_ruler[i] == 0) & !is.na(Water_level_2023$Water_level_piezo[i]) ~ Water_level_2023$Water_level_piezo[i],
  # d) Part I:
            (is.na(Water_level_2023$Water_level_ruler_Gasera[i]) | Water_level_2023$Water_level_ruler_Gasera[i] == 0) & 
              (is.na(Water_level_2023$Water_level_ruler[i]) | Water_level_2023$Water_level_ruler[i] == 0) &  Water_level_2023$Treat[i] %in% c("AWD", "MSD") & 
              is.na(closest_neg_index) ~ Water_level_2023$Water_level_ruler_Gasera[i],
  # d) Part II:
            (is.na(Water_level_2023$Water_level_ruler_Gasera[i]) | Water_level_2023$Water_level_ruler_Gasera[i] == 0) & 
              (is.na(Water_level_2023$Water_level_ruler[i]) | Water_level_2023$Water_level_ruler[i] == 0) &  Water_level_2023$Treat[i] %in% c("AWD", "MSD") & 
              !is.na(closest_neg_index) ~ Water_level_2023$Water_level_piezo[closest_neg_index],
  # e.i)
            is.na(Water_level_2023$Water_level_ruler_Gasera[i]) & Water_level_2023$Water_level_ruler[i] == 0 &  Water_level_2023$Treat[i] %in% "CON" & 
              !is.na(closest_neg_index_CON) ~ Water_level_2023$Water_level_piezo[closest_neg_index_CON], 
  # e.ii)
            is.na(Water_level_2023$Water_level_ruler[i]) & Water_level_2023$Water_level_ruler_Gasera[i] == 0 &  Water_level_2023$Treat[i] %in% "CON" & 
              !is.na(closest_neg_index_CON) ~ Water_level_2023$Water_level_piezo[closest_neg_index_CON], 
  # e.iii)
            is.na(Water_level_2023$Water_level_ruler[i]) & is.na(Water_level_2023$Water_level_ruler_Gasera[i]) &  Water_level_2023$Treat[i] %in% "CON" &
              !is.na(closest_neg_index_CON) ~ NA,
  
  # e.iv)
            Water_level_2023$Water_level_ruler[i] == 0 & Water_level_2023$Water_level_ruler_Gasera[i] == 0 & Water_level_2023$Treat[i] %in% "CON" & 
              !is.na(closest_neg_index_CON) ~ Water_level_2023$Water_level_piezo[closest_neg_index_CON], 
  
  ## Test 14.03.24:
  
  (is.na(Water_level_2023$Water_level_ruler[i]) | Water_level_2023$Water_level_ruler[i] == 0) & Water_level_2023$Water_level_ruler_Gasera[i] == 0 & is.na(Water_level_2023$Water_level_piezo[i]) 
  & Water_level_2023$Treat[i] %in% "CON" & is.na(closest_neg_index_CON) ~ Water_level_2023$Water_level_ruler[i], # d.ii) part I
  
  
  (is.na(Water_level_2023$Water_level_ruler[i]) | Water_level_2023$Water_level_ruler[i] == 0) & Water_level_2023$Water_level_ruler_Gasera[i] == 0 & is.na(Water_level_2023$Water_level_piezo[i])
  & Water_level_2023$Treat[i] %in% "CON" & !is.na(closest_neg_index_CON) ~ Water_level_2023$Water_level_piezo[closest_neg_index_CON], # d.ii) part II
  
  TRUE ~ NA_real_
)
}

# Adding info for missing water level data for CON plots and for mesocosm set up:
# 2023-06-22: Assigns mean level of MSD plots (10.33cm) to CON plots.
# 2023-03-10: Day of mesocosm setting (all plots flooded up to app 5cm), assigns this to all plots

CON_water_miss <- data.frame(Sampling_date = c(rep("2023-06-22",times=3), rep(c("2023-10-03"),times=9)),
                             Plot = c("P03",  "P06", "P08", rep(c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09"),times=1)),
                             Rep = c(1, 2, 3, rep(c(1, 1, 1, 2, 2, 2, 3, 3, 3),times=1)),
                             Treat = c(rep("CON",times=3), rep(c("AWD", "MSD", "CON", "MSD", "AWD", "CON", "MSD", "CON", "AWD"),times=1)),
                             Water_level_corr = c(10.33, 10.33, 10.33, rep(c(5.0),times=9)))

CON_water_miss$Row_Nr <- NA
CON_water_miss$Water_level_piezo <- NA
CON_water_miss$Water_level_ruler <- NA
CON_water_miss$Water_level_ruler_Gasera <- NA

CON_water_miss$Sampling_date <- as.Date(CON_water_miss$Sampling_date)

Water_level_2023 <- Water_level_2023 %>%
                      rbind(CON_water_miss) %>% 
                      arrange(Sampling_date, Plot)

write_xlsx(Water_level_2023, "outputs/Water_level_2023.xlsx")

save(Water_level_2023, file = "outputs/Water_level_2023.RData")

# Plotting water level ####

Avg_water_level2 <- Water_level_2023 %>% 
                          group_by(Sampling_date, Treat) %>% 
                          summarize(avg_Water_level_corr = mean(Water_level_corr, na.rm = TRUE))

Avg_water_level2$avg_Water_level_corr[is.na(Avg_water_level2$avg_Water_level_corr)] <- NA # Replace NaN for NA values

pdf('outputs/CERESTRES_results/Water_plot_2023.pdf', width = 12)

Water_plot_2023 <- ggplot(Avg_water_level2, aes(x = Sampling_date, color = Treat, linetype = avg_Water_level_corr)) +
                          geom_line(aes(y = avg_Water_level_corr, linetype = "avg_Water_level_corr", color = Treat), show.legend = FALSE) +
                          scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                          theme_bw() +
                          labs(y = "Water level (cm)") +
                          labs(x = "Sampling date") +
                          geom_hline(yintercept=0, color = "grey") +
                          scale_linetype_manual(name = "avg_Water_level_corr",
                                                values = c("avg_Water_level_corr" = "dashed"), labels = "Water level (cm)") +
                          guides(linetype = guide_legend(override.aes = list(color = "black"))) +
                          theme(axis.title.y = element_text(color = "black"), 
                                axis.text.y = element_text(color = "black"),
                                axis.title.y.right = element_text(color = "black"),
                                axis.text.y.right = element_text(color = "black"), strip.background = element_blank(),
                                strip.placement = "outside",
                                plot.margin = unit(c(0, 1, 1, 1), "lines"))+
                          scale_x_date(date_breaks = "14 day", date_labels = "%m.%d")

print(Water_plot_2023) # Water level all treats

dev.off()

# Ploting Emissions ####

# 1. Using all calculated rates - No corrections according to the R2 >= 0.7 requirement ###########################################

## 1.1. Considering only TR chambers for Gasera data #####

## Filter to consider only TR Gasera chambers and average rates from Gasera and Chromatography:
Avg_rates_compare_TR2 <- Master_GHG_2023 %>% 
                          filter(Chamber_type == "TR") %>% 
                          group_by(Sampling_date, Treat) %>% 
                          summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE),
                                    avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE))

Avg_rates_compare_TR2$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TR2$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR2$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_TR2$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values

write_xlsx(Avg_rates_compare_TR2, "outputs/CERESTRES_results/Gasera_vs_Chromat/Avg_rates_compare_TR2.xlsx")

## Plot rates across time:

### TR - CH4: ####

# I.CHROM and GASERA in separate plots:

# I.a. CHROM: 

Avg_rates_compare_TR2_CHROM <- Avg_rates_compare_TR2 %>% 
                                filter(!is.na(avg_Chrom_CH4_flux_mgm2h))

Master_GHG_2023_CHROM <- Master_GHG_2023 %>% 
                          filter(!is.na(Chrom_CH4_flux_corrected))

Rates_vs_time_CH4_CHROM <- ggplot(data = Avg_rates_compare_TR2_CHROM, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CH4_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_CHROM, aes(x = Sampling_date, y = Chrom_CH4_flux_corrected, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h)) + #, linetype = "Average CH4 Flux - Chromatography")) +   , na.rm = TRUE) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Chromatography - ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    ggtitle("CH4 Emission rates Gasera vs. Chromatography") +
                                    scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                    theme(
                                      axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                      axis.text.y = element_text(color = "black"),
                                      axis.title.y.right = element_text(color = "black"),
                                      axis.text.y.right = element_text(color = "black"),
                                      strip.background = element_blank(),
                                      strip.placement = "outside",
                                      legend.text = element_text(size = 12),
                                      legend.title = element_text(size = 12),
                                      axis.text.x = element_blank(),
                                      legend.position="top",
                                      plot.margin = unit(c(1, 1, 0, 1.3), "lines")) +
                                    xlab(NULL)
                                  
print(Rates_vs_time_CH4_CHROM)

# I.b. GASERA:

Avg_rates_compare_TR2_GASERA <- Avg_rates_compare_TR2 %>% 
                                filter(!is.na(avg_Gasera_CH4_flux_mgm2h))

Master_GHG_2023_GASERA_TR <- Master_GHG_2023 %>% 
                          filter(Chamber_type == "TR") %>% 
                          filter(!is.na(Gasera_CH4_flux_mgm2h))

Rates_vs_time_CH4_GASERA <- ggplot(data = Avg_rates_compare_TR2_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CH4_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CH4_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h)) + #, linetype = "Average CH4 Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera - ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                    theme(
                                      axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                      axis.text.y = element_text(color = "black"),
                                      axis.title.y.right = element_text(color = "black"),
                                      axis.text.y.right = element_text(color = "black"),
                                      strip.background = element_blank(),
                                      strip.placement = "outside",
                                      legend.position="none",
                                      plot.margin = unit(c(0, 1, 0, 1), "lines")) +
                                    xlab(NULL)

print(Rates_vs_time_CH4_GASERA)

# I.c. Arrange both plots:

Rates_vs_time_CH4_methods <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Rates_vs_time_CH4_GASERA, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/First_plot_tests/Rates_vs_time_CH4_methods1.pdf", width = 20, height = 12, plot = Rates_vs_time_CH4_methods)

#  II. Gasera and Chrom in one plot: 

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TR2.pdf', width = 12)

Rates_vs_time_CH4_compare_TR2 <- ggplot(data = Avg_rates_compare_TR2, aes(color = Treat, x = Sampling_date, y = avg_Gasera_CH4_flux_mgm2h, group = Treat)) +
                                        geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Gasera")) +
                                        geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Chromatography")) +  # , na.rm = TRUE) +
                                        scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                        scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                        theme_bw() +
                                        labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                        xlab("Time") +
                                        ggtitle("CH4 Emission rates Gasera vs. Chromatography") +
                                        scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                        guides(linetype = guide_legend(override.aes = list(color = c("black", "black")))) +
                                        theme(
                                          axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                          axis.text.y = element_text(color = "black"),
                                          axis.title.y.right = element_text(color = "black"),
                                          axis.text.y.right = element_text(color = "black"),
                                          strip.background = element_blank(),
                                          strip.placement = "outside",
                                          legend.text = element_text(size = 7),
                                          legend.title = element_text(size = 7),
                                          # axis.text.x = element_blank(),
                                          legend.position="top",
                                          plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                        xlab(NULL)
                                        # theme(plot.title = element_text(hjust = 0.5)) +
                                        # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CH4_compare_TR2)

dev.off()

# Arrange plots:

# Gasera and Chrom in one plot, arranged with water plot:

Rates_vs_time_CH4_compare_TR2_water <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_compare_TR2, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TR2_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CH4_compare_TR2_water) # Arrange CH4 emissions + Water level all treats

### TR - N2O: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_N2O_compare_TR.pdf', width = 12)

Rates_vs_time_N2O_compare_TR <- ggplot(data = Avg_rates_compare_TR2, aes(color = Treat, x = Sampling_date, y = avg_Gasera_N2O_flux_mgm2h, group = Treat)) +
                                        geom_line(aes(y = avg_Gasera_N2O_flux_mgm2h, linetype = "Average N2O Flux - Gasera")) +
                                        geom_line(aes(y = avg_Chrom_N2O_flux_mgm2h, linetype = "Average N2O Flux - Chromatography"), na.rm = TRUE) +
                                        scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                        scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                        theme_bw() +
                                        labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                        # xlab("Time") +
                                        ggtitle("N2O Emission rates Gasera vs. Chromatography") +
                                        scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                        theme(
                                          axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                          axis.text.y = element_text(color = "black"),
                                          axis.title.y.right = element_text(color = "black"),
                                          axis.text.y.right = element_text(color = "black"),
                                          strip.background = element_blank(),
                                          strip.placement = "outside",
                                          legend.text = element_text(size = 7),
                                          legend.title = element_text(size = 7),
                                          axis.text.x = element_blank(),
                                          legend.position="top",
                                          plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                        xlab(NULL)
                                        # theme(plot.title = element_text(hjust = 0.5)) +
                                        # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_N2O_compare_TR)

dev.off()

# Arrange plots:

Rates_vs_time_N2O_compare_TR_water <- grid.arrange(arrangeGrob(Rates_vs_time_N2O_compare_TR, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_N2O_compare_TR_water.pdf", width = 7, height = 5, plot = Rates_vs_time_N2O_compare_TR_water) # Arrange N2O emissions + Water level all treats

### TR - CO2: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CO2_compare_TR.pdf', width = 12)

Rates_vs_time_CO2_compare_TR <- ggplot(data = Avg_rates_compare_TR2, aes(color = Treat, x = Sampling_date, y = avg_Gasera_CO2_flux_mgm2h, group = Treat)) +
                                        geom_line(aes(y = avg_Gasera_CO2_flux_mgm2h, linetype = "Average CO2 Flux - Gasera")) +
                                        geom_line(aes(y = avg_Chrom_CO2_flux_mgm2h, linetype = "Average CO2 Flux - Chromatography"), na.rm = TRUE) +
                                        scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                        scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                        theme_bw() +
                                        labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                        # xlab("Time") +
                                        ggtitle("CO2 Emission rates Gasera vs. Chromatography") +
                                        scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                        theme(
                                          axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                          axis.text.y = element_text(color = "black"),
                                          axis.title.y.right = element_text(color = "black"),
                                          axis.text.y.right = element_text(color = "black"),
                                          strip.background = element_blank(),
                                          strip.placement = "outside",
                                          legend.text = element_text(size = 7),
                                          legend.title = element_text(size = 7),
                                          axis.text.x = element_blank(),
                                          legend.position="top",
                                          plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                        xlab(NULL)
                                        # theme(plot.title = element_text(hjust = 0.5)) +
                                        # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CO2_compare_TR)

dev.off()

# Arrange plots:

Rates_vs_time_CO2_compare_TR_water <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_compare_TR, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CO2_compare_TR_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CO2_compare_TR_water) # Arrange CO2 emissions + Water level all treats

## 1.2. Averaging TR and DK chambers for Gasera data ####

## Average TR and DK Chambers: (Averages per Date, Plot, Treat)
Emission_rates_Gasera_2023_TRDK <- Gasera_emission_rates_2023 %>% 
                                    group_by(Date, Plot, Treat) %>% 
                                    summarize(avg_Gasera_CH4_flux_mgm2h = mean(CH4_flux_mgm2h), avg_Gasera_R2_CH4 = mean(R2_CH4), avg_Gasera_p_CH4 = mean(p_CH4),
                                              avg_Gasera_N2O_flux_mgm2h = mean(N2O_flux_mgm2h), avg_Gasera_R2_N2O = mean(R2_N2O), avg_Gasera_p_N2O = mean(p_N2O),
                                              avg_Gasera_CO2_flux_mgm2h = mean(CO2_flux_mgm2h), avg_Gasera_R2_CO2 = mean(R2_CO2), avg_Gasera_p_CO2 = mean(p_CO2))

## Merging Gasera and Chromatography emission rate dataframes:
Compare_rates_TRDK_2023 <- merge(Emission_rates_Gasera_2023_TRDK, Chrom_emission_rates_2023, by.x = c("Date", "Plot", "Treat"), by.y = c("Sampling_date", "Plot", "Treat"), all.x = TRUE)

## Selecting columns for comparison:
Rate_compare_TRDK <- select(Compare_rates_TRDK_2023, "Date", "Plot", "Treat", "avg_Gasera_CH4_flux_mgm2h", "avg_Gasera_R2_CH4", "Chrom_CH4_flux_mgm2h", "Chrom_R2_CH4",
                            "avg_Gasera_N2O_flux_mgm2h", "avg_Gasera_R2_N2O", "Chrom_N2O_flux_mgm2h", "Chrom_R2_N2O", "avg_Gasera_CO2_flux_mgm2h", "avg_Gasera_R2_CO2",
                            "Chrom_CO2_flux_mgm2h", "Chrom_R2_CO2")

## Renaming columns:
colnames(Rate_compare_TRDK) = c("Date", "Plot", "Treat", "Gasera_CH4_flux_mgm2h", "Gasera_R2_CH4", "Chrom_CH4_flux_mgm2h", "Chrom_R2_CH4", "Gasera_N2O_flux_mgm2h",
                                "Gasera_R2_N2O", "Chrom_N2O_flux_mgm2h", "Chrom_R2_N2O", "Gasera_CO2_flux_mgm2h", "Gasera_R2_CO2", "Chrom_CO2_flux_mgm2h", "Chrom_R2_CO2")

write_xlsx(Rate_compare_TRDK, "outputs/CERESTRES_results/Gasera_vs_Chromat/Rate_compare_TRDK.xlsx")

## Averaged rates from Gasera and Chromatography: (Averages per Date, Treat)
Avg_rates_compare_TRDK <- Rate_compare_TRDK %>% 
                          group_by(Date, Treat) %>% 
                          summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_mgm2h, na.rm = TRUE), 
                                    avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_mgm2h, na.rm = TRUE),
                                    avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_mgm2h, na.rm = TRUE))

Avg_rates_compare_TRDK$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TRDK$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_TRDK$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK$avg_Chrom_CO2_flux_mgm2h[is.na(Avg_rates_compare_TRDK$avg_Chrom_CO2_flux_mgm2h)] <- NA # Replace NaN for NA values
                        
write_xlsx(Avg_rates_compare_TRDK, "outputs/CERESTRES_results/Gasera_vs_Chromat/Avg_rates_compare_TRDK.xlsx")

## Plot rates across time:

### TRDK - CH4: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TRDK.pdf', width = 12)

Rates_vs_time_CH4_compare_TRDK <- ggplot(data = Avg_rates_compare_TRDK,  aes(color = Treat, x = Date, y = avg_Gasera_CH4_flux_mgm2h, group = Treat)) +
                                          geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Gasera")) +
                                          geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Chromatography"), na.rm = TRUE) +
                                          scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                          scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                          theme_bw() +
                                          labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                          # xlab("Time") +
                                          ggtitle("CH4 Emission rates Gasera vs. Chromatography") +
                                          scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                          theme(
                                            axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                            axis.text.y = element_text(color = "black"),
                                            axis.title.y.right = element_text(color = "black"),
                                            axis.text.y.right = element_text(color = "black"),
                                            strip.background = element_blank(),
                                            strip.placement = "outside",
                                            legend.text = element_text(size = 7),
                                            legend.title = element_text(size = 7),
                                            axis.text.x = element_blank(),
                                            legend.position="top",
                                            plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                          xlab(NULL)
                                          # theme(plot.title = element_text(hjust = 0.5)) +
                                          # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CH4_compare_TRDK)

dev.off()

# Arrange plots:

Rates_vs_time_CH4_compare_TRDK_water <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_compare_TRDK, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TRDK_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CH4_compare_TRDK_water) # Arrange CH4 emissions + Water level all treats

### TRDK - N2O: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_N2O_compare_TRDK.pdf', width = 12)

Rates_vs_time_N2O_compare_TRDK <- ggplot(data = Avg_rates_compare_TRDK,  aes(color = Treat, x = Date, y = avg_Gasera_N2O_flux_mgm2h, group = Treat)) +
                                          geom_line(aes(y = avg_Gasera_N2O_flux_mgm2h, linetype = "Average N2O Flux - Gasera")) +
                                          geom_line(aes(y = avg_Chrom_N2O_flux_mgm2h, linetype = "Average N2O Flux - Chromatography"), na.rm = TRUE) +
                                          scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                          scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                          theme_bw() +
                                          labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                          # xlab("Time") +
                                          ggtitle("N2O Emission rates Gasera vs. Chromatography") +
                                          scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                          theme(
                                            axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                            axis.text.y = element_text(color = "black"),
                                            axis.title.y.right = element_text(color = "black"),
                                            axis.text.y.right = element_text(color = "black"),
                                            strip.background = element_blank(),
                                            strip.placement = "outside",
                                            legend.text = element_text(size = 7),
                                            legend.title = element_text(size = 7),
                                            axis.text.x = element_blank(),
                                            legend.position="top",
                                            plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                          xlab(NULL)
                                          # theme(plot.title = element_text(hjust = 0.5)) +
                                          # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_N2O_compare_TRDK)

dev.off()

# Arrange plots:

Rates_vs_time_N2O_compare_TRDK_water <- grid.arrange(arrangeGrob(Rates_vs_time_N2O_compare_TRDK, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_N2O_compare_TRDK_water.pdf", width = 7, height = 5, plot = Rates_vs_time_N2O_compare_TRDK_water) # Arrange CH4 emissions + Water level all treats

###TRDK - CO2: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CO2_compare_TRDK.pdf', width = 12)

Rates_vs_time_CO2_compare_TRDK <- ggplot(data = Avg_rates_compare_TRDK,  aes(color = Treat, x = Date, y = avg_Gasera_CO2_flux_mgm2h, group = Treat)) +
                                          geom_line(aes(y = avg_Gasera_CO2_flux_mgm2h, linetype = "Average CO2 Flux - Gasera")) +
                                          geom_line(aes(y = avg_Chrom_CO2_flux_mgm2h, linetype = "Average CO2 Flux - Chromatography"), na.rm = TRUE) +
                                          scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                          scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                          theme_bw() +
                                          labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                          # xlab("Time") +
                                          ggtitle("CO2 Emission rates Gasera vs. Chromatography") +
                                          scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                          theme(
                                            axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                            axis.text.y = element_text(color = "black"),
                                            axis.title.y.right = element_text(color = "black"),
                                            axis.text.y.right = element_text(color = "black"),
                                            strip.background = element_blank(),
                                            strip.placement = "outside",
                                            legend.text = element_text(size = 7),
                                            legend.title = element_text(size = 7),
                                            axis.text.x = element_blank(),
                                            legend.position="top",
                                            plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                          xlab(NULL)
                                          # theme(plot.title = element_text(hjust = 0.5)) +
                                          # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CO2_compare_TRDK)

dev.off()

# Arrange plots:

Rates_vs_time_CO2_compare_TRDK_water <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_compare_TRDK, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CO2_compare_TRDK_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CO2_compare_TRDK_water) # Arrange CH4 emissions + Water level all treats

# 2. Using corrected rates - According to the R2 >= 0.7 requirement #######################################################

## 2.1. Considering only TR chambers for Gasera data #####

## Filter to consider only TR chambers:
Emission_rates_Gasera_2023_TR_cor <- filter(Emission_rates_COR_1, Chamber_type == "TR")

## Merging Gasera and Chromatography emission rate dataframes:
Compare_rates_TR_2023_cor <- merge(Emission_rates_Gasera_2023_TR_cor, Chrom_emission_rates_2023, by.x = c("Date", "Plot", "Treat"), by.y = c("Sampling_date", "Plot", "Treat"), all.x = TRUE)

## Selecting columns for comparison:
Rate_compare_TR_cor <- select(Compare_rates_TR_2023_cor, "Date", "Plot", "Treat", "CH4_flux_mgm2h", "R2_CH4", "CH4_flux_mgm2h_cor", "Chrom_CH4_flux_mgm2h", "Chrom_R2_CH4",                                   "N2O_flux_mgm2h","R2_N2O", "Chrom_N2O_flux_mgm2h", "Chrom_R2_N2O", "CO2_flux_mgm2h", "R2_CO2", "Chrom_CO2_flux_mgm2h", "Chrom_R2_CO2")

## Renaming columns:
colnames(Rate_compare_TR_cor) = c("Date", "Plot", "Treat", "Gasera_CH4_flux_mgm2h", "Gasera_R2_CH4", "Gasera_CH4_flux_mgm2h_cor", "Chrom_CH4_flux_mgm2h", "Chrom_R2_CH4", "Gasera_N2O_flux_mgm2h",
                              "Gasera_R2_N2O", "Chrom_N2O_flux_mgm2h", "Chrom_R2_N2O", "Gasera_CO2_flux_mgm2h", "Gasera_R2_CO2", "Chrom_CO2_flux_mgm2h", "Chrom_R2_CO2")

write_xlsx(Rate_compare_TR_cor, "outputs/CERESTRES_results/Gasera_vs_Chromat/Rate_compare_TR_cor.xlsx")

## Averaged rates from Gasera and Chromatography:
Avg_rates_compare_TR_cor <- Rate_compare_TR_cor %>% 
                            group_by(Date, Treat) %>% 
                            summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_mgm2h, na.rm = TRUE),
                                  avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_mgm2h, na.rm = TRUE),
                                  avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE))

Avg_rates_compare_TR_cor$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TR_cor$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR_cor$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_TR_cor$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR_cor$avg_Chrom_CO2_flux_mgm2h[is.na(Avg_rates_compare_TR_cor$avg_Chrom_CO2_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR_cor$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TR_cor$avg_Gasera_CH4_flux_mgm2h_cor)] <- NA # Replace NaN for NA values

write_xlsx(Avg_rates_compare_TR_cor, "outputs/CERESTRES_results/Gasera_vs_Chromat/Avg_rates_compare_TR_cor.xlsx")

## Plot rates across time:

### TR - CH4: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TR_cor.pdf', width = 12)

Rates_vs_time_CH4_compare_TR_cor <- ggplot(data = Avg_rates_compare_TR_cor, aes(color = Treat, x = Date, y = avg_Gasera_CH4_flux_mgm2h_cor, group = Treat)) +
                                          geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h_cor, linetype = "Average CH4 Flux - Gasera")) +
                                          geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Chromatography"), na.rm = TRUE) +
                                          scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                          scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                          theme_bw() +
                                          labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                          # xlab("Time") +
                                          ggtitle("CH4 Emission rates Gasera vs. Chromatography") +
                                          scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                          theme(
                                            axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                            axis.text.y = element_text(color = "black"),
                                            axis.title.y.right = element_text(color = "black"),
                                            axis.text.y.right = element_text(color = "black"),
                                            strip.background = element_blank(),
                                            strip.placement = "outside",
                                            legend.text = element_text(size = 7),
                                            legend.title = element_text(size = 7),
                                            axis.text.x = element_blank(),
                                            legend.position="top",
                                            plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                          xlab(NULL)
                                          # theme(plot.title = element_text(hjust = 0.5)) +
                                          # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CH4_compare_TR_cor)

dev.off()

# Arrange plots:

Rates_vs_time_CH4_compare_TR_cor_water <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_compare_TR_cor, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TR_cor_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CH4_compare_TR_cor_water) # Arrange CH4 emissions + Water level all treats

## 2.2. Averaging TR and DK chambers for Gasera data ####

## Average TR and DK Chambers: (Averages per Date, Plot, Treat)
Emission_rates_Gasera_2023_TRDK_cor <- Emission_rates_COR_1 %>% 
                                        group_by(Date, Plot, Treat) %>% 
                                        summarize(avg_Gasera_CH4_flux_mgm2h = mean(CH4_flux_mgm2h), avg_Gasera_R2_CH4 = mean(R2_CH4), avg_Gasera_p_CH4 = mean(p_CH4),
                                                  avg_Gasera_N2O_flux_mgm2h = mean(N2O_flux_mgm2h), avg_Gasera_R2_N2O = mean(R2_N2O), avg_Gasera_p_N2O = mean(p_N2O),
                                                  avg_Gasera_CO2_flux_mgm2h = mean(CO2_flux_mgm2h), avg_Gasera_R2_CO2 = mean(R2_CO2), avg_Gasera_p_CO2 = mean(p_CO2),
                                                  avg_Gasera_CH4_flux_mgm2h_cor = mean(CH4_flux_mgm2h_cor, na.rm = TRUE))

## Merging Gasera and Chromatography emission rate dataframes:
Compare_rates_TRDK_2023_cor <- merge(Emission_rates_Gasera_2023_TRDK_cor, Chrom_emission_rates_2023, by.x = c("Date", "Plot", "Treat"), by.y = c("Sampling_date", "Plot", "Treat"), all.x = TRUE)

## Selecting columns for comparison:
Rate_compare_TRDK_cor <- select(Compare_rates_TRDK_2023_cor,  "Date", "Plot", "Treat", "avg_Gasera_CH4_flux_mgm2h", "avg_Gasera_R2_CH4", "avg_Gasera_CH4_flux_mgm2h_cor", "Chrom_CH4_flux_mgm2h",                                        "Chrom_R2_CH4", "avg_Gasera_N2O_flux_mgm2h", "avg_Gasera_R2_N2O", "Chrom_N2O_flux_mgm2h", "Chrom_R2_N2O", "avg_Gasera_CO2_flux_mgm2h", "avg_Gasera_R2_CO2",
                                "Chrom_CO2_flux_mgm2h", "Chrom_R2_CO2")

## Renaming columns:
colnames(Rate_compare_TRDK_cor) =c("Date", "Plot", "Treat", "Gasera_CH4_flux_mgm2h", "Gasera_R2_CH4", "Gasera_CH4_flux_mgm2h_cor", "Chrom_CH4_flux_mgm2h", "Chrom_R2_CH4", "Gasera_N2O_flux_mgm2h",
                                   "Gasera_R2_N2O", "Chrom_N2O_flux_mgm2h", "Chrom_R2_N2O", "Gasera_CO2_flux_mgm2h", "Gasera_R2_CO2", "Chrom_CO2_flux_mgm2h", "Chrom_R2_CO2")

write_xlsx(Rate_compare_TRDK_cor, "outputs/CERESTRES_results/Gasera_vs_Chromat/Rate_compare_TRDK_cor.xlsx")

## Averaged rates from Gasera and Chromatography: (Averages per Date, Treat)
Avg_rates_compare_TRDK_cor <- Rate_compare_TRDK_cor %>% 
                              group_by(Date, Treat) %>% 
                              summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_mgm2h, na.rm = TRUE), 
                                    avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_mgm2h, na.rm = TRUE),
                                    avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_mgm2h, na.rm = TRUE),
                                    avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE))

Avg_rates_compare_TRDK_cor$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TRDK_cor$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK_cor$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_TRDK_cor$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK_cor$avg_Chrom_CO2_flux_mgm2h[is.na(Avg_rates_compare_TRDK_cor$avg_Chrom_CO2_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK_cor$avg_Gasera_CH4_flux_mgm2h_cor[is.na(Avg_rates_compare_TRDK_cor$avg_Gasera_CH4_flux_mgm2h_cor)] <- NA # Replace NaN for NA values

write_xlsx(Avg_rates_compare_TRDK_cor, "outputs/CERESTRES_results/Gasera_vs_Chromat/Avg_rates_compare_TRDK_cor.xlsx")

## Plot rates across time:

### TRDK - CH4: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TRDK_cor.pdf', width = 12)

Rates_vs_time_CH4_compare_TRDK_cor <- ggplot(data = Avg_rates_compare_TRDK_cor,  aes(color = Treat, x = Date, y = avg_Gasera_CH4_flux_mgm2h_cor, group = Treat)) +
                                          geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h_cor, linetype = "Average CH4 Flux - Gasera")) +
                                          geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Chromatography"), na.rm = TRUE) +
                                          scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                          scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                          theme_bw() +
                                          labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                          # xlab("Time") +
                                          ggtitle("CH4 Emission rates Gasera vs. Chromatography") +
                                          scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                          theme(
                                            axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                            axis.text.y = element_text(color = "black"),
                                            axis.title.y.right = element_text(color = "black"),
                                            axis.text.y.right = element_text(color = "black"),
                                            strip.background = element_blank(),
                                            strip.placement = "outside",
                                            legend.text = element_text(size = 7),
                                            legend.title = element_text(size = 7),
                                            axis.text.x = element_blank(),
                                            legend.position="top",
                                            plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                          xlab(NULL)
                                          # theme(plot.title = element_text(hjust = 0.5)) +
                                          # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CH4_compare_TRDK_cor)

dev.off()

# Arrange plots:

Rates_vs_time_CH4_compare_TRDK_cor_water <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_compare_TRDK_cor, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TRDK_cor_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CH4_compare_TRDK_cor_water) # Arrange CH4 emissions + Water level all treats

# 3. Removing outliers - Using corrected rates - According to the R2 >= 0.7 requirement #######################################################

## 3.1. Considering only TR chambers for Gasera data #####

## Removing outliers
Rate_compare_TR_cor_nooutliers <- Rate_compare_TR_cor %>% # New data frame with a (1/0) Outliers column, then removing outlier rows
                                    mutate(Outliers = case_when(Gasera_CH4_flux_mgm2h_cor > 10 ~ 1,TRUE ~ 0)) %>% 
                                    filter(Outliers == 0)

## Averaged rates from Gasera and Chromatography:
Avg_rates_compare_TR_cor_nooutliers <- Rate_compare_TR_cor_nooutliers %>% 
                            group_by(Date, Treat) %>% 
                            summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE))

Avg_rates_compare_TR_cor_nooutliers$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TR_cor_nooutliers$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR_cor_nooutliers$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_TR_cor_nooutliers$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR_cor_nooutliers$avg_Chrom_CO2_flux_mgm2h[is.na(Avg_rates_compare_TR_cor_nooutliers$avg_Chrom_CO2_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR_cor_nooutliers$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TR_cor_nooutliers$avg_Gasera_CH4_flux_mgm2h_cor)] <- NA # Replace NaN for NA values

write_xlsx(Avg_rates_compare_TR_cor_nooutliers, "outputs/CERESTRES_results/Gasera_vs_Chromat/Avg_rates_compare_TR_cor_nooutliers.xlsx")

### TR - CH4: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TR_cor_nooutliers.pdf', width = 12)

Rates_vs_time_CH4_compare_TR_cor_nooutliers <- ggplot(data = Avg_rates_compare_TR_cor_nooutliers, aes(color = Treat, x = Date, y = avg_Gasera_CH4_flux_mgm2h_cor, group = Treat)) +
                                            geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h_cor, linetype = "Average CH4 Flux - Gasera")) +
                                            geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Chromatography"), na.rm = TRUE) +
                                            scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                            scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                            theme_bw() +
                                            labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                            # xlab("Time") +
                                            ggtitle("CH4 Emission rates Gasera vs. Chromatography") +
                                            scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                            theme(
                                              axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                              axis.text.y = element_text(color = "black"),
                                              axis.title.y.right = element_text(color = "black"),
                                              axis.text.y.right = element_text(color = "black"),
                                              strip.background = element_blank(),
                                              strip.placement = "outside",
                                              legend.text = element_text(size = 7),
                                              legend.title = element_text(size = 7),
                                              axis.text.x = element_blank(),
                                              legend.position="top",
                                              plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                            xlab(NULL)
                                            # theme(plot.title = element_text(hjust = 0.5)) +
                                            # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CH4_compare_TR_cor_nooutliers)

dev.off()

# Arrange plots:

Rates_vs_time_CH4_compare_TR_cor_nooutliers_water <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_compare_TR_cor_nooutliers, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TR_cor_nooutliers_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CH4_compare_TR_cor_nooutliers_water) # Arrange CH4 emissions + Water level all treats

## 3.2. Averaging TR and DK chambers for Gasera data ####
  
## Removing outliers
Rate_compare_TRDK_cor_nooutliers <- Rate_compare_TRDK_cor %>% # New data frame with a (1/0) Outliers column, then removing outlier rows
                                    mutate(Outliers = case_when(Gasera_CH4_flux_mgm2h_cor > 10 ~ 1,TRUE ~ 0)) %>% 
                                    filter(Outliers == 0)

## Averaged rates from Gasera and Chromatography:
Avg_rates_compare_TRDK_cor_nooutliers <- Rate_compare_TRDK_cor_nooutliers %>% 
                            group_by(Date, Treat) %>% 
                            summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE))

Avg_rates_compare_TRDK_cor_nooutliers$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TRDK_cor_nooutliers$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK_cor_nooutliers$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_TRDK_cor_nooutliers$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK_cor_nooutliers$avg_Chrom_CO2_flux_mgm2h[is.na(Avg_rates_compare_TRDK_cor_nooutliers$avg_Chrom_CO2_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TRDK_cor_nooutliers$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TRDK_cor_nooutliers$avg_Gasera_CH4_flux_mgm2h_cor)] <- NA # Replace NaN for NA values

write_xlsx(Avg_rates_compare_TRDK_cor_nooutliers, "outputs/CERESTRES_results/Gasera_vs_Chromat/Avg_rates_compare_TRDK_cor_nooutliers.xlsx")

### TRDK - CH4: ####

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TRDK_cor_nooutliers.pdf', width = 12)

Rates_vs_time_CH4_compare_TRDK_cor_nooutliers <- ggplot(data = Avg_rates_compare_TRDK_cor_nooutliers, aes(color = Treat, x = Date, y = avg_Gasera_CH4_flux_mgm2h_cor, group = Treat)) +
                                                      geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h_cor, linetype = "Average CH4 Flux - Gasera")) +
                                                      geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h, linetype = "Average CH4 Flux - Chromatography"), na.rm = TRUE) +
                                                      scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                                      scale_linetype_manual(values = c("solid", "dashed")) +  # Manually set line types
                                                      theme_bw() +
                                                      labs(y = expression(paste(CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                                      # xlab("Time") +
                                                      ggtitle("CH4 Emission rates Gasera vs. Chromatography") +
                                                      scale_x_date(date_breaks = "14 day", date_labels = "%m.%d") +
                                                      theme(
                                                        axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                                        axis.text.y = element_text(color = "black"),
                                                        axis.title.y.right = element_text(color = "black"),
                                                        axis.text.y.right = element_text(color = "black"),
                                                        strip.background = element_blank(),
                                                        strip.placement = "outside",
                                                        legend.text = element_text(size = 7),
                                                        legend.title = element_text(size = 7),
                                                        axis.text.x = element_blank(),
                                                        legend.position="top",
                                                        plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                                      xlab(NULL)
                                                      # theme(plot.title = element_text(hjust = 0.5)) +
                                                      # theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Rates_vs_time_CH4_compare_TRDK_cor_nooutliers)

dev.off()

# Arrange plots:

Rates_vs_time_CH4_compare_TRDK_cor_nooutliers_water <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_compare_TRDK_cor_nooutliers, Water_plot_2023, nrow = 2, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/Rates_vs_time_CH4_compare_TRDK_cor_nooutliers_water.pdf", width = 7, height = 5, plot = Rates_vs_time_CH4_compare_TRDK_cor_nooutliers_water) # Arrange CH4 emissions + Water level all treats