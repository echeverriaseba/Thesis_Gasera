
######################################################################################################
############################ Working with GASERA Measurements ########################################
######################################################################################################

#################################### 1. GASERA raw data ##############################################

library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)
library(ggpubr)
library(ggrepel) 
library(ggpmisc)

folder_path <- "data/CERESTRES_Tests_Arena/"  # Path to the folder
file_names <- list.files(folder_path, full.names = TRUE)

df_list <- list()  # List to store the dataframes

for (file_name in file_names) {
  # Extract the filename without the full path
  file_name <- basename(file_name)
  
  # Read the file
  file_path <- file.path(folder_path, file_name)  # Construct the full file path
  
  # Reading and creating working dataframe
  a <- readLines(file_path)
  b <- a[-(1:9)]
  c <- as.matrix(str_split(b, "\t"))
  mat <- do.call(rbind, c)
  Mat <- as.data.frame (mat, stringsAsFactors = FALSE)
  new_colnames <- c("Date Time", "CH4_ppm", "CO2_ppm", "H2O_ppm", "N2O_ppm", "NH3_ppm", "Inlet")
  colnames(Mat) <- new_colnames
  
  # Split the first column into two columns
  date_time <- strsplit(Mat[, 1], " ")
  Date <- sapply(date_time, "[", 1)
  Time <- sapply(date_time, "[", 2)
  Mat <- cbind(Date, Time, Mat[, -1])
  
  # Convert columns to numeric
  cols_to_convert <- c(3, 4, 5, 6, 7, 8)
  Mat[, cols_to_convert] <- lapply(Mat[, cols_to_convert], as.numeric)
  
  # Assign a unique name to the dataframe
  df_name <- gsub("\\.", "_", file_name)  # Replace dots with underscores
  df_name <- gsub("-", "_", df_name)  # Replace dashes with underscores
  
  # Extract portions of the file name
  Exp <- substr(file_name, 1, 3)
  Treat <- substr(file_name, 4, 6)
  Rep <- substr(file_name, 7, 8)
  
  # Add the portions as new columns to the dataframe
  Mat <- cbind(Exp, Mat, Treat, Rep)
  
  # Add the dataframe Mat to a list of dataframes, each resulting from each measuring file, so then we can merge into a unique dataframe
  df_list[[file_name]] <- Mat
}

# Merge resulting dataframes into a unique one  
Gasera_results_tests_arena <- bind_rows(df_list)

# Including Time steps column (T0, T1, ...)

# Assigning Time steps:
Gasera_results_tests_arena$Time_step <- with(Gasera_results_tests_arena, ave(Time, Date, Rep, Treat, FUN = function(x) paste0("T", seq_along(x)-1)))

#################################### 2. Merging with field sheet data to calculate rates ############

##### 2.1. Assigning Temperature to each Time Step: #### 

Temp <- read.csv("data/Field_records_Gasera_Tests_Arena.csv", fileEncoding="latin1", na.strings=c("","NA")) # Field sheet with initial and final temperatures

# Group by Date and Rep, calculate the minimum and maximum Time, and store it in new dataframes
min_time_df <- Gasera_results_tests_arena %>%
  group_by(Date, Treat, Rep) %>%
  summarize(Time_initial = min(Time))
max_time_df <- Gasera_results_tests_arena %>%
  group_by(Date, Treat, Rep) %>%
  summarize(Time_final = max(Time))

# Edit Date format:
Temp$Date <- as.Date(Temp$Date)
min_time_df$Date <- as.Date(min_time_df$Date)
max_time_df$Date <- as.Date(max_time_df$Date)

# Merge the min_time_df and max_time_df dataframe with the Temp dataframe
Temp <- merge(Temp, min_time_df, by = c("Date", "Rep", "Treat"))
Temp <- merge(Temp, max_time_df, by = c("Date", "Rep", "Treat"))

# Convert "Time", Time_initial" and "Time_final" to POSIXct format
Temp$Time_initial <- as.POSIXct(Temp$Time_initial, format = "%H:%M:%S")
Temp$Time_final <- as.POSIXct(Temp$Time_final, format = "%H:%M:%S")
Gasera_results_tests_arena$Time <- as.POSIXct(Gasera_results_tests_arena$Time, format = "%H:%M:%S", tz = "UTC")

# Calculate temperature change rate in ºC per Time
Temp$time_diff <- as.numeric(difftime(Temp$Time_final, Temp$Time_initial, units = "secs"))
Temp$Temp_rate <- (Temp$Temp_final - Temp$Temp_initial) / Temp$time_diff # This calculates the temp rate in ºC/sec

# Sort Gasera_results_tests_arena by "Rep", "Date", and "Time_step"
Gasera_results_tests_arena <- Gasera_results_tests_arena[order(Gasera_results_tests_arena$Date, Gasera_results_tests_arena$Rep, Gasera_results_tests_arena$Time_step), ]

# Group by "Date" and "Rep" and calculate time differences in seconds
Gasera_results_tests_arena <- Gasera_results_tests_arena %>%
  group_by(Date, Treat, Rep) %>%
  arrange(Date, Treat, Rep, Time) %>%
  mutate(Time_diff = difftime(Time, first(Time), units = "secs")) %>%
  mutate(Time_diff = format(as.numeric(Time_diff), nsmall = 0))

# Merge the "Temp" dataframe with `Gasera_results_tests_arena` based on "Rep", "Date" and "Treat"
Gasera_results_tests_arena$Date <- as.Date(Gasera_results_tests_arena$Date)
Gasera_results_tests_arena <- merge(Gasera_results_tests_arena, Temp[, c("Date", "Rep", "Treat", "Temp_initial", "Temp_final", "Temp_rate")], by = c("Rep", "Date", "Treat"), all.x = TRUE)

# Sort `Gasera_results_tests_arena` by "Date", "Rep" , "Treat", and "Time_step"
Gasera_results_tests_arena <- Gasera_results_tests_arena[order(Gasera_results_tests_arena$Rep, Gasera_results_tests_arena$Date, Gasera_results_tests_arena$Treat, Gasera_results_tests_arena$Time_step), ]

# Group by "Rep" and "Date" and calculate the time difference in seconds between each "Time_step" and T0
Gasera_results_tests_arena <- Gasera_results_tests_arena %>%
  group_by(Treat, Rep, Date) %>%
  mutate(Time_diff = as.numeric(difftime(Time, first(Time), units = "secs")))

# Create a "temp" column with calculated temperatures per Time_step:
Gasera_results_tests_arena$Temp <- Gasera_results_tests_arena$Temp_initial + (Gasera_results_tests_arena$Temp_rate*Gasera_results_tests_arena$Time_diff)

# Create the Code column:
Gasera_results_tests_arena$Code <- paste(Gasera_results_tests_arena$Exp, Gasera_results_tests_arena$Treat, Gasera_results_tests_arena$Date, Gasera_results_tests_arena$Rep, sep = "_")

# Create column with Sample time in minutes:
Gasera_results_tests_arena$Sample_time_min <- Gasera_results_tests_arena$Time_diff/60

##### 2.2. Creating emission rate calculation columns ####

Gasera_results_tests_arena$Chamber_temp_k <- Gasera_results_tests_arena$Temp + 273 ## Adding column "Chamber_temp_k".
Gasera_results_tests_arena$Surface_Area <- 0.0314 ## Adding column "Surface_Area" : Be careful to edit this in case different chamber types are used.
Gasera_results_tests_arena$Chamber_Height <- 0.75 ## Adding column "Chamber_Height" : Be careful to edit this in case different chamber types are used.
Gasera_results_tests_arena$Volume <- Gasera_results_tests_arena$Surface_Area * Gasera_results_tests_arena$Chamber_Height ## Adding column "Volume".
Gasera_results_tests_arena$CH4_density_g_m3 <- (16 / (82.0575 * Gasera_results_tests_arena$Chamber_temp_k)) * 1000000
Gasera_results_tests_arena$N2O_density_g_m3 <- (44 / (82.0575 * Gasera_results_tests_arena$Chamber_temp_k)) * 1000000
Gasera_results_tests_arena$CO2_density_g_m3 <- (44 / (82.0575 * Gasera_results_tests_arena$Chamber_temp_k)) * 1000000
Gasera_results_tests_arena$CH4_byMass_mgm3 <- (Gasera_results_tests_arena$CH4_density_g_m3 * Gasera_results_tests_arena$CH4_ppm) / 1000
Gasera_results_tests_arena$CH4_byMass_mgm2 <- (Gasera_results_tests_arena$CH4_byMass_mgm3 * Gasera_results_tests_arena$Volume) / Gasera_results_tests_arena$Surface_Area
Gasera_results_tests_arena$N2O_byMass_mgm3 <- (Gasera_results_tests_arena$N2O_density_g_m3 * Gasera_results_tests_arena$N2O_ppm) / 1000
Gasera_results_tests_arena$N2O_byMass_mgm2 <- (Gasera_results_tests_arena$N2O_byMass_mgm3 * Gasera_results_tests_arena$Volume) / Gasera_results_tests_arena$Surface_Area
Gasera_results_tests_arena$CO2_byMass_mgm3 <- (Gasera_results_tests_arena$CO2_density_g_m3 * Gasera_results_tests_arena$CO2_ppm) / 1000
Gasera_results_tests_arena$CO2_byMass_mgm2 <- (Gasera_results_tests_arena$CO2_byMass_mgm3 * Gasera_results_tests_arena$Volume) / Gasera_results_tests_arena$Surface_Area

#### 2.3. Creating new data frame to calculate rates and R2 ####

Emission_rates_tests_arena <- Gasera_results_tests_arena %>% distinct(Gasera_results_tests_arena$Code) ## Creates data frame with unique values for certain columns
Emission_rates_tests_arena <- cbind(Emission_rates_tests_arena, empty_column2=NA, empty_column3=NA, empty_column4=NA, empty_column5=NA, empty_column6=NA, empty_column7=NA, empty_column8=NA, empty_column9=NA, empty_column10=NA, empty_column11=NA)
colnames(Emission_rates_tests_arena) = c("Treat", "Rep", "Date", "Code", "Code_Nr","CH4_flux_mgm2h", "R2_CH4", "p_CH4", "N2O_flux_mgm2h", "R2_N2O", "p_N2O", "CO2_flux_mgm2h", "R2_CO2", "p_CO2")
Emission_rates_tests_arena <- Emission_rates_tests_arena[,c("Date", "Code_Nr", "Code", "Treat", "Rep", "CH4_flux_mgm2h", "R2_CH4", "p_CH4", "N2O_flux_mgm2h", "R2_N2O", "p_N2O", "CO2_flux_mgm2h", "R2_CO2", "p_CO2")] # Reorder columns
Emission_rates_tests_arena <- Emission_rates_tests_arena[order(Emission_rates_tests_arena$Treat, Emission_rates_tests_arena$Rep), ] # reorder rows
Emission_rates_tests_arena$Code_Nr <- 1:nrow(Emission_rates_tests_arena)

#################################### 3. Loops to fill up new data frame Emission_rates_tests_arena #################

#### 3.1 Loops for CH4 ####

pdf('outputs/CERESTRES_Tests_Arena/CH4_Plots_Tests_Arena.pdf')

for (i in 1:length(Emission_rates_tests_arena$Code)) {
  Code_i <- Emission_rates_tests_arena$Code[i]
  Filt_i <- filter(Gasera_results_tests_arena, Code == Code_i) # if returned as Time-Series, re-run library(dplyr)
  
  ## Loop section 1: Rate calculation.
  lm_i <- lm(CH4_byMass_mgm2~Sample_time_min, data=Filt_i)
  Emission_rates_tests_arena$CH4_flux_mgm2h[i] <- coef(lm_i)[2]*60 # Returns CH4_flux_mgm2h for each Code.
  Emission_rates_tests_arena$R2_CH4[i] <- summary(lm_i)$r.squared # Returns R2_CH4 for each Code.
  lmp_i <- function (modelobject) {  # Function created to call later the model's p-value
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  Emission_rates_tests_arena$p_CH4[i] <- lmp_i(lm_i) # Returns p-value for each Code.
  
  ## Plot - Original values (before corrections):
  Plot_i <- ggplot(data = Filt_i, aes(x=Sample_time_min, y=CH4_byMass_mgm2)) +
    geom_point() +
    xlab("Sample time (min)") +
    ylab("CH4 Emissions (mgm-2)") +
    ggtitle(paste("Code = ", Emission_rates_tests_arena$Code[i],"; Original Model")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_poly_line() +
    stat_poly_eq() +
    annotate(geom="text", -Inf, Inf, label=paste("Rate: ", round(Emission_rates_tests_arena$CH4_flux_mgm2h[i], digits = 4),"mgm2h"), hjust = -0.25, vjust = 6)
  
  print(Plot_i)
}  

dev.off()

#### 3.2 Loops for N2O ####

pdf('outputs/CERESTRES_Tests_Arena/N2O_Plots_Tests_Arena.pdf')

for (j in 1:length(Emission_rates_tests_arena$Code)) {
  Code_j <- Emission_rates_tests_arena$Code[j]
  Filt_j <- filter(Gasera_results_tests_arena, Code == Code_j) # if returned as Time-Series, re-run library(dplyr)
  
  ## Loop section 1: Rate calculation.
  lm_j <- lm(N2O_byMass_mgm2~Sample_time_min, data=Filt_j)
  Emission_rates_tests_arena$N2O_flux_mgm2h[j] <- coef(lm_j)[2]*60 # Returns CH4_flux_mgm2h for each Code.
  Emission_rates_tests_arena$R2_N2O[j] <- summary(lm_j)$r.squared # Returns R2_CH4 for each Code.
  lmp_j <- function (modelobject) {  # Function created to call later the model's p-value
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  Emission_rates_tests_arena$p_N2O[j] <- lmp_j(lm_j) # Returns p-value for each Code.
  
  ## Plot - Original values (before corrections):
  Plot_j <- ggplot(data = Filt_j, aes(x=Sample_time_min, y=N2O_byMass_mgm2)) +
    geom_point() +
    xlab("Sample time (min)") +
    ylab("N2O Emissions (mgm-2)") +
    ggtitle(paste("Code = ", Emission_rates_tests_arena$Code[j],"; Original Model")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_poly_line() +
    stat_poly_eq() +
    annotate(geom="text", -Inf, Inf, label=paste("Rate: ", round(Emission_rates_tests_arena$N2O_flux_mgm2h[j], digits = 4),"mgm2h"), hjust = -0.25, vjust = 6)
  
  print(Plot_j)
}  

dev.off()

#### 3.3 Loops for CO2 ####

pdf('outputs/CERESTRES_Tests_Arena/CO2_Plots_Tests_Arena.pdf')

for (k in 1:length(Emission_rates_tests_arena$Code)) {
  Code_k <- Emission_rates_tests_arena$Code[k]
  Filt_k <- filter(Gasera_results_tests_arena, Code == Code_k) # if returned as Time-Series, re-run library(dplyr)
  
  ## Loop section 1: Rate calculation.
  lm_k <- lm(CO2_byMass_mgm2~Sample_time_min, data=Filt_k)
  Emission_rates_tests_arena$CO2_flux_mgm2h[k] <- coef(lm_k)[2]*60 # Returns CH4_flux_mgm2h for each Code.
  Emission_rates_tests_arena$R2_CO2[k] <- summary(lm_k)$r.squared # Returns R2_CH4 for each Code.
  lmp_k <- function (modelobject) {  # Function created to call later the model's p-value
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  Emission_rates_tests_arena$p_CO2[k] <- lmp_i(lm_k) # Returns p-value for each Code.
  
  ## Plot - Original values (before corrections):
  Plot_k <- ggplot(data = Filt_k, aes(x=Sample_time_min, y=CO2_byMass_mgm2)) +
    geom_point() +
    xlab("Sample time (min)") +
    ylab("CO2 Emissions (mgm-2)") +
    ggtitle(paste("Code = ", Emission_rates_tests_arena$Code[k],"; Original Model")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks=c(0, 10, 20, 30)) +
    stat_poly_line() +
    stat_poly_eq() +
    annotate(geom="text", -Inf, Inf, label=paste("Rate: ", round(Emission_rates_tests_arena$CO2_flux_mgm2h[k], digits = 4),"mgm2h"), hjust = -0.25, vjust = 6)
  
  print(Plot_k)
}  

dev.off()

# Sort `Emission_rates_tests_arena` by "Date", "Rep" and "Time_step"
Emission_rates_tests_arena <- Emission_rates_tests_arena[order(Emission_rates_tests_arena$Date, Emission_rates_tests_arena$Rep), ]

write_xlsx(Emission_rates_tests_arena, "outputs/CERESTRES_Tests_Arena/CERESTRES_Emission_rates_tests_arena.xlsx")