x = 3

current_plot <- Master_GHG_2023$Plot[x]
current_rep <- Master_GHG_2023$Rep[x]
neg_piezo_indices <- which(Master_GHG_2023$Water_level_piezo <= 0 & Master_GHG_2023$Plot == current_plot)
neg_piezo_indices_CON <- which(Master_GHG_2023$Water_level_piezo <= 0 & Master_GHG_2023$Rep == current_rep & Master_GHG_2023$Treat == "MSD")
closest_neg_index <- ifelse(Master_GHG_2023$Treat[x] %in% c("AWD", "MSD"), 
                            neg_piezo_indices[which.min(abs(as.numeric(Master_GHG_2023$Sampling_date[x] - 
                                                                         Master_GHG_2023$Sampling_date[neg_piezo_indices])))], 1) # the ifelse (... , ... , 1) solves the "replacement of length zero" for cases CON / piezo: NA / ruler: 0
closest_neg_index_CON <- neg_piezo_indices_CON[which.min(abs(as.numeric(Master_GHG_2023$Sampling_date[x] - Master_GHG_2023$Sampling_date[neg_piezo_indices_CON])))]

Test_level_corr <- case_when(
  
  # a.i): 
            Master_GHG_2023$Water_level_piezo[x] < 0 ~ Master_GHG_2023$Water_level_piezo[x],
  # a.ii): 
            !is.na(Master_GHG_2023$Water_level_ruler_Gasera[x]) & Master_GHG_2023$Water_level_ruler_Gasera[x] != 0 ~ Master_GHG_2023$Water_level_ruler_Gasera[x],
  # b):
            (is.na(Master_GHG_2023$Water_level_ruler_Gasera[x]) | Master_GHG_2023$Water_level_ruler_Gasera[x] == 0) & Master_GHG_2023$Water_level_ruler[x] != 0 
            ~ Master_GHG_2023$Water_level_ruler[x],
  # c):
            (is.na(Master_GHG_2023$Water_level_ruler_Gasera[x]) | Master_GHG_2023$Water_level_ruler_Gasera[x] == 0) & 
              (is.na(Master_GHG_2023$Water_level_ruler[x]) | Master_GHG_2023$Water_level_ruler[x] == 0) & !is.na(Master_GHG_2023$Water_level_piezo[x]) ~ Master_GHG_2023$Water_level_piezo[x],
  # d) Part I:
            (is.na(Master_GHG_2023$Water_level_ruler_Gasera[x]) | Master_GHG_2023$Water_level_ruler_Gasera[x] == 0) & 
              (is.na(Master_GHG_2023$Water_level_ruler[x]) | Master_GHG_2023$Water_level_ruler[x] == 0) &  Master_GHG_2023$Treat[x] %in% c("AWD", "MSD") & 
              is.na(closest_neg_index) ~ Master_GHG_2023$Water_level_ruler_Gasera[x],
  # d) Part II:
            (is.na(Master_GHG_2023$Water_level_ruler_Gasera[x]) | Master_GHG_2023$Water_level_ruler_Gasera[x] == 0) & 
              (is.na(Master_GHG_2023$Water_level_ruler[x]) | Master_GHG_2023$Water_level_ruler[x] == 0) &  Master_GHG_2023$Treat[x] %in% c("AWD", "MSD") & 
              !is.na(closest_neg_index) ~ Master_GHG_2023$Water_level_piezo[closest_neg_index],
  
  # e)
            (is.na(Master_GHG_2023$Water_level_ruler_Gasera[x]) | Master_GHG_2023$Water_level_ruler_Gasera[x] == 0) &  
            (is.na(Master_GHG_2023$Water_level_ruler[x]) | Master_GHG_2023$Water_level_ruler[x] == 0) &  
            Master_GHG_2023$Treat[x] %in% "CON" & 
            !is.na(closest_neg_index_CON) ~ Master_GHG_2023$Water_level_piezo[closest_neg_index_CON],   
  
  TRUE ~ NA_real_
)

Test_level_corr