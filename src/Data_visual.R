
################################################### Thesis Paper 2 - Data Visualization #########################################################

library(tidyverse)
library(ggplot2)
library(zoo)
library(writexl)
library(gridExtra)
library(cowplot)
library(ggpmisc)
library(ggpubr)

# Data Visualization from data previously ordered in script: Data_prep

# 1. Plotting water level ####

Avg_water_level3 <- Master_GHG_2023 %>% 
                    group_by(Sampling_date, Treat, Doub_piez) %>% 
                    summarize(avg_Water_level_corr = mean(Water_level_corr, na.rm = TRUE))

Avg_water_level3$avg_Water_level_corr[is.na(Avg_water_level3$avg_Water_level_corr)] <- NA # Replace NaN for NA values

date_breaks <- seq(from = as.Date("2023-05-24"), to = as.Date("2023-12-12"), by = "14 days")

pdf('outputs/CERESTRES_results/Water_plot_2023b.pdf', width = 12)

Water_plot_2023b <- ggplot(Avg_water_level3, aes(x = Sampling_date, color = Treat, linetype = avg_Water_level_corr)) +
                            geom_line(aes(y = avg_Water_level_corr, linetype = "avg_Water_level_corr", color = Treat), show.legend = FALSE) +
                            scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme_bw() +
                            labs(y = "Water level (cm)") +
                            labs(x = "Date (mm-dd)") +
                            geom_hline(yintercept=0, color = "grey") +
                            geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                            annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                            scale_linetype_manual(name = "avg_Water_level_corr",
                                                  values = c("avg_Water_level_corr" = "dashed"), labels = "Water level (cm)") +
                            guides(linetype = guide_legend(override.aes = list(color = "black"))) +
                            theme(axis.title.y = element_text(color = "black", size = 16), 
                                  axis.text.y = element_text(color = "black", size = 16),
                                  axis.title.x = element_text(color = "black", size = 16), 
                                  axis.text.x = element_text(color = "black", size = 16),
                                  axis.title.y.right = element_text(color = "black"),
                                  axis.text.y.right = element_text(color = "black"), strip.background = element_blank(),
                                  strip.placement = "outside",
                                  plot.margin = unit(c(0, 1, 1, 1.35), "lines"))+
                            scale_x_date(limits = as.Date(c("2023-05-24", "2023-12-15")), breaks = date_breaks, date_labels = "%m.%d")

print(Water_plot_2023b) # Water level all treats

dev.off()

ggsave("outputs/CERESTRES_results/Water_plot_2023c.pdf", plot = Water_plot_2023b ,width = 20, height = 5)   
Water_plot_2023b_limits <- as.Date(layer_scales(Water_plot_2023b)$x$get_limits()) # extracting limits from plot Water_plot_2023b to use in Gasera and Chrom plots, so their x axis coincide when arranging

# 2. Plotting Emissions ####

## 2.1. Preparing subsetted dataframes for plots ####

## Gasera - Tansparent chambers:

Avg_rates_compare_TR <- Master_GHG_2023 %>%  # New df with averaged emissions for all Gasera samplings with transparent chambers and all Chromatography samplings,
                          filter(Chamber_type == "TR" | is.na(Chamber_type)) %>% 
                          group_by(Sampling_date, Treat) %>% 
                          summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE), 
                                    avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE), avg_Gasera_CCH4_flux_mgm2h = mean(Gasera_CCH4_flux_mgm2h, na.rm = TRUE),
                                    avg_Gasera_CCH4_flux_mgm2h_cor = mean(Gasera_CCH4_flux_mgm2h_cor, na.rm = TRUE),
                                    avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                                    avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE), avg_Gasera_NN2O_flux_mgm2h = mean(Gasera_NN2O_flux_mgm2h, na.rm = TRUE), 
                                    avg_Gasera_NN2O_flux_mgm2h_cor = mean(Gasera_NN2O_flux_mgm2h_cor, na.rm = TRUE),
                                    avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                                    avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE), avg_Gasera_CCO2_flux_mgm2h = mean(Gasera_CCO2_flux_mgm2h, na.rm = TRUE), 
                                    avg_Gasera_CCO2_flux_mgm2h_cor = mean(Gasera_CCO2_flux_mgm2h_cor, na.rm = TRUE))

Avg_rates_compare_TR <- Avg_rates_compare_TR %>% # Replace NaN for NA values
                        mutate_if(is.numeric, ~na_if(., NaN))

Avg_rates_compare_TR_CHROM <- Avg_rates_compare_TR %>% # New df with averaged Chromatography results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                              filter(!is.na(avg_Chrom_CH4_flux_mgm2h))

Master_GHG_2023_CHROM <- Master_GHG_2023 %>% # New df with all Chromatography results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                          filter(!is.na(Chrom_CH4_flux_corrected))

Avg_rates_compare_TR_GASERA <- Avg_rates_compare_TR %>% # New df with averaged Gasera results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                                filter(!is.na(avg_Gasera_CH4_flux_mgm2h))

Master_GHG_2023_GASERA_TR <- Master_GHG_2023 %>% # New df with all Gasera results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                              filter(Chamber_type == "TR" & !is.na(Gasera_CH4_flux_mgm2h)) 

# Gasera - Dark chambers:

Avg_rates_compare_DK <- Master_GHG_2023 %>%  # New df with averaged emissions for all Gasera samplings with dark chambers and all chromatography samplings,
                            filter(Chamber_type == "DK" | is.na(Chamber_type)) %>% 
                            group_by(Sampling_date, Treat) %>% 
                            summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE), 
                                      avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE), avg_Gasera_CCH4_flux_mgm2h = mean(Gasera_CCH4_flux_mgm2h, na.rm = TRUE),
                                      avg_Gasera_CCH4_flux_mgm2h_cor = mean(Gasera_CCH4_flux_mgm2h_cor, na.rm = TRUE),
                                      avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                                      avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE), avg_Gasera_NN2O_flux_mgm2h = mean(Gasera_NN2O_flux_mgm2h, na.rm = TRUE), 
                                      avg_Gasera_NN2O_flux_mgm2h_cor = mean(Gasera_NN2O_flux_mgm2h_cor, na.rm = TRUE),
                                      avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                                      avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE), avg_Gasera_CCO2_flux_mgm2h = mean(Gasera_CCO2_flux_mgm2h, na.rm = TRUE), 
                                      avg_Gasera_CCO2_flux_mgm2h_cor = mean(Gasera_CCO2_flux_mgm2h_cor, na.rm = TRUE))

Avg_rates_compare_DK <- Avg_rates_compare_DK %>% # Replace NaN for NA values
                        mutate_if(is.numeric, ~na_if(., NaN))

Avg_rates_compare_DK_CHROM <- Avg_rates_compare_DK %>% # New df with averaged Chromatography results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                              filter(!is.na(avg_Chrom_CH4_flux_mgm2h))

Avg_rates_compare_DK_GASERA <- Avg_rates_compare_DK %>% # New df with averaged Gasera results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                                filter(!is.na(avg_Gasera_CH4_flux_mgm2h))

Master_GHG_2023_GASERA_DK <- Master_GHG_2023 %>% # New df with all Gasera results, it's filtered for NA using CH4 column but works as well for N2O and CO2.
                              filter(Chamber_type == "DK" & !is.na(Gasera_CH4_flux_mgm2h)) 

## 2.2. CH4 ####

#### CH4 - Chromatography ####

Rates_vs_time_CH4_CHROM <- ggplot(data = Avg_rates_compare_TR_CHROM, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CH4_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_CHROM, aes(x = Sampling_date, y = Chrom_CH4_flux_corrected, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Chrom_CH4_flux_mgm2h)) + # linetype = "Average CH4 Flux - Chromatography")) +   , na.rm = TRUE) +
                                    scale_colour_manual(name = "Irriation Strategies:", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("C -", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                    geom_hline(yintercept = 0, color = "grey") +
                                    geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                    annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                    annotate('text', x = as.Date("2023-06-27"), y = 45, label = "Mid-Season Drainage", size = 7, color = "darkgrey", angle = '90') +
                                    annotate('text', x = as.Date("2023-09-30"), y = 45, label = "Growing Season", size = 7, color = "grey", angle = '90') +
                                    annotate('text', x = as.Date("2023-10-06"), y = 45, label = "Fallow Season", size = 7, color = "grey", angle = '270') +
                                    theme(
                                          axis.title.y = element_text(color = "black", size = 16), legend.margin= ggplot2::margin(0,0,0,0),
                                          axis.text.y = element_text(color = "black", size = 16),
                                          axis.title.y.right = element_text(color = "black"),
                                          axis.text.y.right = element_text(color = "black"),
                                          strip.background = element_blank(),
                                          strip.placement = "outside",
                                          legend.text = element_text(size = 16),
                                          legend.title = element_text(size = 16),
                                          axis.text.x = element_blank(),
                                          legend.position="top",
                                          plot.margin = unit(c(1, 1, 0, 1.3), "lines"),
                                          plot.title = element_blank()) +
                                    xlab(NULL) 

print(Rates_vs_time_CH4_CHROM)

#### CH4 - Gasera (not corrected) ####

Rates_vs_time_CH4_GASERA <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CCH4_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CCH4_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_CCH4_flux_mgm2h)) + #, linetype = "Average CH4 Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera - C-", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    geom_hline(yintercept=0, color = "grey") +
                                    geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                    annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                    scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                    theme(
                                      axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                      axis.text.y = element_text(color = "black"),
                                      axis.title.y.right = element_text(color = "black"),
                                      axis.text.y.right = element_text(color = "black"),
                                      strip.background = element_blank(),
                                      axis.text.x = element_blank(),
                                      strip.placement = "outside",
                                      legend.position="none",
                                      plot.margin = unit(c(0, 1, 0, 1.3), "lines")) +
                                    xlab(NULL) 

print(Rates_vs_time_CH4_GASERA)

#### CH4 - Gasera (corrected) ####

Rates_vs_time_CH4_GASERA_cor <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CH4_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CCH4_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_CCH4_flux_mgm2h_cor)) + #, linetype = "Average CH4 Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera (corrected)- C-", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    geom_hline(yintercept=0, color = "grey") +
                                    geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                    annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                    scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                    theme(
                                      axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                      axis.text.y = element_text(color = "black"),
                                      axis.title.y.right = element_text(color = "black"),
                                      axis.text.y.right = element_text(color = "black"),
                                      strip.background = element_blank(),
                                      axis.text.x = element_blank(),
                                      strip.placement = "outside",
                                      legend.position="none",
                                      plot.margin = unit(c(0, 1, 0, 1.3), "lines")) +
                                    xlab(NULL) 

print(Rates_vs_time_CH4_GASERA_cor)

#### CH4 - Plot arrange ####

# Gasera not corrected:

Rates_vs_time_CH4_methods <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Rates_vs_time_CH4_GASERA, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_CH4_methods)

# Gasera corrected:

Rates_vs_time_CH4_methods_gascor <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Rates_vs_time_CH4_GASERA_cor, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods_gascor.pdf", width = 20, height = 12, plot = Rates_vs_time_CH4_methods_gascor)

# Only Chromatography:

Rates_vs_time_CH4_CHROM_arr <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Water_plot_2023b, nrow = 2, ncol = 1)) # CCH4 flux and water level arrange
Rates_vs_time_CH4_N2O_CHROM_arr <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Rates_vs_time_N2O_CHROM2, Water_plot_2023b, nrow = 3, ncol = 1)) # CCH4 flux, NN2O flux and water level arrange
Rates_vs_time_N2O_CHROM_arr <- grid.arrange(arrangeGrob(Rates_vs_time_N2O_CHROM2, Water_plot_2023b, nrow = 2, ncol = 1)) # NN2O flux and water level arrange
Rates_vs_time_N2O_CHROM_arr2 <- ((Rates_vs_time_N2O_CHROM2 / Water_plot_2023b) | N2O_acc_arr2)

ggsave("outputs/CERESTRES_results/Chromat_results/CH4_flux_water.pdf", width = 20, height = 10, plot = Rates_vs_time_CH4_CHROM_arr)
ggsave("outputs/CERESTRES_results/Chromat_results/N2O_flux_water.pdf", width = 20, height = 10, plot = Rates_vs_time_N2O_CHROM_arr)
ggsave("outputs/CERESTRES_results/Chromat_results/CH4_N2O_flux_water.pdf", width = 20, height = 10, plot = Rates_vs_time_CH4_N2O_CHROM_arr)
ggsave("outputs/CERESTRES_results/Chromat_results/N2O_flux_acc_water.pdf", width = 20, height = 10, plot = Rates_vs_time_N2O_CHROM_arr2)

## 2.3. N2O ####

#### N2O - Chromatography ####

Rates_vs_time_N2O_CHROM <- ggplot(data = Avg_rates_compare_TR_CHROM, aes(color = Treat, x = Sampling_date, y = avg_Chrom_N2O_flux_mgm2h, group = Treat)) +
                                  geom_line(data = Master_GHG_2023_CHROM, aes(x = Sampling_date, y = Chrom_N2O_flux_corrected, group = Plot), alpha = 0.5, linetype = "dotted") +
                                  geom_line(aes(y = avg_Chrom_N2O_flux_mgm2h)) + # linetype = "Average N2O Flux - Chromatography")) +   , na.rm = TRUE) +
                                  scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                  theme_bw() +
                                  scale_y_continuous(breaks = seq(-0.5, 1, by = 0.5), limits=c(-0.9, 1.5)) +
                                  labs(y = expression(paste("Chromatography - N-", N[2], "O", " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                  xlab("Time") +
                                  ggtitle(expression(paste("N-", N[2], "O", " Emission rates Gasera vs. Chromatography"))) +
                                  scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                  geom_hline(yintercept = 0, color = "grey") +
                                  geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                  annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                  annotate('text', x = as.Date("2023-06-27"), y = 0.75, label = "Mid-Season Drainage", size = 4, color = "darkgrey", angle = '90') +
                                  annotate('text', x = as.Date("2023-10-01"), y = 0.75, label = "Growing Season", size = 4, color = "grey", angle = '90') +
                                  annotate('text', x = as.Date("2023-10-05"), y = 0.75, label = "Fallow Season", size = 4, color = "grey", angle = '270') +
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
                                    legend.position="top"
                                    , plot.margin = unit(c(1, 1, 0, 0.8), "lines")) +
                                  xlab(NULL) 

print(Rates_vs_time_N2O_CHROM)

# 2nd version, to arrange with CCH4 rates and water level:

Rates_vs_time_N2O_CHROM2 <- ggplot(data = Avg_rates_compare_TR_CHROM, aes(color = Treat, x = Sampling_date, y = avg_Chrom_N2O_flux_mgm2h, group = Treat)) +
                                  geom_line(data = Master_GHG_2023_CHROM, aes(x = Sampling_date, y = Chrom_N2O_flux_corrected, group = Plot), alpha = 0.5, linetype = "dotted") +
                                  geom_line(aes(y = avg_Chrom_N2O_flux_mgm2h)) + # linetype = "Average N2O Flux - Chromatography")) +   , na.rm = TRUE) +
                                  scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                  theme_bw() +
                                  labs(y = expression(paste("N-", N[2], "O", " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                  xlab("Time") +
                                  geom_hline(yintercept=0, color = "grey") +
                                  geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                  annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                  scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                  theme(
                                    axis.title.y = element_text(color = "black"), 
                                    axis.text.y = element_text(color = "black"),
                                    axis.title.y.right = element_text(color = "black"),
                                    axis.text.y.right = element_text(color = "black"),
                                    strip.background = element_blank(),
                                    strip.placement = "outside",
                                    legend.text = element_blank(),
                                    legend.position = "none",
                                    legend.title = element_blank(),
                                    axis.text.x = element_blank(),  
                                    plot.title = element_blank(), 
                                    plot.margin = unit(c(0, 1, 0, 0.8), "lines"),
                                    axis.title.x = element_blank()) +
                                  xlab(NULL) 

print(Rates_vs_time_N2O_CHROM2)

#### N2O - Gasera (not corrected) ####

Rates_vs_time_N2O_GASERA <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_N2O_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_NN2O_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_NN2O_flux_mgm2h)) + #, linetype = "Average N2O Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera - N-", N[2], "O", " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                    geom_hline(yintercept = 0, color = "grey") +
                                    geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                    annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                    theme(
                                      axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                      axis.text.y = element_text(color = "black"),
                                      axis.title.y.right = element_text(color = "black"),
                                      axis.text.y.right = element_text(color = "black"),
                                      strip.background = element_blank(),
                                      axis.text.x = element_blank(),
                                      strip.placement = "outside",
                                      legend.position="none",
                                      plot.margin = unit(c(0, 1, 0.15, 0.8), "lines")) +
                                    xlab(NULL) 

print(Rates_vs_time_N2O_GASERA)

#### N2O - Gasera (corrected) ####

Rates_vs_time_N2O_GASERA_cor <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_N2O_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_NN2O_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_NN2O_flux_mgm2h_cor)) + #, linetype = "Average N2O Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera (corrected) - N-", N[2], "O", " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                    geom_hline(yintercept = 0, color = "grey") +
                                    geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                    annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                    theme(
                                      axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                      axis.text.y = element_text(color = "black"),
                                      axis.title.y.right = element_text(color = "black"),
                                      axis.text.y.right = element_text(color = "black"),
                                      strip.background = element_blank(),
                                      axis.text.x = element_blank(),
                                      strip.placement = "outside",
                                      legend.position="none",
                                      plot.margin = unit(c(0, 1, 0, 0.8), "lines")) +
                                    xlab(NULL) 

print(Rates_vs_time_N2O_GASERA_cor)

#### N2O - Plot arrange ####

# Gasera not corrected:

Rates_vs_time_N2O_methods <- grid.arrange(arrangeGrob(Rates_vs_time_N2O_CHROM, Rates_vs_time_N2O_GASERA, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/N2O_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_N2O_methods)

# Gasera corrected:

Rates_vs_time_N2O_methods_gascor <- grid.arrange(arrangeGrob(Rates_vs_time_N2O_CHROM, Rates_vs_time_N2O_GASERA_cor, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/N2O_methods_gascor.pdf", width = 20, height = 12, plot = Rates_vs_time_N2O_methods_gascor)

## 2.4. CO2 ####

#### CO2 - Chromatography ####

Rates_vs_time_CO2_CHROM <- ggplot(data = Avg_rates_compare_TR_CHROM, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                  geom_line(data = Master_GHG_2023_CHROM, aes(x = Sampling_date, y = Chrom_CO2_flux_corrected, group = Plot), alpha = 0.5, linetype = "dotted") +
                                  geom_line(aes(y = avg_Chrom_CO2_flux_mgm2h)) + # linetype = "Average CO2 Flux - Chromatography")) +   , na.rm = TRUE) +
                                  scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                  theme_bw() +
                                  labs(y = expression(paste("Chromatography - C-", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                  xlab("Time") +
                                  ggtitle(expression(paste("C-", CO[2], " Emission rates Gasera vs. Chromatography"))) +
                                  scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                  geom_hline(yintercept = 0, color = "grey") +
                                  geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                  annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                  annotate('text', x = as.Date("2023-06-27"), y = 65, label = "Mid-Season Drainage", size = 4, color = "darkgrey", angle = '90') +
                                  annotate('text', x = as.Date("2023-10-01"), y = 75, label = "Growing Season", size = 4, color = "grey", angle = '90') +
                                  annotate('text', x = as.Date("2023-10-05"), y = 75, label = "Fallow Season", size = 4, color = "grey", angle = '270') +
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
                                    legend.position="top"
                                    , plot.margin = unit(c(1, 1, 0, 0.9), "lines")) +
                                  xlab(NULL) 

print(Rates_vs_time_CO2_CHROM)

#### CO2 - Gasera Transparent (not corrected) ####

Rates_vs_time_CO2_GASERA_TR <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                      geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CCO2_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                      geom_line(aes(y = avg_Gasera_CCO2_flux_mgm2h)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                      scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                      theme_bw() +
                                      labs(y = expression(paste("Gasera (Transp. ch.) - C-", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                      xlab("Time") +
                                      geom_hline(yintercept=0, color = "grey") +
                                      geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                      annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                      scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                      theme(
                                        axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                        axis.text.y = element_text(color = "black"),
                                        axis.title.y.right = element_text(color = "black"),
                                        axis.text.y.right = element_text(color = "black"),
                                        strip.background = element_blank(),
                                        axis.text.x = element_blank(),
                                        strip.placement = "outside",
                                        legend.position="none",
                                        plot.margin = unit(c(0, 1, 0, 0.6), "lines")) +
                                      xlab(NULL) 

print(Rates_vs_time_CO2_GASERA_TR)

#### CO2 - Gasera Dark (not corrected) ####

Rates_vs_time_CO2_GASERA_DK <- ggplot(data = Avg_rates_compare_DK_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                      geom_line(data = Master_GHG_2023_GASERA_DK, aes(x = Sampling_date, y = Gasera_CCO2_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                      geom_line(aes(y = avg_Gasera_CCO2_flux_mgm2h)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                      scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                      theme_bw() +
                                      labs(y = expression(paste("Gasera (Dark ch.) - C-", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                      xlab("Time") +
                                      geom_hline(yintercept=0, color = "grey") +
                                      geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                      annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                      scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                      theme(
                                        axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                        axis.text.y = element_text(color = "black"),
                                        axis.title.y.right = element_text(color = "black"),
                                        axis.text.y.right = element_text(color = "black"),
                                        strip.background = element_blank(),
                                        axis.text.x = element_blank(),
                                        strip.placement = "outside",
                                        legend.position="none",
                                        plot.margin = unit(c(0, 1, 0, 1), "lines")) +
                                      xlab(NULL) 

print(Rates_vs_time_CO2_GASERA_DK)

#### CO2 - Gasera Transparent (corrected) ####

Rates_vs_time_CO2_GASERA_TR_cor <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                        geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CCO2_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                        geom_line(aes(y = avg_Gasera_CCO2_flux_mgm2h_cor)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                        scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                        theme_bw() +
                                        labs(y = expression(paste("Gasera (Transp. ch. & corrected) - C-", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                        xlab("Time") +
                                        geom_hline(yintercept=0, color = "grey") +
                                        geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                        annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                        scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                        theme(
                                          axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                          axis.text.y = element_text(color = "black"),
                                          axis.title.y.right = element_text(color = "black"),
                                          axis.text.y.right = element_text(color = "black"),
                                          strip.background = element_blank(),
                                          axis.text.x = element_blank(),
                                          strip.placement = "outside",
                                          legend.position="none",
                                          plot.margin = unit(c(0, 1, 0, 0.6), "lines")) +
                                        xlab(NULL) 

print(Rates_vs_time_CO2_GASERA_TR_cor)

#### CO2 - Gasera Dark (corrected) ####

Rates_vs_time_CO2_GASERA_DK_cor <- ggplot(data = Avg_rates_compare_DK_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                        geom_line(data = Master_GHG_2023_GASERA_DK, aes(x = Sampling_date, y = Gasera_CCO2_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                        geom_line(aes(y = avg_Gasera_CCO2_flux_mgm2h_cor)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                        scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                        theme_bw() +
                                        labs(y = expression(paste("Gasera (Dark ch. & corrected) - C-", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                        xlab("Time") +
                                        geom_hline(yintercept=0, color = "grey") +
                                        geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                                        annotate("rect", xmin = as.Date("2023-06-22"), xmax = as.Date("2023-07-03"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
                                        scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
                                        theme(
                                          axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                                          axis.text.y = element_text(color = "black"),
                                          axis.title.y.right = element_text(color = "black"),
                                          axis.text.y.right = element_text(color = "black"),
                                          strip.background = element_blank(),
                                          axis.text.x = element_blank(),
                                          strip.placement = "outside",
                                          legend.position="none",
                                          plot.margin = unit(c(0, 1, 0, 1), "lines")) +
                                        xlab(NULL) 

print(Rates_vs_time_CO2_GASERA_DK_cor)

#### CO2 - Plot arrange ####

# Transparent chambers (Gasera not corrected):

Rates_vs_time_CO2_methods_TR <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_CHROM, Rates_vs_time_CO2_GASERA_TR, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_TR_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_CO2_methods_TR)

# Dark chambers (Gasera not corrected):

Rates_vs_time_CO2_methods_DK <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_CHROM, Rates_vs_time_CO2_GASERA_DK, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_DK_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_CO2_methods_DK)

# Transparent chambers (Gasera corrected):

Rates_vs_time_CO2_methods_TR_gascor <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_CHROM, Rates_vs_time_CO2_GASERA_TR_cor, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_TR_methods_gascor.pdf", width = 20, height = 12, plot = Rates_vs_time_CO2_methods_TR_gascor)

# Dark chambers (Gasera corrected):

Rates_vs_time_CO2_methods_DK_gascor <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_CHROM, Rates_vs_time_CO2_GASERA_DK_cor, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_DK_methods_gascor.pdf", width = 20, height = 12, plot = Rates_vs_time_CO2_methods_DK_gascor)

# 3. Plotting Methods ####

# Comparing data from both methods for sampling dates where both where used, during GS and Treat CON.

Master_CON_GS <- Master_GHG_2023 %>% 
                  filter(Treat == "CON" & Season == "GS" & Chamber_type %in% c("TR", NA) & !is.na(Gasera_CH4_flux_mgm2h) & !is.na(Chrom_CH4_flux_corrected)) %>% 
                  group_by(Sampling_date, Treat) %>% 
                  summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE), 
                            avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE), avg_Gasera_CCH4_flux_mgm2h = mean(Gasera_CCH4_flux_mgm2h, na.rm = TRUE), 
                            avg_Gasera_CCH4_flux_mgm2h_cor = mean(Gasera_CCH4_flux_mgm2h_cor, na.rm = TRUE),
                            avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                            avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE), avg_Gasera_NN2O_flux_mgm2h = mean(Gasera_NN2O_flux_mgm2h, na.rm = TRUE), 
                            avg_Gasera_NN2O_flux_mgm2h_cor = mean(Gasera_NN2O_flux_mgm2h_cor, na.rm = TRUE),
                            avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                            avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE), avg_Gasera_CCO2_flux_mgm2h = mean(Gasera_CCO2_flux_mgm2h, na.rm = TRUE), 
                            avg_Gasera_CCO2_flux_mgm2h_cor = mean(Gasera_CCO2_flux_mgm2h_cor, na.rm = TRUE))

## 3.1. CH4 ####

Master_CON_GS_CH4 <- Master_CON_GS %>% 
                      select(Sampling_date, Treat, avg_Gasera_CCH4_flux_mgm2h, avg_Gasera_CCH4_flux_mgm2h_cor, avg_Chrom_CH4_flux_mgm2h)

Master_CON_GS_CH4 <- Master_CON_GS_CH4[order(Master_CON_GS_CH4$avg_Chrom_CH4_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_CH4, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_CH4.xlsx") # Excel file with Master_CON_GS_CH4

# Gasera not corrected:

Versus_CH4_r2 <- Master_CON_GS_CH4 %>% # Calculating R-squared 
                  group_by(Treat) %>%
                  summarize(R_squared = summary(lm(avg_Gasera_CCH4_flux_mgm2h ~ avg_Chrom_CH4_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods_data_R2.pdf', width = 12)

Versus_CH4 <- ggplot(data = Master_CON_GS_CH4, aes(x = avg_Chrom_CH4_flux_mgm2h, y = avg_Gasera_CCH4_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, C-", CH[4]))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CH4_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CH4)

dev.off()

# Gasera corrected:

Versus_CH4_r2_cor <- Master_CON_GS_CH4 %>% # Calculating R-squared 
                      group_by(Treat) %>%
                      summarize(R_squared = summary(lm(avg_Gasera_CCH4_flux_mgm2h_cor ~ avg_Chrom_CH4_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods_data_R2_gascor.pdf', width = 12)

Versus_CH4_cor <- ggplot(data = Master_CON_GS_CH4, aes(x = avg_Chrom_CH4_flux_mgm2h, y = avg_Gasera_CCH4_flux_mgm2h_cor, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, C-", CH[4]))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CH4_r2_cor, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CH4_cor)

dev.off()

## 3.2. N2O ####

Master_CON_GS_N2O <- Master_CON_GS %>% 
                      select(Sampling_date, Treat, avg_Gasera_NN2O_flux_mgm2h, avg_Gasera_NN2O_flux_mgm2h_cor, avg_Chrom_N2O_flux_mgm2h)

Master_CON_GS_N2O <- Master_CON_GS_N2O[order(Master_CON_GS_N2O$avg_Chrom_N2O_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_N2O, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_N2O.xlsx") # Excel file with Master_CON_GS_N2O

# Gasera not corrected:

Versus_N2O_r2 <- Master_CON_GS_N2O %>% # Calculating R-squared 
                  group_by(Treat) %>%
                  summarize(R_squared = summary(lm(avg_Gasera_NN2O_flux_mgm2h ~ avg_Chrom_N2O_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/N2O_methods_data_R2.pdf', width = 12)

Versus_N2O <- ggplot(data = Master_CON_GS_N2O, aes(x = avg_Chrom_N2O_flux_mgm2h, y = avg_Gasera_NN2O_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, N-", N[2], "O"))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_N2O_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_N2O)

dev.off()

# Gasera corrected:

Versus_N2O_r2_cor <- Master_CON_GS_N2O %>% # Calculating R-squared 
                      group_by(Treat) %>%
                      summarize(R_squared = summary(lm(avg_Gasera_NN2O_flux_mgm2h_cor ~ avg_Chrom_N2O_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/N2O_methods_data_R2_gascor.pdf', width = 12)

Versus_N2O_cor <- ggplot(data = Master_CON_GS_N2O, aes(x = avg_Chrom_N2O_flux_mgm2h, y = avg_Gasera_NN2O_flux_mgm2h_cor, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, N-", N[2], "O"))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_N2O_r2_cor, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_N2O_cor)

dev.off()

## 3.2. CO2 ####

Master_CON_GS_DK <- Master_GHG_2023 %>% 
                    filter(Treat == "CON" & Season == "GS" & Chamber_type %in% c("DK", NA) & !is.na(Gasera_CH4_flux_mgm2h) & !is.na(Chrom_CH4_flux_corrected)) %>% 
                    group_by(Sampling_date, Treat) %>% 
                    summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE), 
                              avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE), avg_Gasera_CCH4_flux_mgm2h = mean(Gasera_CCH4_flux_mgm2h, na.rm = TRUE), 
                              avg_Gasera_CCH4_flux_mgm2h_cor = mean(Gasera_CCH4_flux_mgm2h_cor, na.rm = TRUE),
                              avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                              avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE), avg_Gasera_NN2O_flux_mgm2h = mean(Gasera_NN2O_flux_mgm2h, na.rm = TRUE), 
                              avg_Gasera_NN2O_flux_mgm2h_cor = mean(Gasera_NN2O_flux_mgm2h_cor, na.rm = TRUE),
                              avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                              avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE), avg_Gasera_CCO2_flux_mgm2h = mean(Gasera_CCO2_flux_mgm2h, na.rm = TRUE), 
                              avg_Gasera_CCO2_flux_mgm2h_cor = mean(Gasera_CCO2_flux_mgm2h_cor, na.rm = TRUE))

# Transparent chambers:

Master_CON_GS_CO2 <- Master_CON_GS %>% 
                      select(Sampling_date, Treat, avg_Gasera_CCO2_flux_mgm2h, avg_Gasera_CCO2_flux_mgm2h_cor, avg_Chrom_CO2_flux_mgm2h)

Master_CON_GS_CO2 <- Master_CON_GS_CO2[order(Master_CON_GS_CO2$avg_Chrom_CO2_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_CO2, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_CO2.xlsx") # Excel file with Master_CON_GS_CO2

# Dark chambers:

Master_CON_GS_CO2_DK <- Master_CON_GS_DK %>% 
                        select(Sampling_date, Treat, avg_Gasera_CCO2_flux_mgm2h, avg_Gasera_CCO2_flux_mgm2h_cor, avg_Chrom_CO2_flux_mgm2h)

Master_CON_GS_CO2_DK <- Master_CON_GS_CO2_DK[order(Master_CON_GS_CO2_DK$avg_Chrom_CO2_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_CO2_DK, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_CO2_DK.xlsx") # Excel file with Master_CON_GS_CO2_DK

# Gasera Transparent not corrected:

Versus_CO2_r2 <- Master_CON_GS_CO2 %>% # Calculating R-squared 
                  group_by(Treat) %>%
                  summarize(R_squared = summary(lm(avg_Gasera_CCO2_flux_mgm2h ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_methods_data_R2.pdf', width = 12)

Versus_CO2 <- ggplot(data = Master_CON_GS_CO2, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CCO2_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, C-", CO[2]))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CO2_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CO2)

dev.off()

# Gasera Dark not corrected:

Versus_CO2_DK_r2 <- Master_CON_GS_CO2_DK %>% # Calculating R-squared 
                    group_by(Treat) %>%
                    summarize(R_squared = summary(lm(avg_Gasera_CCO2_flux_mgm2h ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_DK_methods_data_R2.pdf', width = 12)

Versus_DK_CO2 <- ggplot(data = Master_CON_GS_CO2_DK, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CCO2_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, C-", CO[2]))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CO2_DK_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_DK_CO2)

dev.off()

# Gasera Transparent corrected:

Versus_CO2_r2_cor <- Master_CON_GS_CO2 %>% # Calculating R-squared 
                      group_by(Treat) %>%
                      summarize(R_squared = summary(lm(avg_Gasera_CCO2_flux_mgm2h_cor ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_methods_data_R2_gascor.pdf', width = 12)

Versus_CO2_cor <- ggplot(data = Master_CON_GS_CO2, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CCO2_flux_mgm2h_cor, group = Treat)) +
                          geom_point() +
                          ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, C-",CO[2]))) +
                          stat_poly_line() +
                          theme(plot.title = element_text(size = 10, face = "bold")) +
                          geom_text(data = Versus_CO2_r2_cor, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CO2_cor)

dev.off()

# Gasera Dark corrected:

Versus_CO2_DK_r2_cor <- Master_CON_GS_CO2_DK %>% # Calculating R-squared 
                        group_by(Treat) %>%
                        summarize(R_squared = summary(lm(avg_Gasera_CCO2_flux_mgm2h_cor ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_DK_methods_data_R2_gascor.pdf', width = 12)

Versus_DK_CO2_cor <- ggplot(data = Master_CON_GS_CO2_DK, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CCO2_flux_mgm2h_cor, group = Treat)) +
                            geom_point() +
                            ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, C-", CO[2]))) +
                            stat_poly_line() +
                            theme(plot.title = element_text(size = 10, face = "bold")) +
                            geom_text(data = Versus_CO2_DK_r2_cor, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_DK_CO2_cor)

dev.off()

# 4. Cumulative emissions ####

## 4.1. Cumulative CCH4 ####

# Total_boxplot (GS & PH):

# Complete version: with all titles, to be plotted alone (arrange only CCH4 acc GS, PH, Tot - "CH4_acc_arr"):

Acc_CH4_tot_plot <-  ggplot(Acc_CHROM_tot_sum, aes(x = Treat, y = CCH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Annual Emissions") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.x = element_text(size = 16),
                              axis.text.y = element_text(size = 16),
                              axis.title.y = element_text(size = 16),
                              plot.title = element_text(hjust = 0.5, size = 16))

print(Acc_CH4_tot_plot)

Acc_CH4_tot_plot_limits <- layer_scales(Acc_CH4_tot_plot)$y$get_limits() # extracting limits from plot Acc_CH4_tot_plot to use in Acc_CH4_GS_plot, so they coincide when arranging

# Arrange with NN2O acc version: without x text and title:

Acc_CH4_tot_plot2 <-  ggplot(Acc_CHROM_tot_sum, aes(x = Treat, y = CCH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Complete Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 0, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank())

print(Acc_CH4_tot_plot2)

# Total_dots (GS & PH):

Acc_CH4_tot_plot3 <- ggplot(Acc_CHROM_tot_sum, aes(Treat, CCH4_kgha_tot, group = Treat, colour = Treat, fill = Treat)) +
                            geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                            scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                            scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                            ggtitle("Complete Season") +
                            theme_bw() +
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 0, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank(),
                              legend.position = "none")+
                            geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot), shape = 19, colour = "black", size = 6) +
                            geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot), shape = 19, size = 5) +
                            geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot, ymin =  mean_CCH4_kgha_tot - se_CCH4_kgha_tot, 
                                                                            ymax =  mean_CCH4_kgha_tot + se_CCH4_kgha_tot), width = 0.3, size = 0.5) 

print(Acc_CH4_tot_plot3)

# GS_boxplot:

# Complete version: with all titles, to be plotted alone (arrange only CCH4 acc GS, PH, Tot - "CH4_acc_arr"):

Acc_CH4_GS_plot <-  ggplot(Acc_CHROM_GS_sum, aes(x = Treat, y = CCH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Growing Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.text.x = element_text(size = 16),
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5, size = 16))

print(Acc_CH4_GS_plot)

# Arrange with NN2O acc version: without x text and title:

Acc_CH4_GS_plot2 <-  ggplot(Acc_CHROM_GS_sum, aes(x = Treat, y = CCH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Growing Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 0, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank())

print(Acc_CH4_GS_plot2)

# GS_dots:

Acc_CH4_GS_plot3 <- ggplot(Acc_CHROM_GS_sum, aes(Treat, CCH4_kgha_tot, group = Treat, colour = Treat, fill = Treat)) +
                            geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                            scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                            scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                            ggtitle("Growing Season") +
                            theme_bw() +
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 0, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank(),
                              legend.position = "none")+
                            geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot), shape = 19, colour = "black", size = 6) +
                            geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot), shape = 19, size = 5) +
                            geom_errorbar(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot, ymin =  mean_CCH4_kgha_tot - se_CCH4_kgha_tot, 
                                                  ymax =  mean_CCH4_kgha_tot + se_CCH4_kgha_tot), width = 0.3, size = 0.5) 

print(Acc_CH4_GS_plot3)

# PH_boxplot:

# Complete version: with all titles, to be plotted alone (arrange only CCH4 acc GS, PH, Tot - "CH4_acc_arr"):

Acc_CH4_PH_plot <-  ggplot(Acc_CHROM_PH_sum, aes(x = Treat, y = CCH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "Irrigation Strategies", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Fallow Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = ggplot2::margin(l = 0, r = 5, t = 25, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.x = element_text(size = 16),
                              axis.title.x = element_text(size = 16),
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5, size = 16))

print(Acc_CH4_PH_plot)

# Arrange with NN2O acc version: without x text and title:

Acc_CH4_PH_plot2 <-  ggplot(Acc_CHROM_PH_sum, aes(x = Treat, y = CCH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "Irrigation Strategies", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Fallow season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = ggplot2::margin(l = 0, r = 5, t = 25, b = 0, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank())

print(Acc_CH4_PH_plot2)

# PH_dots:

Acc_CH4_PH_plot3 <- ggplot(Acc_CHROM_PH_sum, aes(Treat, CCH4_kgha_tot, group = Treat, colour = Treat, fill = Treat)) +
                            geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                            scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                            scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                            ggtitle("Fallow season") +
                            theme_bw() +
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme(plot.margin = ggplot2::margin(l = 0, r = 5, t = 25, b = 0, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank(),
                              legend.position = "none")+
                            geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot), shape = 19, colour = "black", size = 6) +
                            geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot), shape = 19, size = 5) +
                            geom_errorbar(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y =  mean_CCH4_kgha_tot, ymin =  mean_CCH4_kgha_tot - se_CCH4_kgha_tot, 
                                                                           ymax =  mean_CCH4_kgha_tot + se_CCH4_kgha_tot), width = 0.3, size = 0.5) 

print(Acc_CH4_PH_plot3)

## 4.2. Cumulative NN2O ####

# Total_boxplot (GS & PH):

Acc_N2O_tot_plot <-  ggplot(Acc_CHROM_tot_sum, aes(x = Treat, y = NN2O_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "", y = expression(paste("Cumulative ",N-N[2], "O emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Complete Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 10, t = 0, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(-2, 5), breaks = seq(-2, 5, by = 1)) +
                            theme(
                              plot.title = element_blank())

print(Acc_N2O_tot_plot)

Acc_N2O_tot_plot_limits <- layer_scales(Acc_N2O_tot_plot)$y$get_limits() # extracting limits from plot Acc_N2O_tot_plot to use in Acc_N2O_tot_plot, so they coincide when arranging

# Total_dots (GS & PH):

Acc_N2O_tot_plot2 <- ggplot(Acc_CHROM_tot_sum, aes(Treat, NN2O_kgha_tot, group = Treat, colour = Treat, fill = Treat)) +
                            geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                            scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                            scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                            ggtitle("Complete Season") +
                            theme_bw() +
                            labs(x = "", y = expression(paste("Cumulative ",N-N[2], "O emissions (kg ", ha^-1, ")"))) +
                            # theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 0, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(-2, 5), breaks = seq(-2, 5, by = 1)) +
                            theme(
                              plot.title = element_text(hjust = 0.5),
                              legend.position = "none",
                              plot.margin = margin(l = 0, r = 10, t = 0, b = 14, unit = "pt")) +
                            geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot), shape = 19, colour = "black", size = 6) +
                            geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot), shape = 19, size = 5) +
                            geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot, ymin =  mean_NN2O_kgha_tot - se_NN2O_kgha_tot, 
                                                                            ymax =  mean_NN2O_kgha_tot + se_NN2O_kgha_tot), width = 0.3, size = 0.5) 

print(Acc_N2O_tot_plot2)

# GS_boxplot:

Acc_N2O_GS_plot <-  ggplot(Acc_CHROM_GS_sum, aes(x = Treat, y = NN2O_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "", y = expression(paste("Cumulative ",N-N[2], "O emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Growing Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 0, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(-2, 5), breaks = seq(-2, 5, by = 1)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_blank())

print(Acc_N2O_GS_plot)

# GS_dots:

Acc_N2O_GS_plot2 <- ggplot(Acc_CHROM_GS_sum, aes(Treat, NN2O_kgha_tot, group = Treat, colour = Treat, fill = Treat)) +
                            geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                            scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                            scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                            ggtitle("Growing Season") +
                            theme_bw() +
                            labs(x = "", y = expression(paste("Cumulative ",N-N[2], "O emissions (kg ", ha^-1, ")"))) +
                            scale_y_continuous(position = "right", limits = c(-2, 5), breaks = seq(-2, 5, by = 1)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              legend.position = "none",
                              plot.title = element_text(hjust = 0.5),
                              plot.margin = margin(l = 0, r = 5, t = 0, b = 14, unit = "pt")) +
                            geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot), shape = 19, colour = "black", size = 6) +
                            geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot), shape = 19, size = 5) +
                            geom_errorbar(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot, ymin =  mean_NN2O_kgha_tot - se_NN2O_kgha_tot, 
                                                                            ymax =  mean_NN2O_kgha_tot + se_NN2O_kgha_tot), width = 0.3, size = 0.5) 

print(Acc_N2O_GS_plot2)

# PH_boxplot:

Acc_N2O_PH_plot <-  ggplot(Acc_CHROM_PH_sum, aes(x = Treat, y = NN2O_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "Irrigation Strategies", y = expression(paste("Cumulative ",N-N[2], "O emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Fallow Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 0, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(-2, 5), breaks = seq(-2, 5, by = 1)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_blank())

print(Acc_N2O_PH_plot)

# PH_dots:

Acc_N2O_PH_plot2 <- ggplot(Acc_CHROM_PH_sum, aes(Treat, NN2O_kgha_tot, group = Treat, colour = Treat, fill = Treat)) +
                            geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                            scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                            scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                            ggtitle("Fallow Season") +
                            theme_bw() +
                            labs(x = "Irrigation Strategies", y = expression(paste("Cumulative ",N-N[2], "O emissions (kg ", ha^-1, ")"))) +
                            scale_y_continuous(position = "right", limits = c(-2, 5), breaks = seq(-2, 5, by = 1)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              legend.position = "none",
                              plot.title = element_text(hjust = 0.5),
                              plot.margin = margin(l = 0, r = 5, t = 0, b = 14, unit = "pt")) +
                            geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot), shape = 19, colour = "black", size = 6) +
                            geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot), shape = 19, size = 5) +
                            geom_errorbar(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y =  mean_NN2O_kgha_tot, ymin =  mean_NN2O_kgha_tot - se_NN2O_kgha_tot, 
                                                                           ymax =  mean_NN2O_kgha_tot + se_NN2O_kgha_tot), width = 0.3, size = 0.5) 

print(Acc_N2O_PH_plot2)

## 4.3. Plot arrange ####

# Flux and cumulative emission plots:

CH4_acc_arr <- grid.arrange(arrangeGrob(Acc_CH4_GS_plot, Acc_CH4_PH_plot, Acc_CH4_tot_plot, nrow = 1, ncol = 3, widths = c(0.3, 0.3, 0.4) )) # CCH4 acc with all titles - BOXPLOTS
CH4_acc_arr2 <- grid.arrange(arrangeGrob(Acc_CH4_GS_plot2, Acc_CH4_PH_plot2, Acc_CH4_tot_plot2, nrow = 1, ncol = 3, widths = c(0.3, 0.3, 0.4) )) # CCH4 acc adapted to arrange with NN2O acc - BOXPLOTS
CH4_acc_arr3 <- grid.arrange(arrangeGrob(Acc_CH4_GS_plot3, Acc_CH4_PH_plot3, Acc_CH4_tot_plot3, nrow = 1, ncol = 3, widths = c(0.3, 0.3, 0.4) )) # CCH4 acc adapted to arrange with NN2O acc - DOTS
CH4_acc_rate_arr <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM_arr, CH4_acc_arr, nrow = 1, ncol = 2, widths = c(0.6, 0.4))) # CCH4 acc arrange with all titles
N2O_acc_arr <- grid.arrange(arrangeGrob(Acc_N2O_GS_plot, Acc_N2O_PH_plot, Acc_N2O_tot_plot, nrow = 1, ncol = 3, widths = c(0.3, 0.3, 0.4) )) # NN2O acc adapted to arrange with CCH4 acc - BOXPLOTS
N2O_acc_arr2 <- grid.arrange(arrangeGrob(Acc_N2O_GS_plot2, Acc_N2O_PH_plot2, Acc_N2O_tot_plot2, nrow = 1, ncol = 3, widths = c(0.3, 0.3, 0.4) )) # NN2O acc adapted to arrange with CCH4 acc - DOTS
N2O_CH4_acc_arr <- grid.arrange(arrangeGrob(CH4_acc_arr2, N2O_acc_arr, nrow = 2, ncol = 1)) # CCH4 and NN2O acc arrange - BOXPLOTS
N2O_CH4_acc_arr2 <- grid.arrange(arrangeGrob(CH4_acc_arr3, N2O_acc_arr2, nrow = 2, ncol = 1)) # CCH4 and NN2O acc arrange - DOTS
CH4_N2O_acc_rate_arr <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_N2O_CHROM_arr, N2O_CH4_acc_arr, nrow = 1, ncol = 2, widths = c(0.6, 0.4))) # CCH4 and NN2O flUx and acc arrange - BOXPLOTS
CH4_N2O_acc_rate_arr2 <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_N2O_CHROM_arr, N2O_CH4_acc_arr2, nrow = 1, ncol = 2, widths = c(0.6, 0.4))) # CCH4 and NN2O flUx and acc arrange - DOTS

ggsave("outputs/CERESTRES_results/Chromat_results/CH4_flux_water_acc.pdf", width = 20, height = 10, plot = CH4_acc_rate_arr)
ggsave("outputs/CERESTRES_results/Chromat_results/CH4_N2O_flux_water_acc.pdf", width = 20, height = 10, plot = CH4_N2O_acc_rate_arr) # - BOXPLOTS
ggsave("outputs/CERESTRES_results/Chromat_results/CH4_N2O_flux_water_acc2.pdf", width = 20, height = 10, plot = CH4_N2O_acc_rate_arr2) # - DOTS

# 5. GWP ####

## 5.1. GWP Boxplots ####

GWP_tot_boxplot <-  ggplot(Acc_CHROM_tot_sum, aes(x = Treat, y = GWP, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(y = expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) +
                            theme_bw()+
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            theme(
                              axis.title.x = element_blank(),
                              plot.title = element_text(hjust = 0.5))

print(GWP_tot_boxplot)

ggsave("outputs/CERESTRES_results/Chromat_results/GWP_boxplot.pdf", plot = GWP_tot_boxplot ,width = 10, height = 10)

## 5.2. GWP dots ####

GWP_tot_dots <- ggplot(Acc_CHROM_tot_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
                    geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
                    scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                    scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                    theme_bw() +
                    ylab(expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) +
                    theme(
                          axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_blank(), legend.position = "none", 
                          axis.text.y = element_text(margin = margin(r = 0)), panel.border = element_rect(size = 1)) +
                    geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP), shape = 19, colour = "black", size = 12) +
                    geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP), shape = 19, size = 10) +
                    geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP, ymin = mean_GWP - se_GWP, ymax = mean_GWP + se_GWP), width = 0.3, size = 1) 

print(GWP_tot_dots)

ggsave("outputs/CERESTRES_results/Chromat_results/GWP_dots.pdf", plot = GWP_tot_dots ,width = 10, height = 10)
 
# Complete Season: Second version, to arrange with GS and PH.

GWP_tot_dots2 <- ggplot(Acc_CHROM_tot_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
                          geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                          scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                          scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                          theme_bw() +
                          ggtitle("Complete Season") +
                          scale_y_continuous(position = "right", limits = c(500, 20000), breaks = seq(0, 20000, by = 4000)) +
                          ylab(expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) +
                          theme(
                            plot.margin = margin(l = 0, r = 5, t = 0, b = 14, unit = "pt"),
                            axis.title.y = element_text(margin = margin(r = 12)), 
                            axis.title.x = element_blank(), 
                            legend.position = "none", 
                            axis.text.y = element_text(margin = margin(r = 0)), 
                            # panel.border = element_rect(size = 1),
                            plot.title = element_text(hjust = 0.5)) +
                          geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP), shape = 19, colour = "black", size = 6) +
                          geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP), shape = 19, size = 5) +
                          geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP, ymin = mean_GWP - se_GWP, ymax = mean_GWP + se_GWP), width = 0.3, size = 0.5) 

print(GWP_tot_dots2)

layer_scales(GWP_tot_dots2)$y$get_limits() # extracting limits from plot GWP_tot_dots2 to use in GS and PH plots, so they coincide when arranging

# Complete Season: Third version (To arrange with GWPY and Yield)

GWP_tot_dots3 <- ggplot(Acc_CHROM_tot_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
                        geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                        scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                        scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                        theme_bw() +
                        ggtitle("Complete Season") +
                        scale_y_continuous(position = "left", limits = c(500, 20000), breaks = seq(0, 20000, by = 4000)) +
                        scale_x_discrete(position = "top") +
                        ylab(expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) +
                        theme(
                          plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                          axis.title.y = element_blank(), 
                          axis.title.x = element_blank(), 
                          legend.position = "none", 
                          axis.text.y = element_blank(), 
                          # panel.border = element_rect(size = 1),
                          plot.title = element_text(hjust = 0.5)) +
                        geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP), shape = 19, colour = "black", size = 6) +
                        geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP), shape = 19, size = 5) +
                        geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWP, ymin = mean_GWP - se_GWP, ymax = mean_GWP + se_GWP), width = 0.3, size = 0.5) 

print(GWP_tot_dots3)

# GS

GWP_GS_dots <- ggplot(Acc_CHROM_GS_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
                        geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                        scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                        scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                        theme_bw() +
                        labs(x ="Irrigation Strategies") +
                        ggtitle("Growing Season") +
                        scale_y_continuous(position = "right", limits = c(500, 20000), breaks = seq(0, 20000, by = 4000)) +
                        # ylab(expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) + 
                        theme(
                          plot.margin = margin(l = 0, r = 5, t = 0, b = 14, unit = "pt"),
                          axis.title.y = element_blank(), 
                          axis.title.x = element_blank(), 
                          legend.position = "none", 
                          axis.text.y = element_blank(), 
                          # panel.border = element_rect(size = 1),
                          plot.title = element_text(hjust = 0.5)) +
                        geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWP), shape = 19, colour = "black", size = 6) +
                        geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWP), shape = 19, size = 5) +
                        geom_errorbar(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWP, ymin = mean_GWP - se_GWP, ymax = mean_GWP + se_GWP), width = 0.3, size = 0.5) 

print(GWP_GS_dots)

layer_scales(GWP_GS_dots)$y$get_limits() # extracting limits

# GS - Second version (To arrange with GWPY and Yield)

GWP_GS_dots2 <- ggplot(Acc_CHROM_GS_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
                      geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                      scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                      scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                      theme_bw() +
                      ggtitle("Growing Season") +
                      scale_x_discrete(position = "top") +
                      scale_y_continuous(position = "left", limits = c(500, 20000), breaks = seq(0, 20000, by = 4000)) +
                      ylab(expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) + 
                      theme(
                        plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                        axis.title.y = element_text(margin = margin(r = 0)), 
                        axis.title.x = element_blank(), 
                        legend.position = "none", 
                        axis.text.y = element_text(margin = margin(r = 0)),
                        # panel.border = element_rect(size = 1),
                        plot.title = element_text(hjust = 0.5)) +
                      geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWP), shape = 19, colour = "black", size = 6) +
                      geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWP), shape = 19, size = 5) +
                      geom_errorbar(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWP, ymin = mean_GWP - se_GWP, ymax = mean_GWP + se_GWP), width = 0.3, size = 0.5) 

print(GWP_GS_dots2)

# PH

GWP_PH_dots <- ggplot(Acc_CHROM_PH_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
                      geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                      scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                      scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                      theme_bw() +
                      labs(x = "Irrigation Strategies") +
                      ggtitle("Fallow Season") +
                      scale_y_continuous(position = "right", limits = c(500, 20000), breaks = seq(0, 20000, by = 4000)) +
                      # ylab(expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) +
                      theme(
                        plot.margin = margin(l = 0, r = 5, t = 0, b = 0, unit = "pt"),
                        axis.title.y = element_blank(), 
                        legend.position = "none", 
                        axis.text.y = element_blank(), 
                        # panel.border = element_rect(size = 1),
                        plot.title = element_text(hjust = 0.5)) +
                      geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWP), shape = 19, colour = "black", size = 6) +
                      geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWP), shape = 19, size = 5) +
                      geom_errorbar(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWP, ymin = mean_GWP - se_GWP, ymax = mean_GWP + se_GWP), width = 0.3, size = 0.5) 

print(GWP_PH_dots)

# PH - Second version (To arrange with GWPY and Yield)

GWP_PH_dots2 <- ggplot(Acc_CHROM_PH_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
                        geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                        scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                        scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                        theme_bw() +
                        ggtitle("Fallow Season") +
                        scale_x_discrete(position = "top") +
                        scale_y_continuous(position = "left", limits = c(500, 20000), breaks = seq(0, 20000, by = 4000)) +
                        theme(
                          plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                          axis.title.y = element_blank(), 
                          axis.title.x = element_blank(),
                          legend.position = "none", 
                          axis.text.y = element_blank(), 
                          # panel.border = element_rect(size = 1),
                          plot.title = element_text(hjust = 0.5)) +
                        geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWP), shape = 19, colour = "black", size = 6) +
                        geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWP), shape = 19, size = 5) +
                        geom_errorbar(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWP, ymin = mean_GWP - se_GWP, ymax = mean_GWP + se_GWP), width = 0.3, size = 0.5) 

print(GWP_PH_dots2)

## 5.3. Plot arrange ####

GWP_dots_arr <- grid.arrange(arrangeGrob(GWP_GS_dots, GWP_PH_dots, GWP_tot_dots2, nrow = 1, ncol = 3, widths = c(0.3, 0.3, 0.4))) # GWP plots arranged (GS, PH and Tot)
GWP_dots_arr2 <- grid.arrange(arrangeGrob(GWP_GS_dots2, GWP_PH_dots2, GWP_tot_dots3, nrow = 1, ncol = 3, widths = c(0.4, 0.3, 0.3))) # GWP plots arranged (GS, PH and Tot)

ggsave("outputs/CERESTRES_results/Chromat_results/GWP_dots_arr.pdf", plot = GWP_dots_arr ,width = 10, height = 10)   
ggsave("outputs/CERESTRES_results/Chromat_results/GWP_dots_arr2.pdf", plot = GWP_dots_arr2 ,width = 8, height = 10) 

# 6. Yield-scaled GWP (GWPY)) ####

## 6.1. Yield plot ####

Yield_2023_plot <- ggplot(Acc_CHROM_tot_sum, aes(Treat, Yield_Mgha_14perc, group = Treat, colour = Treat, fill = Treat)) +
                            geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                            scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                            scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                            theme_bw() +
                            labs(x = "Irrigation Strategies") +
                            # scale_x_discrete(position = "top") +
                            scale_y_continuous(limits = c(5.5, 8.5), breaks = seq(5.5, 8.5, by = 0.5)) +
                            ylab(expression(paste("Grain Yield (Mg ", ha^-1, ")"))) +
                            theme(
                              plot.margin = margin(l = 7.5, r = 5, t = 0, b = 10, unit = "pt"), 
                              legend.position = "none", 
                              plot.title = element_blank()) +
                            geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_Yield_Mgha_14perc), shape = 19, colour = "black", size = 6) +
                            geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_Yield_Mgha_14perc), shape = 19, size = 5) +
                            geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_Yield_Mgha_14perc, ymin = mean_Yield_Mgha_14perc - se_Yield_Mgha_14perc,
                                                                           ymax = mean_Yield_Mgha_14perc + se_Yield_Mgha_14perc), width = 0.3, size = 0.5) 

print(Yield_2023_plot)

## 6.2. GWPY - Complete Season ####

GWPY_tot <- ggplot(Acc_CHROM_tot_sum, aes(Treat, GWPY, group = Treat, colour = Treat, fill = Treat)) +
                        geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                        scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                        scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                        theme_bw() +
                        ggtitle("Complete Season") +
                        scale_y_continuous(position = "left", limits = c(0, 3500), breaks = seq(0, 3500, by = 500)) +
                        scale_x_discrete(position = "top") +
                        ylab(expression(paste(GWP[Y], " (kg ", CO[2], " eq ", Mg^-1, ")"))) +
                        theme(
                          plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                          axis.title.y = element_blank(), 
                          axis.title.x = element_blank(), 
                          legend.position = "none", 
                          axis.text.y = element_blank(), 
                          # panel.border = element_rect(size = 1),
                          plot.title = element_text(hjust = 0.5)) +
                        geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWPY), shape = 19, colour = "black", size = 6) +
                        geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWPY), shape = 19, size = 5) +
                        geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWPY, ymin = mean_GWPY - se_GWPY, ymax = mean_GWPY + se_GWPY), width = 0.3, size = 0.5) 

print(GWPY_tot)

# Second version: To arrange with GWP and Yield

GWPY_tot2 <- ggplot(Acc_CHROM_tot_sum, aes(Treat, GWPY, group = Treat, colour = Treat, fill = Treat)) +
                    geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                    scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                    scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                    theme_bw() +
                    scale_y_continuous(position = "left", limits = c(0, 3500), breaks = seq(0, 3500, by = 500)) +
                    ylab(expression(paste(GWP[Y], " (kg ", CO[2], " eq ", Mg^-1, ")"))) +
                    theme(
                      plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                      axis.title.y = element_blank(), 
                      axis.title.x = element_blank(), 
                      legend.position = "none", 
                      axis.text.y = element_blank(), 
                      axis.text.x = element_blank(), 
                      axis.ticks.x=element_blank(),
                      # panel.border = element_rect(size = 1),
                      plot.title = element_blank()) +
                    geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWPY), shape = 19, colour = "black", size = 6) +
                    geom_point(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWPY), shape = 19, size = 5) +
                    geom_errorbar(data = Avg_Acc_CHROM_tot_sum, aes(x = Treat, y = mean_GWPY, ymin = mean_GWPY - se_GWPY, ymax = mean_GWPY + se_GWPY), width = 0.3, size = 0.5) 

print(GWPY_tot2)

## 6.3. GWPY - Growing Season ####

GWPY_GS <- ggplot(Acc_CHROM_GS_sum, aes(Treat, GWPY, group = Treat, colour = Treat, fill = Treat)) +
                    geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                    scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                    scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                    theme_bw() +
                    ggtitle("Growing Season") +
                    scale_y_continuous(position = "left", limits = c(0, 3500), breaks = seq(0, 3500, by = 500)) +
                    scale_x_discrete(position = "top") +
                    ylab(expression(paste(GWP[Y], " (kg ", CO[2], " eq ", Mg^-1, ")"))) +
                    theme(
                      plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                      axis.title.y = element_text(margin = margin(r = 3)), 
                      axis.title.x = element_blank(), 
                      legend.position = "none", 
                      axis.text.y = element_text(margin = margin(r = 0)), 
                      # panel.border = element_rect(size = 1),
                      plot.title = element_text(hjust = 0.5)) +
                    geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWPY), shape = 19, colour = "black", size = 6) +
                    geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWPY), shape = 19, size = 5) +
                    geom_errorbar(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWPY, ymin = mean_GWPY - se_GWPY, ymax = mean_GWPY + se_GWPY), width = 0.3, size = 0.5) 

print(GWPY_GS)

# Second version: To arrange with GWP and Yield

GWPY_GS2 <- ggplot(Acc_CHROM_GS_sum, aes(Treat, GWPY, group = Treat, colour = Treat, fill = Treat)) +
                    geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                    scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D")) +
                    scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                    theme_bw() +
                    scale_y_continuous(position = "left", limits = c(0, 3500), breaks = seq(0, 3500, by = 500)) +
                    ylab(expression(paste(GWP[Y], " (kg ", CO[2], " eq ", Mg^-1, ")"))) +
                    theme(
                      plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                      axis.title.y = element_text(margin = margin(r = 5)), 
                      axis.title.x = element_blank(),
                      axis.ticks.x=element_blank(),
                      legend.position = "none", 
                      axis.text.y = element_text(margin = margin(r = 0)), 
                      axis.text.x = element_blank(), 
                      # panel.border = element_rect(size = 1),
                      plot.title = element_blank()) +
                    geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWPY), shape = 19, colour = "black", size = 6) +
                    geom_point(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWPY), shape = 19, size = 5) +
                    geom_errorbar(data = Avg_Acc_CHROM_GS_sum, aes(x = Treat, y = mean_GWPY, ymin = mean_GWPY - se_GWPY, ymax = mean_GWPY + se_GWPY), width = 0.3, size = 0.5) 

print(GWPY_GS2)

## 6.4. GWPY - Post-Harvest ####

GWPY_PH <- ggplot(Acc_CHROM_PH_sum, aes(Treat, GWPY, group = Treat, colour = Treat, fill = Treat)) +
                    geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                    scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                    scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                    theme_bw() +
                    ggtitle("Fallow Season") +
                    scale_y_continuous(position = "left", limits = c(0, 3500), breaks = seq(0, 3500, by = 500)) +
                    scale_x_discrete(position = "top") +
                    ylab(expression(paste(GWP[Y], " (kg ", CO[2], " eq ", Mg^-1, ")"))) +
                    theme(
                      plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                      axis.title.y = element_blank(), 
                      axis.title.x = element_blank(), 
                      legend.position = "none", 
                      axis.text.y = element_blank(), 
                      # panel.border = element_rect(size = 1),
                      plot.title = element_text(hjust = 0.5)) +
                    geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWPY), shape = 19, colour = "black", size = 6) +
                    geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWPY), shape = 19, size = 5) +
                    geom_errorbar(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWPY, ymin = mean_GWPY - se_GWPY, ymax = mean_GWPY + se_GWPY), width = 0.3, size = 0.5) 

print(GWPY_PH)

# Second version: To arrange with GWP and Yield

GWPY_PH2 <- ggplot(Acc_CHROM_PH_sum, aes(Treat, GWPY, group = Treat, colour = Treat, fill = Treat)) +
                    geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 5)+
                    scale_colour_manual(name = "Irrigation Strategies: ", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                    scale_fill_manual(values = c("#002B5B", "#03C988", "#FF5D5D"), guide = "none") +
                    theme_bw() +
                    scale_y_continuous(position = "left", limits = c(0, 3500), breaks = seq(0, 3500, by = 500)) +
                    ylab(expression(paste(GWP[Y], " (kg ", CO[2], " eq ", Mg^-1, ")"))) +
                    theme(
                      plot.margin = margin(l = 0, r = 5, t = 0, b = 5, unit = "pt"),
                      axis.title.y = element_blank(), 
                      axis.title.x = element_blank(), 
                      legend.position = "none", 
                      axis.text.y = element_blank(), 
                      axis.ticks.x=element_blank(),
                      axis.text.x = element_blank(), 
                      # panel.border = element_rect(size = 1),
                      plot.title = element_blank()) +
                    geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWPY), shape = 19, colour = "black", size = 6) +
                    geom_point(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWPY), shape = 19, size = 5) +
                    geom_errorbar(data = Avg_Acc_CHROM_PH_sum, aes(x = Treat, y = mean_GWPY, ymin = mean_GWPY - se_GWPY, ymax = mean_GWPY + se_GWPY), width = 0.3, size = 0.5) 

print(GWPY_PH2)

## 6.5. Plot arrange ####

GWPY_arr <- grid.arrange(arrangeGrob(GWPY_GS, GWPY_PH, GWPY_tot, nrow = 1, ncol = 3, widths = c(0.4, 0.3, 0.3) )) # GWPY plots arranged (GS, PH and Tot)
GWPY_arr2 <- grid.arrange(arrangeGrob(GWPY_GS2, GWPY_PH2, GWPY_tot2, nrow = 1, ncol = 3, widths = c(0.4, 0.3, 0.3) )) # GWPY plots arranged (GS, PH and Tot)
Yield_GWPY_arr <- grid.arrange(arrangeGrob(GWPY_arr, Yield_2023_plot, nrow = 2, ncol = 1, heights = c(0.7, 0.3))) # Yield and GWPY plots arranged 
Yield_GWP_GWPY_arr <- grid.arrange(arrangeGrob(GWP_dots_arr2, GWPY_arr2, Yield_2023_plot, nrow = 3, ncol = 1, heights = c(0.4, 0.4, 0.3))) # Yield, GWP and GWPY plots arranged 

ggsave("outputs/CERESTRES_results/Chromat_results/Yield_GWPY_arr.pdf", plot = Yield_GWPY_arr ,width = 10, height = 10)   
ggsave("outputs/CERESTRES_results/Chromat_results/Yield_GWP_GWPY_arr.pdf", plot = Yield_GWP_GWPY_arr ,width = 10, height = 10) 

# 7. Weather plot ####

Weather <- read.csv("data/Meteocat_temp.csv", fileEncoding="latin1", na.strings=c("","NA")) # Field sheet with weather data from Meteocat station within the experimental station

Weather$Date2 <- as.Date(Weather$Date2)

pdf("outputs/Meteocat_temp.pdf", width = 7, height = 5)

plot(Weather$Date2, Weather$CAT_temp_med, type = "l", col = "red", lwd = 2, # Left y-axis: temperature.
        ylab = "", xlab = "Month",
        ylim = range(c(0, 30)))
axis(side = 2, col.axis = "red", col = "red", lwd = 2)  # Right y-axis

par(new = TRUE)  # Allow for a second plot
plot(Weather$Date2, Weather$PPT, type = "l", col = "blue", lwd = 2, 
       axes = FALSE, xlab = "", ylab = "", 
       ylim = range(c(0, 100)))

axis(side = 4, col.axis = "blue", col = "blue", lwd = 2)  # Right y-axis

# text(par("usr")[2], mean(Weather$PPT), labels = "Precipitation (mm)", 
     # srt = 180, xpd = TRUE, col = "blue", pos = 4)
 
legend("topright", legend = c("Temperature (ºC)", "Precipitation (mm)"), 
       col = c("red", "blue"), lwd = 2)

dev.off()

# 8. Mesocosm vs field conditions plot #### 
# Supporting mesocosms being representative of field conditions. 

Field_FS_phys <- read.csv("data/2017-2019-PostCollita_BASE DADES GEH.csv", fileEncoding="latin1", na.strings=c("","NA")) # Field sheet with physicochemical data years 2017/18 (Maria Belenguer's paper)

Field_FS_phys <- Field_FS_phys %>% 
  filter(Sampling_date != "2017-10-20") # date removed due to note on field sheet about not trusting the measured values

Field_FS_phys_2017_18 <- Field_FS_phys %>% # Field Physicochemical parameters FS 2017
        filter(Year %in% c(2017, 2018),
               Month %in% c(10, 11, 12),
               Tr1 == "WFL",
               Tr2 == "ESI") %>%
        select(Sampling_date, Year, Conduct_microS_cm, pH_soil, Temp_10_cm, Redox_pot)
Field_FS_phys_2017_18$Sampling_date <- as.Date(Field_FS_phys_2017_18$Sampling_date)
Field_FS_phys_2017_18$Year <- as.character(Field_FS_phys_2017_18$Year)
Field_FS_phys_2017_18$Month <- format(Field_FS_phys_2017_18$Sampling_date, "%B")

Meso_FS_phys_2023 <- Master_GHG_2023_phys_noNA %>% 
        filter(Season == "PH") %>%
        select(Sampling_date, Conduct_microS_cm, pH_soil, Temp_10_cm, Redox_pot)
Meso_FS_phys_2023$Year <- format(Meso_FS_phys_2023$Sampling_date, "%Y") 
Meso_FS_phys_2023$Month <- format(Meso_FS_phys_2023$Sampling_date, "%B")

Meso_vs_Field <- bind_rows(Field_FS_phys_2017_18, Meso_FS_phys_2023) %>% 
        filter(!is.na(Conduct_microS_cm)) %>% 
        filter(!is.na(pH_soil)) %>%
        group_by(Sampling_date, Year, Month) %>% 
        summarize(avg_Conduct_microS_cm = mean(Conduct_microS_cm),
                  avg_pH_soil = mean(pH_soil),
                  avg_Temp_10_cm = mean(Temp_10_cm),
                  avg_Redox_pot = mean(Redox_pot)) %>%
        mutate(Month_day = format(Sampling_date, "%m-%d")) %>%
        mutate(Month = factor(Month, levels = c("October", "November", "December")))

Physchem_vars <- c("avg_Temp_10_cm", "avg_Conduct_microS_cm", "avg_pH_soil")

y_axis_labels <- c(
        avg_Temp_10_cm = "Soil Temperature (10 cm) (°C)",
        avg_Conduct_microS_cm = "Soil Conductivity (µS/cm)",
        avg_pH_soil = "Soil pH"
)

line_types <- c("2017" = "dotted", "2018" = "dotdash", "2023" = "solid")

plot_list <- list()

for (i in Physchem_vars) {
  
Meso_vs_Field_Plot <- ggplot(Meso_vs_Field, aes(x = Month_day, y = .data[[i]], linetype  = as.factor(Year), group = interaction(Year))) +
        geom_line() +
        labs(x = "Date (MM-DD)", y = y_axis_labels[i], linetype = "Year") +
        theme_bw() +
        scale_linetype_manual(values = line_types) + 
        theme(
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = if (i == "avg_Temp_10_cm") c(0.95, 0.8) else "none",
          axis.title.x = if (i == "avg_pH_soil") element_text(size = 14) else element_blank(),  # Only include x-axis title in the third plot
          axis.text.x = if (i == "avg_pH_soil") element_text(size = 14) else element_blank())  # Only include x-axis text in the third plot)

plot_list[[i]] <- Meso_vs_Field_Plot
}
  
Meso_vs_Field_Plot <- plot_list[[1]] / plot_list[[2]] / plot_list[[3]]  
print(Meso_vs_Field_Plot)
ggsave("outputs/Meso_vs_Field_Plot.pdf", Meso_vs_Field_Plot, width = 14, height = 10)
