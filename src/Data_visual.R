
################################################### Gasera vs Chromatography - CERESTRES 2023 #########################################################

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

pdf('outputs/CERESTRES_results/Water_plot_2023b.pdf', width = 12)

Water_plot_2023b <- ggplot(Avg_water_level3, aes(x = Sampling_date, color = Treat, linetype = avg_Water_level_corr)) +
                            geom_line(aes(y = avg_Water_level_corr, linetype = "avg_Water_level_corr", color = Treat), show.legend = FALSE) +
                            scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme_bw() +
                            labs(y = "Water level (cm)") +
                            labs(x = "Sampling date") +
                            geom_hline(yintercept=0, color = "grey") +
                            geom_vline(xintercept = as.Date("2023-10-03"), linetype = "dashed", color = "grey") +
                            scale_linetype_manual(name = "avg_Water_level_corr",
                                                  values = c("avg_Water_level_corr" = "dashed"), labels = "Water level (cm)") +
                            guides(linetype = guide_legend(override.aes = list(color = "black"))) +
                            theme(axis.title.y = element_text(color = "black"), 
                                  axis.text.y = element_text(color = "black"),
                                  axis.title.y.right = element_text(color = "black"),
                                  axis.text.y.right = element_text(color = "black"), strip.background = element_blank(),
                                  strip.placement = "outside",
                                  plot.margin = unit(c(0, 1, 1, 1.35), "lines"))+
                            scale_x_date(date_breaks = "14 day", date_labels = "%m.%d")

print(Water_plot_2023b) # Water level all treats

dev.off()

Water_plot_2023b_limits <- as.Date(layer_scales(Water_plot_2023b)$x$get_limits()) # extracting limits from plot Water_plot_2023b to use in Gasera and Chrom plots, so their x axis coincide when arranging

# 2. Plotting Emissions ####

## 2.1. Preparing subsetted dataframes for plots ####

## Gasera - Tansparent chambers:

Avg_rates_compare_TR <- Master_GHG_2023 %>%  # New df with averaged emissions for all Gasera samplings with transparent chambers and all Chromatography samplings,
                          filter(Chamber_type == "TR" | is.na(Chamber_type)) %>% 
                          group_by(Sampling_date, Treat) %>% 
                          summarize(avg_Gasera_CH4_flux_mgm2h = mean(Gasera_CH4_flux_mgm2h, na.rm = TRUE), avg_Gasera_CH4_flux_mgm2h_cor = mean(Gasera_CH4_flux_mgm2h_cor, na.rm = TRUE), 
                                    avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE),
                                    avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                                    avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE),
                                    avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                                    avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE))

Avg_rates_compare_TR$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_TR$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_TR$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_TR$avg_Chrom_CO2_flux_mgm2h[is.na(Avg_rates_compare_TR$avg_Chrom_CO2_flux_mgm2h)] <- NA # Replace NaN for NA values

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
                                      avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE),
                                      avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                                      avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE),
                                      avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                                      avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE))

Avg_rates_compare_DK$avg_Chrom_CH4_flux_mgm2h[is.na(Avg_rates_compare_DK$avg_Chrom_CH4_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_DK$avg_Chrom_N2O_flux_mgm2h[is.na(Avg_rates_compare_DK$avg_Chrom_N2O_flux_mgm2h)] <- NA # Replace NaN for NA values
Avg_rates_compare_DK$avg_Chrom_CO2_flux_mgm2h[is.na(Avg_rates_compare_DK$avg_Chrom_CO2_flux_mgm2h)] <- NA # Replace NaN for NA values

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
                                    annotate('text', x = as.Date("2023-09-30"), y = 60, label = "Growing Season", size = 4, color = "grey", angle = '90') +
                                    annotate('text', x = as.Date("2023-10-06"), y = 60, label = "Post-Harvest", size = 4, color = "grey", angle = '270') +
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
                                      plot.margin = unit(c(1, 1, 0, 1.3), "lines"),
                                      plot.title = element_blank()) +
                                    xlab(NULL) 

print(Rates_vs_time_CH4_CHROM)

#### CH4 - Gasera (not corrected) ####

Rates_vs_time_CH4_GASERA <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CH4_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CH4_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h)) + #, linetype = "Average CH4 Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera - ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
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

print(Rates_vs_time_CH4_GASERA)

#### CH4 - Gasera (corrected) ####

Rates_vs_time_CH4_GASERA_cor <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CH4_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CH4_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_CH4_flux_mgm2h_cor)) + #, linetype = "Average CH4 Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera (corrected)- ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
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

print(Rates_vs_time_CH4_GASERA_cor)

#### CH4 - Plot arrange ####

# Gasera not corrected:

Rates_vs_time_CH4_methods <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Rates_vs_time_CH4_GASERA, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_CH4_methods)

# Gasera corrected:

Rates_vs_time_CH4_methods_gascor <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Rates_vs_time_CH4_GASERA_cor, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods_gascor.pdf", width = 20, height = 12, plot = Rates_vs_time_CH4_methods_gascor)

# Only Chromatography:

Rates_vs_time_CH4_CHROM_arr <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Water_plot_2023b, nrow = 2, ncol = 1))

## 2.3. N2O ####

#### N2O - Chromatography ####

Rates_vs_time_N2O_CHROM <- ggplot(data = Avg_rates_compare_TR_CHROM, aes(color = Treat, x = Sampling_date, y = avg_Chrom_N2O_flux_mgm2h, group = Treat)) +
                                  geom_line(data = Master_GHG_2023_CHROM, aes(x = Sampling_date, y = Chrom_N2O_flux_corrected, group = Plot), alpha = 0.5, linetype = "dotted") +
                                  geom_line(aes(y = avg_Chrom_N2O_flux_mgm2h)) + # linetype = "Average N2O Flux - Chromatography")) +   , na.rm = TRUE) +
                                  scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                  theme_bw() +
                                  labs(y = expression(paste("Chromatography - ", N[2], "O", " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                  xlab("Time") +
                                  ggtitle(expression(paste(N[2], "O", " Emission rates Gasera vs. Chromatography"))) +
                                  scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
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

#### N2O - Gasera (not corrected) ####

Rates_vs_time_N2O_GASERA <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_N2O_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_N2O_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_N2O_flux_mgm2h)) + #, linetype = "Average N2O Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera - ", N[2], "O", " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
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

print(Rates_vs_time_N2O_GASERA)

#### N2O - Gasera (corrected) ####

Rates_vs_time_N2O_GASERA_cor <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_N2O_flux_mgm2h, group = Treat)) +
                                    geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_N2O_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                    geom_line(aes(y = avg_Gasera_N2O_flux_mgm2h_cor)) + #, linetype = "Average N2O Flux - Gasera")) +
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Gasera (corrected) - ", N[2], "O", " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
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
                                  labs(y = expression(paste("Chromatography - ", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                  xlab("Time") +
                                  ggtitle(expression(paste(CO[2], " Emission rates Gasera vs. Chromatography"))) +
                                  scale_x_date(limits = Water_plot_2023b_limits, date_breaks = "14 day", date_labels = "%m.%d") +
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
                                    , plot.margin = unit(c(1, 1, 0, 1), "lines")) +
                                  xlab(NULL) 

print(Rates_vs_time_CO2_CHROM)

#### CO2 - Gasera Transparent (not corrected) ####

Rates_vs_time_CO2_GASERA_TR <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                      geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CO2_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                      geom_line(aes(y = avg_Gasera_CO2_flux_mgm2h)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                      scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                      theme_bw() +
                                      labs(y = expression(paste("Gasera (Transp. ch.) - ", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                      xlab("Time") +
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
                                        plot.margin = unit(c(0, 1, 0, 0.3), "lines")) +
                                      xlab(NULL) 

print(Rates_vs_time_CO2_GASERA_TR)

#### CO2 - Gasera Dark (not corrected) ####

Rates_vs_time_CO2_GASERA_DK <- ggplot(data = Avg_rates_compare_DK_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                      geom_line(data = Master_GHG_2023_GASERA_DK, aes(x = Sampling_date, y = Gasera_CO2_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                                      geom_line(aes(y = avg_Gasera_CO2_flux_mgm2h)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                      scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                      theme_bw() +
                                      labs(y = expression(paste("Gasera (Dark ch.) - ", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                      xlab("Time") +
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
                                        plot.margin = unit(c(0, 1, 0, 0.7), "lines")) +
                                      xlab(NULL) 

print(Rates_vs_time_CO2_GASERA_DK)

#### CO2 - Gasera Transparent (corrected) ####

Rates_vs_time_CO2_GASERA_TR_cor <- ggplot(data = Avg_rates_compare_TR_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                        geom_line(data = Master_GHG_2023_GASERA_TR, aes(x = Sampling_date, y = Gasera_CO2_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                        geom_line(aes(y = avg_Gasera_CO2_flux_mgm2h_cor)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                        scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                        theme_bw() +
                                        labs(y = expression(paste("Gasera (Transp. ch. & corrected) - ", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                        xlab("Time") +
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
                                          plot.margin = unit(c(0, 1, 0, 0.3), "lines")) +
                                        xlab(NULL) 

print(Rates_vs_time_CO2_GASERA_TR_cor)

#### CO2 - Gasera Dark (corrected) ####

Rates_vs_time_CO2_GASERA_DK_cor <- ggplot(data = Avg_rates_compare_DK_GASERA, aes(color = Treat, x = Sampling_date, y = avg_Chrom_CO2_flux_mgm2h, group = Treat)) +
                                        geom_line(data = Master_GHG_2023_GASERA_DK, aes(x = Sampling_date, y = Gasera_CO2_flux_mgm2h_cor, group = Plot), alpha = 0.5, linetype = "dotted") +
                                        geom_line(aes(y = avg_Gasera_CO2_flux_mgm2h_cor)) + #, linetype = "Average CO2 Flux - Gasera")) +
                                        scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                        theme_bw() +
                                        labs(y = expression(paste("Gasera (Dark ch. & corrected) - ", CO[2], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                        xlab("Time") +
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
                                          plot.margin = unit(c(0, 1, 0, 0.7), "lines")) +
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
                            avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE),
                            avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                            avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE),
                            avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                            avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE))

## 3.1. CH4 ####

Master_CON_GS_CH4 <- Master_CON_GS %>% 
                      select(Sampling_date, Treat, avg_Gasera_CH4_flux_mgm2h, avg_Gasera_CH4_flux_mgm2h_cor, avg_Chrom_CH4_flux_mgm2h)

Master_CON_GS_CH4 <- Master_CON_GS_CH4[order(Master_CON_GS_CH4$avg_Chrom_CH4_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_CH4, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_CH4.xlsx") # Excel file with Master_CON_GS_CH4

# Gasera not corrected:

Versus_CH4_r2 <- Master_CON_GS_CH4 %>% # Calculating R-squared 
                  group_by(Treat) %>%
                  summarize(R_squared = summary(lm(avg_Gasera_CH4_flux_mgm2h ~ avg_Chrom_CH4_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods_data_R2.pdf', width = 12)

Versus_CH4 <- ggplot(data = Master_CON_GS_CH4, aes(x = avg_Chrom_CH4_flux_mgm2h, y = avg_Gasera_CH4_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, ", CH[4]))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CH4_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CH4)

dev.off()

# Gasera corrected:

Versus_CH4_r2_cor <- Master_CON_GS_CH4 %>% # Calculating R-squared 
                      group_by(Treat) %>%
                      summarize(R_squared = summary(lm(avg_Gasera_CH4_flux_mgm2h_cor ~ avg_Chrom_CH4_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods_data_R2_gascor.pdf', width = 12)

Versus_CH4_cor <- ggplot(data = Master_CON_GS_CH4, aes(x = avg_Chrom_CH4_flux_mgm2h, y = avg_Gasera_CH4_flux_mgm2h_cor, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, ", CH[4]))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CH4_r2_cor, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CH4_cor)

dev.off()

## 3.2. N2O ####

Master_CON_GS_N2O <- Master_CON_GS %>% 
                      select(Sampling_date, Treat, avg_Gasera_N2O_flux_mgm2h, avg_Gasera_N2O_flux_mgm2h_cor, avg_Chrom_N2O_flux_mgm2h)

Master_CON_GS_N2O <- Master_CON_GS_N2O[order(Master_CON_GS_N2O$avg_Chrom_N2O_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_N2O, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_N2O.xlsx") # Excel file with Master_CON_GS_N2O

# Gasera not corrected:

Versus_N2O_r2 <- Master_CON_GS_N2O %>% # Calculating R-squared 
                  group_by(Treat) %>%
                  summarize(R_squared = summary(lm(avg_Gasera_N2O_flux_mgm2h ~ avg_Chrom_N2O_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/N2O_methods_data_R2.pdf', width = 12)

Versus_N2O <- ggplot(data = Master_CON_GS_N2O, aes(x = avg_Chrom_N2O_flux_mgm2h, y = avg_Gasera_N2O_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, ", N[2], "O"))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_N2O_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_N2O)

dev.off()

# Gasera corrected:

Versus_N2O_r2_cor <- Master_CON_GS_N2O %>% # Calculating R-squared 
                      group_by(Treat) %>%
                      summarize(R_squared = summary(lm(avg_Gasera_N2O_flux_mgm2h_cor ~ avg_Chrom_N2O_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/N2O_methods_data_R2_gascor.pdf', width = 12)

Versus_N2O_cor <- ggplot(data = Master_CON_GS_N2O, aes(x = avg_Chrom_N2O_flux_mgm2h, y = avg_Gasera_N2O_flux_mgm2h_cor, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, ", N[2], "O"))) +
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
                              avg_Chrom_CH4_flux_mgm2h = mean(Chrom_CH4_flux_corrected, na.rm = TRUE),
                              avg_Gasera_N2O_flux_mgm2h = mean(Gasera_N2O_flux_mgm2h, na.rm = TRUE), avg_Gasera_N2O_flux_mgm2h_cor = mean(Gasera_N2O_flux_mgm2h_cor, na.rm = TRUE),
                              avg_Chrom_N2O_flux_mgm2h = mean(Chrom_N2O_flux_corrected, na.rm = TRUE),
                              avg_Gasera_CO2_flux_mgm2h = mean(Gasera_CO2_flux_mgm2h, na.rm = TRUE), avg_Gasera_CO2_flux_mgm2h_cor = mean(Gasera_CO2_flux_mgm2h_cor, na.rm = TRUE),
                              avg_Chrom_CO2_flux_mgm2h = mean(Chrom_CO2_flux_corrected, na.rm = TRUE))

# Transparent chambers:

Master_CON_GS_CO2 <- Master_CON_GS %>% 
                      select(Sampling_date, Treat, avg_Gasera_CO2_flux_mgm2h, avg_Gasera_CO2_flux_mgm2h_cor, avg_Chrom_CO2_flux_mgm2h)

Master_CON_GS_CO2 <- Master_CON_GS_CO2[order(Master_CON_GS_CO2$avg_Chrom_CO2_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_CO2, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_CO2.xlsx") # Excel file with Master_CON_GS_CO2

# Dark chambers:

Master_CON_GS_CO2_DK <- Master_CON_GS_DK %>% 
                        select(Sampling_date, Treat, avg_Gasera_CO2_flux_mgm2h, avg_Gasera_CO2_flux_mgm2h_cor, avg_Chrom_CO2_flux_mgm2h)

Master_CON_GS_CO2_DK <- Master_CON_GS_CO2_DK[order(Master_CON_GS_CO2_DK$avg_Chrom_CO2_flux_mgm2h), ]                    

write_xlsx(Master_CON_GS_CO2_DK, "outputs/CERESTRES_results/Gasera_vs_Chromat/Master_CON_GS_CO2_DK.xlsx") # Excel file with Master_CON_GS_CO2_DK

# Gasera Transparent not corrected:

Versus_CO2_r2 <- Master_CON_GS_CO2 %>% # Calculating R-squared 
                  group_by(Treat) %>%
                  summarize(R_squared = summary(lm(avg_Gasera_CO2_flux_mgm2h ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_methods_data_R2.pdf', width = 12)

Versus_CO2 <- ggplot(data = Master_CON_GS_CO2, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CO2_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, ", N[2], "O"))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CO2_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CO2)

dev.off()

# Gasera Dark not corrected:

Versus_CO2_DK_r2 <- Master_CON_GS_CO2_DK %>% # Calculating R-squared 
                    group_by(Treat) %>%
                    summarize(R_squared = summary(lm(avg_Gasera_CO2_flux_mgm2h ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_DK_methods_data_R2.pdf', width = 12)

Versus_DK_CO2 <- ggplot(data = Master_CON_GS_CO2_DK, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CO2_flux_mgm2h, group = Treat)) +
                      geom_point() +
                      ggtitle(expression(paste("Gasera (TR Chamb.) vs. Chrom. rates - GS, CON, ", N[2], "O"))) +
                      stat_poly_line() +
                      theme(plot.title = element_text(size = 10, face = "bold")) +
                      geom_text(data = Versus_CO2_DK_r2, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_DK_CO2)

dev.off()

# Gasera Transparent corrected:

Versus_CO2_r2_cor <- Master_CON_GS_CO2 %>% # Calculating R-squared 
                      group_by(Treat) %>%
                      summarize(R_squared = summary(lm(avg_Gasera_CO2_flux_mgm2h_cor ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_methods_data_R2_gascor.pdf', width = 12)

Versus_CO2_cor <- ggplot(data = Master_CON_GS_CO2, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CO2_flux_mgm2h_cor, group = Treat)) +
                          geom_point() +
                          ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, ", N[2], "O"))) +
                          stat_poly_line() +
                          theme(plot.title = element_text(size = 10, face = "bold")) +
                          geom_text(data = Versus_CO2_r2_cor, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_CO2_cor)

dev.off()

# Gasera Dark corrected:

Versus_CO2_DK_r2_cor <- Master_CON_GS_CO2_DK %>% # Calculating R-squared 
                        group_by(Treat) %>%
                        summarize(R_squared = summary(lm(avg_Gasera_CO2_flux_mgm2h_cor ~ avg_Chrom_CO2_flux_mgm2h))$r.squared)

pdf('outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_DK_methods_data_R2_gascor.pdf', width = 12)

Versus_DK_CO2_cor <- ggplot(data = Master_CON_GS_CO2_DK, aes(x = avg_Chrom_CO2_flux_mgm2h, y = avg_Gasera_CO2_flux_mgm2h_cor, group = Treat)) +
                            geom_point() +
                            ggtitle(expression(paste("Gasera (TR Chamb. & corrected) vs. Chrom. rates - GS, CON, ", N[2], "O"))) +
                            stat_poly_line() +
                            theme(plot.title = element_text(size = 10, face = "bold")) +
                            geom_text(data = Versus_CO2_DK_r2_cor, aes(label = paste("R^2 =", round(R_squared, 4)), x = Inf, y = -Inf), hjust = 1, vjust = -1)

print(Versus_DK_CO2_cor)

dev.off()

# 4. Cumulative emissions ####

## 4.1. Cumulative CH4 ####

# Total (GS & PH):

Acc_CH4_tot_plot <-  ggplot(Acc_CHROM_tot_sum, aes(x = Treat, y = CH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Complete Season") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              plot.title = element_text(hjust = 0.5))

print(Acc_CH4_tot_plot)

Acc_CH4_tot_plot_limits <- layer_scales(Acc_CH4_tot_plot)$y$get_limits() # extracting limits from plot Acc_CH4_tot_plot to use in Acc_CH4_GS_plot, so they coincide when arranging

# GS:

Acc_CH4_GS_plot <-  ggplot(Acc_CHROM_GS_sum, aes(x = Treat, y = CH4_kgha_tot, fill = Treat, color = Treat)) + 
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
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5))

print(Acc_CH4_GS_plot)

# PH:

Acc_CH4_PH_plot <-  ggplot(Acc_CHROM_PH_sum, aes(x = Treat, y = CH4_kgha_tot, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(x = "Irrigation Strategies", y = expression(paste("Cumulative ",C-CH[4], " emissions (kg ", ha^-1, ")"))) +
                            theme_bw()+
                            ggtitle("Post-Harvest") +
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            scale_y_continuous(position = "right", limits = c(0, 320), breaks = seq(0, 320, by = 50)) +
                            theme(
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(hjust = 0.5))

print(Acc_CH4_PH_plot)

# Arrange plots: 

# Cumulative emission plots:

CH4_acc_arr <- grid.arrange(arrangeGrob(Acc_CH4_GS_plot, Acc_CH4_PH_plot, Acc_CH4_tot_plot, nrow = 1, ncol = 3, widths = c(0.3, 0.3, 0.4) ))
CH4_acc_rate_arr <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM_arr, CH4_acc_arr, nrow = 1, ncol = 2, widths = c(0.6, 0.4)))

ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_flux_water_acc.pdf", width = 20, height = 10, plot = CH4_acc_rate_arr) 

# 5. GWP ####

## 5.1. GWP Boxplots ####

GWP_boxplot <-  ggplot(Acc_CHROM_tot_sum, aes(x = Treat, y = GWP, fill = Treat, color = Treat)) + 
                            geom_boxplot(width = 0.4, size = 0.2, show.legend = FALSE) + 
                            labs(y = expression(paste("GWP (kg ",CO[2], " eq ", ha^-1, " ", season^-1, ")"))) +
                            theme_bw()+
                            scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#FF5D5D"), guide = "none") +
                            scale_colour_manual(name = "Treatment", values = c("#820300", "#820300", "#820300"), breaks=c('CON', 'MSD', 'AWD')) +
                            theme(plot.margin = margin(l = 0, r = 5, t = 25, b = 14, unit = "pt")) + # Adjust margins to correct arrange below.
                            theme(
                              axis.title.x = element_blank(),
                              plot.title = element_text(hjust = 0.5))

print(GWP_boxplot)

ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/GWP_boxplot.pdf", plot = GWP_boxplot ,width = 10, height = 10)

## 5.2. GWP dots ####

GWP_dots <- ggplot(Acc_CHROM_tot_sum, aes(Treat, GWP, group = Treat, colour = Treat, fill = Treat)) +
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

print(GWP_dots)

ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/GWP_dots.pdf", plot = GWP_dots ,width = 10, height = 10)

