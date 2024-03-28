
################################################### Gasera vs Chromatography - CERESTRES 2023 #########################################################

library(tidyverse)
library(ggplot2)
library(zoo)
library(writexl)
library(gridExtra)
library(cowplot)

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
                                    scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                                    theme_bw() +
                                    labs(y = expression(paste("Chromatography - ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                                    xlab("Time") +
                                    ggtitle(expression(paste(CH[4], " Emission rates Gasera vs. Chromatography"))) +
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
                                      , plot.margin = unit(c(1, 1, 0, 1.3), "lines")) +
                                    xlab(NULL) 

print(Rates_vs_time_CH4_CHROM)

#### CH4 - Gasera ####

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

#### CH4 - Plot arrange ####

Rates_vs_time_CH4_methods <- grid.arrange(arrangeGrob(Rates_vs_time_CH4_CHROM, Rates_vs_time_CH4_GASERA, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CH4_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_CH4_methods)

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

#### N2O - Gasera ####

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

#### N2O - Plot arrange ####

Rates_vs_time_N2O_methods <- grid.arrange(arrangeGrob(Rates_vs_time_N2O_CHROM, Rates_vs_time_N2O_GASERA, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/N2O_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_N2O_methods)

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

#### CO2 - Gasera Transparent ####

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

#### CO2 - Gasera Transparent ####

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

#### CO2 - Plot arrange ####

# Transparent chambers:

Rates_vs_time_CO2_methods_TR <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_CHROM, Rates_vs_time_CO2_GASERA_TR, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_TR_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_CO2_methods_TR)

# Dark chambers:

Rates_vs_time_CO2_methods_DK <- grid.arrange(arrangeGrob(Rates_vs_time_CO2_CHROM, Rates_vs_time_CO2_GASERA_DK, Water_plot_2023b, nrow = 3, ncol = 1))
ggsave("outputs/CERESTRES_results/Gasera_vs_Chromat/CO2_DK_methods.pdf", width = 20, height = 12, plot = Rates_vs_time_CO2_methods_DK)

# 3. Plotting Methods ####

## 3.1. CH4 ####

Versus_CH4 <- ggplot(data = subset(Master_GHG_2023, Season == "GS" & Treat == "CON"), aes(x = Chrom_CH4_flux_corrected, y = Gasera_CH4_flux_mgm2h, color = Treat, group = Treat)) +
                     stat_summary(fun = "mean", geom = "point", aes(x = Chrom_CH4_flux_corrected), color = "green") + 
                     stat_summary(fun = "mean", geom = "point", aes(y = Gasera_CH4_flux_mgm2h), color = "blue")

print(Versus_CH4)



pdf('outputs/CERESTRES_results/Versus_CH4.pdf', width = 12)

Versus_CH4 <- ggplot(data = subset(Master_GHG_2023, Season == "GS" & Treat == "CON"), aes(x = Chrom_CH4_flux_corrected, y = Gasera_CH4_flux_mgm2h, color = Treat, group = Treat)) +
                      # geom_line(data = Master_GHG_2023_CHROM, aes(x = Chrom_CH4_flux_corrected, y = Gasera_CH4_flux_mgm2h, group = Plot), alpha = 0.5, linetype = "dotted") +
                      geom_line(data = Avg_rates_compare_TR, aes(x = avg_Chrom_CH4_flux_mgm2h, y = avg_Gasera_CH4_flux_mgm2h)) +
                      scale_colour_manual(name = "Treatment", values = c("#002B5B", "#03C988", "#FF5D5D"), breaks=c('CON', 'MSD', 'AWD')) +
                      theme_bw() +
                      labs(y = expression(paste("Gasera - ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")")), x = expression(paste("Chromatography - ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                      # labs(x = expression(paste("Chromatography - ", CH[4], " flux (mg ", m^-2, " ", h^-1, ")"))) +
                      ggtitle(expression(paste(CH[4], " Emission rates Gasera vs. Chromatography"))) +
                      theme(
                        axis.title.y = element_text(color = "black"), legend.margin=margin(0,0,0,0),
                        axis.text.y = element_text(color = "black"),
                        axis.title.y.right = element_text(color = "black"),
                        axis.text.y.right = element_text(color = "black"),
                        strip.background = element_blank(),
                        strip.placement = "outside",
                        legend.text = element_text(size = 12),
                        legend.title = element_text(size = 12),
                        # axis.text.x = element_blank(),
                        legend.position="top",
                        plot.margin = unit(c(1, 1, 2, 1), "lines")) +
                      xlab(NULL) 

print(Versus_CH4)

dev.off()

