
################################################## Micro Visual ###################################################

library(gridExtra)    # for arranging plots
library(ggplot2)
library(microeco)     # for FAPROTAX analisis
library("rcompanion") # for FAPROTAX analisis 
library(patchwork)    # for arranging plots
library(ggsignif)     # for geom_signif()
library(ggtext)       # for element_markdown()

# Microbiology data visualization: Uses elements from CERESTRES_BACTERIS_microeco_STAGES script as inputs.

# 1. Faprotax ####

# For each test only functions that result significative in any of the period-depth combinations are selected:
# - ANOVA: c("methanotrophy", "methanogenesis by CO2 reduction with H2", "hydrogenotrophic methanogenesis", "methanogenesis", "methylotrophy", "sulfate respiration", "respiration of sulfur compounds", "dark hydrogen oxidation", "nitrogen fixation")
#   which in Faprotax order are in order: c(1, 2, 4, 5, 7, 10, 13, 19, 20)

# 1.1. Per Stages (T and B separated) ####

## a. IN-TOP ####
#Anova:
FapPlot_InTOP_an <- f2$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab("Relative abundance (%) \nTest: ANOVA") +
                      ylab(NULL) +
                      scale_fill_manual(
                        values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                        labels = c("AWD", "MSD", "CON")) +
                      annotate('text', x = 9, y = 2, label = "Initial \nTop soil", size = 7, color = "black") + 
                      scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                                                    "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                                                  "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                                                  "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                                                    "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                      theme(
                        plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.title = element_blank(),
                        axis.text.y = element_text(size = 16),
                        axis.text.x = element_text(size = 16),
                        legend.position=c(0.8, 0.8),
                        legend.text = element_text(size = 20),
                        legend.key.size = unit(30, "pt"), 
                        plot.title =element_text("IN_TOP"),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                        )
print(FapPlot_InTOP_an)

#KW-dunn:
FapPlot_InTOP_kw <- f2.1$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                        xlab("Relative abundance (%) \nTest: ANOVA") +
                        ylab(NULL) +
                        scale_fill_manual(
                          values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                          labels = c("AWD", "MSD", "CON")) +
                        annotate('text', x = 9, y = 2, label = "Initial \nTop soil", size = 7, color = "black") + 
                        scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                                                      "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                                                    "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                                                    "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" =
                                                      "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                        theme(
                          plot.margin = margin(l = 50, r = 0, t = 0, b = 0, unit = "pt"),
                          legend.title = element_blank(),
                          axis.text.y = element_text(size = 16),
                          axis.text.x = element_text(size = 16),
                          legend.position=c(0.8, 0.8),
                          legend.text = element_text(size = 20),
                          legend.key.size = unit(30, "pt"), 
                          plot.title =element_text("IN_TOP"),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(0, "pt")
                        )
print(FapPlot_InTOP_kw)

## b. IN-BOT ####
#Anova:
FapPlot_InBOT_an <- f2_INI_B$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 2, label = "Initial \nBottom soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_text("IN_BOT"),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                        )
print(FapPlot_InBOT_an)

#KW-dunn:
FapPlot_InBOT_kw <- f2.1_INI_B$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 2, label = "Initial \nBottom soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_text("IN_BOT"),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(FapPlot_InBOT_kw)

## c. GS-TOP ####
#Anova:
FapPlot_GSTOP_an <- f2_GRO_T$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 4, label = "Growing season \nTop soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_blank(),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                        )
print(FapPlot_GSTOP_an)

# KW-dunn:
FapPlot_GSTOP_kw <- f2.1_GRO_T$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 4, label = "Growing season \nTop soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_blank(),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(FapPlot_GSTOP_kw)

## d. GS-BOT ####
#Anova:
FapPlot_GSBOT_an <- f2_GRO_B$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 3, label = "Growing season \nBottom soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_blank(),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                        )
print(FapPlot_GSBOT_an)

# KW-dunn:
FapPlot_GSBOT_kw <- f2.1_GRO_B$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                        xlab(NULL) +
                        ylab(NULL) +
                        annotate('text', x = 9, y = 3, label = "Growing season \nBottom soil", size = 7, color = "black") +
                        theme(
                          plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt"),
                          legend.position="none",
                          axis.text.x = element_text(size = 16),
                          axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                          plot.title =element_blank(),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(0, "pt")
                        )
print(FapPlot_GSBOT_kw)

## e. FS-TOP ####
# Anova:
FapPlot_FSTOP_an <- f2_POST_T$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 2, label = "Fallow season \nTop soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        # axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_blank(),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(FapPlot_FSTOP_an)

# KW-dunn:
FapPlot_FSTOP_kw <- f2.1_POST_T$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 2, label = "Fallow season \nTop soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        # axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_blank(),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(FapPlot_FSTOP_kw)

## f. FS-BOT ####
# Anova:
FapPlot_FSBOT_an <- f2_POST_B$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 2, label = "Fallow season \nBottom soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_blank(),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(FapPlot_FSBOT_an)

# KW-dunn:
FapPlot_FSBOT_kw <- f2.1_POST_B$plot_diff_abund(use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20), add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab(NULL) +
                      ylab(NULL) +
                      annotate('text', x = 9, y = 2, label = "Fallow season \nBottom soil", size = 7, color = "black") +
                      theme(
                        plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt"),
                        legend.position="none",
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
                        plot.title =element_blank(),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(FapPlot_FSBOT_kw)

# Arrange test:
All_an_arr <- grid.arrange(arrangeGrob(FapPlot_InTOP_an, FapPlot_InBOT_an, FapPlot_GSTOP_an, FapPlot_GSBOT_an, FapPlot_FSTOP_an, FapPlot_FSBOT_an, nrow = 1, ncol = 6, widths = c(0.25, 0.15, 0.15, 0.15, 0.15, 0.15))) 
ggsave("outputs/Micro/Micro_visual/All_an_arr.pdf", plot = All_an_arr ,width = 30, height = 10)  

All_kw_arr <- grid.arrange(arrangeGrob(FapPlot_InTOP_kw, FapPlot_InBOT_kw, FapPlot_GSTOP_kw, FapPlot_GSBOT_kw, FapPlot_FSTOP_kw, FapPlot_FSBOT_kw, nrow = 1, ncol = 6, widths = c(0.25, 0.15, 0.15, 0.15, 0.15, 0.15))) 
ggsave("outputs/Micro/Micro_visual/All_kw_arr.pdf", plot = All_an_arr ,width = 30, height = 10)  

All_an_kw_arr <- grid.arrange(arrangeGrob(All_an_arr, All_kw_arr, nrow = 2, ncol = 1))
ggsave("outputs/Micro/Micro_visual/All_an_kw_arr.pdf", plot = All_an_kw_arr ,width = 30, height = 20)

## Check function order in rel_ab plots - 1.2. IN-BOT ####
#Anova:
FapPlot_InBOT_an <- f2_INI_B$plot_diff_abund( add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
  xlab(NULL) +
  ylab(NULL) +
  annotate('text', x = 9, y = 2, label = "Initial \nBottom soil", size = 7, color = "black") +
  theme(
    plot.margin = margin(l = 0, r = 0, t = 10, b = 0, unit = "pt"),
    legend.position="none",
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(), # use for all plots but IN-TOP, as they carry the function names.
    plot.title =element_text("IN_BOT"),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt")
  )
print(FapPlot_InBOT_an)

# 1.2. Per Stages (T and B together) ####

dataset_initial <- clone(dataset_INI)
dataset_growing <- clone(dataset_GRO)
dataset_fallow <- clone(dataset_POST)

dataset_initial$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))
dataset_growing$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))
dataset_fallow$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))

dataset_initial$cal_abund()
dataset_growing$cal_abund()
dataset_fallow$cal_abund()

finitial <- trans_func$new(dataset_initial)
fgrowing <- trans_func$new(dataset_growing)
ffallow <- trans_func$new(dataset_fallow)

finitial$cal_spe_func(prok_database = "FAPROTAX") 
fgrowing$cal_spe_func(prok_database = "FAPROTAX")
ffallow$cal_spe_func(prok_database = "FAPROTAX") 

finitial$cal_spe_func_perc(abundance_weighted = TRUE) 
fgrowing$cal_spe_func_perc(abundance_weighted = TRUE)
ffallow$cal_spe_func_perc(abundance_weighted = TRUE) 

tmp_in <- list() 
tmp_gs <- list()
tmp_fs <- list()

# transpose res_spe_func_perc to be a data.frame like taxonomic abundance
tmp_in$func <- as.data.frame(t(finitial$res_spe_func_perc), check.names = FALSE)
tmp_gs$func <- as.data.frame(t(finitial$res_spe_func_perc), check.names = FALSE)
tmp_fs$func <- as.data.frame(t(finitial$res_spe_func_perc), check.names = FALSE)

# assign the list as taxa_abund in your microtable object
dataset_initial$taxa_abund <- tmp_in
dataset_growing$taxa_abund <- tmp_gs
dataset_fallow$taxa_abund <- tmp_fs

# use trans_diff class to perform differential test
finitial2 <- trans_diff$new(dataset = dataset_initial, method = "anova", group = "tractament", taxa_level = "all")
fgrowing2 <- trans_diff$new(dataset = dataset_growing, method = "anova", group = "tractament", taxa_level = "all") #
ffallow2 <- trans_diff$new(dataset = dataset_fallow, method = "anova", group = "tractament", taxa_level = "all") #

ginitialf1 <- finitial2$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                  add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                  xlab("Relative abundance (%) \nTest: ANOVA") +
                  ylab(NULL) +
                  scale_fill_manual(
                    values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                    labels = c("AWD", "MSD", "CON")) +
                  annotate('text', x = 18, y = 2, label = "Initial Stage", size = 7, color = "black") + 
                  # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                  #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                  #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                  #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                  #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                  theme(
                    # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                    legend.title = element_blank(),
                    axis.text.y = element_text(size = 16),
                    axis.text.x = element_text(size = 16),
                    legend.position=c(0.8, 0.8),
                    legend.text = element_text(size = 20),
                    legend.key.size = unit(30, "pt"), 
                    plot.title = element_text("t1"),
                    axis.ticks = element_blank(),
                    axis.ticks.length = unit(0, "pt")
                  )
print(ginitialf1)

ggsave("outputs/Micro/Micro_visual/FAP_an_IN.jpg", plot = ginitialf1, width = 8, height = 10, units = "in")

ggrowingf1 <- fgrowing2$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                  add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                  xlab("Relative abundance (%) \nTest: ANOVA") +
                  ylab(NULL) +
                  scale_fill_manual(
                    values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                    labels = c("AWD", "MSD", "CON")) +
                  annotate('text', x = 18, y = 2, label = "Growing Season", size = 7, color = "black") + 
                  # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                  #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                  #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                  #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                  #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                  theme(
                    # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                    legend.title = element_blank(),
                    axis.text.y = element_text(size = 16),
                    axis.text.x = element_text(size = 16),
                    legend.position=c(0.8, 0.8),
                    legend.text = element_text(size = 20),
                    legend.key.size = unit(30, "pt"), 
                    plot.title = element_text("t1"),
                    axis.ticks = element_blank(),
                    axis.ticks.length = unit(0, "pt")
                  )
print(ggrowingf1)

ggsave("outputs/Micro/Micro_visual/FAP_an_GS.jpg", plot = ggrowingf1, width = 8, height = 10, units = "in")

gfallowf1 <- ffallow2$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                xlab("Relative abundance (%) \nTest: ANOVA") +
                ylab(NULL) +
                scale_fill_manual(
                  values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                  labels = c("AWD", "MSD", "CON")) +
                annotate('text', x = 18, y = 2, label = "Fallow Season", size = 7, color = "black") + 
                # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                theme(
                  # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                  legend.title = element_blank(),
                  axis.text.y = element_text(size = 16),
                  axis.text.x = element_text(size = 16),
                  legend.position=c(0.8, 0.8),
                  legend.text = element_text(size = 20),
                  legend.key.size = unit(30, "pt"), 
                  plot.title = element_text("t1"),
                  axis.ticks = element_blank(),
                  axis.ticks.length = unit(0, "pt")
                )
print(gfallowf1)

ggsave("outputs/Micro/Micro_visual/FAP_an_FS.jpg", plot = gfallowf1, width = 8, height = 10, units = "in")

finitial2.1 <- trans_diff$new(dataset = dataset_initial, method = "KW_dunn", group = "tractament", taxa_level = "all")
fgrowing2.1 <- trans_diff$new(dataset = dataset_initial, method = "KW_dunn", group = "tractament", taxa_level = "all")
ffallow2.1 <- trans_diff$new(dataset = dataset_initial, method = "KW_dunn", group = "tractament", taxa_level = "all")

ginitialf1.1 <- finitial2.1$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                  add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                  xlab("Relative abundance (%) \nTest: KW") +
                  ylab(NULL) +
                  scale_fill_manual(
                    values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                    labels = c("AWD", "MSD", "CON")) +
                  annotate('text', x = 18, y = 2, label = "Initial Stage", size = 7, color = "black") + 
                  # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                  #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                  #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                  #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                  #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                  theme(
                    # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                    legend.title = element_blank(),
                    axis.text.y = element_text(size = 16),
                    axis.text.x = element_text(size = 16),
                    legend.position=c(0.8, 0.8),
                    legend.text = element_text(size = 20),
                    legend.key.size = unit(30, "pt"), 
                    plot.title = element_text("t1"),
                    axis.ticks = element_blank(),
                    axis.ticks.length = unit(0, "pt")
                  )
print(ginitialf1.1)

ggsave("outputs/Micro/Micro_visual/FAP_kw_IN.jpg", plot = ginitialf1.1, width = 8, height = 10, units = "in")

ggrowingf1.1 <- fgrowing2.1$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                  add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                  xlab("Relative abundance (%) \nTest: KW") +
                  ylab(NULL) +
                  scale_fill_manual(
                    values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                    labels = c("AWD", "MSD", "CON")) +
                  annotate('text', x = 18, y = 2, label = "Growing Season", size = 7, color = "black") + 
                  # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                  #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                  #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                  #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                  #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                  theme(
                    # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                    legend.title = element_blank(),
                    axis.text.y = element_text(size = 16),
                    axis.text.x = element_text(size = 16),
                    legend.position=c(0.8, 0.8),
                    legend.text = element_text(size = 20),
                    legend.key.size = unit(30, "pt"), 
                    plot.title = element_text("t1"),
                    axis.ticks = element_blank(),
                    axis.ticks.length = unit(0, "pt")
                  )
print(ggrowingf1.1)

ggsave("outputs/Micro/Micro_visual/FAP_kw_GS.jpg", plot = ggrowingf1.1, width = 8, height = 10, units = "in")

gfallowf1.1 <- ffallow2.1$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                  add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                  xlab("Relative abundance (%) \nTest: KW") +
                  ylab(NULL) +
                  scale_fill_manual(
                    values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                    labels = c("AWD", "MSD", "CON")) +
                  annotate('text', x = 18, y = 2, label = "Fallow Season", size = 7, color = "black") + 
                  # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                  #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                  #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                  #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                  #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                  theme(
                    # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                    legend.title = element_blank(),
                    axis.text.y = element_text(size = 16),
                    axis.text.x = element_text(size = 16),
                    legend.position=c(0.8, 0.8),
                    legend.text = element_text(size = 20),
                    legend.key.size = unit(30, "pt"), 
                    plot.title = element_text("t1"),
                    axis.ticks = element_blank(),
                    axis.ticks.length = unit(0, "pt")
                  )
print(gfallowf1.1)

ggsave("outputs/Micro/Micro_visual/FAP_kw_GF.jpg", plot = gfallowf1.1, width = 8, height = 10, units = "in")

# 1.3. Per Sampling Date (t1 to t6) ####

## a.i. Elements and data frames #### 

# Setting up new data frames:
dataset_sampl <-  clone(dataset)
dataset_sampl$sample_table$data %<>% factor(., levels = c("t1", "t2", "t3", "t4", "t5", "t6")) # order X axis

dataset_t1 <- clone(dataset_sampl)
dataset_t2 <- clone(dataset_sampl)
dataset_t3 <- clone(dataset_sampl)
dataset_t4 <- clone(dataset_sampl)
dataset_t5 <- clone(dataset_sampl)
dataset_t6 <- clone(dataset_sampl)

# Selecting the groups:
dataset_t1$sample_table <- subset(dataset_t1$sample_table, data == "t1") 
dataset_t2$sample_table <- subset(dataset_t2$sample_table, data == "t2") 
dataset_t3$sample_table <- subset(dataset_t3$sample_table, data == "t3") 
dataset_t4$sample_table <- subset(dataset_t4$sample_table, data == "t4") 
dataset_t5$sample_table <- subset(dataset_t5$sample_table, data == "t5") 
dataset_t6$sample_table <- subset(dataset_t6$sample_table, data == "t6") 

# Defining treatments as factors
dataset_t1$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))
dataset_t2$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))
dataset_t3$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))
dataset_t4$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))
dataset_t5$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))
dataset_t6$sample_table$tractament %<>% factor(., levels = c("CONV", "MSD", "AWD"))

dataset_t1$cal_abund()
dataset_t2$cal_abund()
dataset_t3$cal_abund()
dataset_t4$cal_abund()
dataset_t5$cal_abund()
dataset_t6$cal_abund()

library(microeco)
library("rcompanion")

func1 <- trans_func$new(dataset_t1)
func2 <- trans_func$new(dataset_t2)
func3 <- trans_func$new(dataset_t3)
func4 <- trans_func$new(dataset_t4)
func5 <- trans_func$new(dataset_t5)
func6 <- trans_func$new(dataset_t6)

func1$cal_spe_func(prok_database = "FAPROTAX")
func2$cal_spe_func(prok_database = "FAPROTAX") 
func3$cal_spe_func(prok_database = "FAPROTAX") 
func4$cal_spe_func(prok_database = "FAPROTAX") 
func5$cal_spe_func(prok_database = "FAPROTAX") 
func6$cal_spe_func(prok_database = "FAPROTAX") 

func1$cal_spe_func_perc(abundance_weighted = TRUE) 
func2$cal_spe_func_perc(abundance_weighted = TRUE)
func3$cal_spe_func_perc(abundance_weighted = TRUE)
func4$cal_spe_func_perc(abundance_weighted = TRUE)
func5$cal_spe_func_perc(abundance_weighted = TRUE)
func6$cal_spe_func_perc(abundance_weighted = TRUE)

# use list to prepare data
tmp1 <- list()
tmp2 <- list()
tmp3 <- list()
tmp4 <- list()
tmp5 <- list()
tmp6 <- list()

# transpose res_spe_func_perc to be a data.frame like taxonomic abundance
tmp1$func <- as.data.frame(t(func1$res_spe_func_perc), check.names = FALSE)
tmp2$func <- as.data.frame(t(func2$res_spe_func_perc), check.names = FALSE)
tmp3$func <- as.data.frame(t(func3$res_spe_func_perc), check.names = FALSE)
tmp4$func <- as.data.frame(t(func4$res_spe_func_perc), check.names = FALSE)
tmp5$func <- as.data.frame(t(func5$res_spe_func_perc), check.names = FALSE)
tmp6$func <- as.data.frame(t(func6$res_spe_func_perc), check.names = FALSE)

# assign the list as taxa_abund in your microtable object
dataset_t1$taxa_abund <- tmp1
dataset_t2$taxa_abund <- tmp2
dataset_t3$taxa_abund <- tmp3
dataset_t4$taxa_abund <- tmp4
dataset_t5$taxa_abund <- tmp5
dataset_t6$taxa_abund <- tmp6

# use trans_diff class to perform differential test
func1_b <- trans_diff$new(dataset = dataset_t1, method = "anova", group = "tractament", taxa_level = "all")
func2_b <- trans_diff$new(dataset = dataset_t2, method = "anova", group = "tractament", taxa_level = "all")
func3_b <- trans_diff$new(dataset = dataset_t3, method = "anova", group = "tractament", taxa_level = "all")
func4_b <- trans_diff$new(dataset = dataset_t4, method = "anova", group = "tractament", taxa_level = "all")
func5_b <- trans_diff$new(dataset = dataset_t5, method = "anova", group = "tractament", taxa_level = "all")
func6_b <- trans_diff$new(dataset = dataset_t6, method = "anova", group = "tractament", taxa_level = "all")

## a.ii. Anova plots #### 

fap_t1 <-func1_b$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
            add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
            xlab("Relative abundance (%) \nTest: ANOVA") +
            ylab(NULL) +
            scale_fill_manual(
              values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
              labels = c("AWD", "MSD", "CON")) +
            annotate('text', x = 18, y = 2, label = "Sampling 1 \n(Initial stage)", size = 7, color = "black") + 
            # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
            #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
            #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
            #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
            #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
            theme(
              # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
              legend.title = element_blank(),
              axis.text.y = element_text(size = 16),
              axis.text.x = element_text(size = 16),
              legend.position=c(0.8, 0.8),
              legend.text = element_text(size = 20),
              legend.key.size = unit(30, "pt"), 
              plot.title = element_text("t1"),
              axis.ticks = element_blank(),
              axis.ticks.length = unit(0, "pt")
            )
print(fap_t1)

ggsave("outputs/Micro/Micro_visual/FAP_an_samp1.pdf", plot = fap_t1, width = 20, height = 10, units = "in")

fap_t2 <- func2_b$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
              add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
              xlab("Relative abundance (%) \nTest: ANOVA") +
              ylab(NULL) +
              scale_fill_manual(
                values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                labels = c("AWD", "MSD", "CON")) +
              annotate('text', x = 18, y = 2, label = "Sampling 2 \n(Growing season)", size = 7, color = "black") + 
              # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
              #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
              #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
              #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
              #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
              theme(
                # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                legend.title = element_blank(),
                axis.text.y = element_text(size = 16),
                axis.text.x = element_text(size = 16),
                legend.position=c(0.8, 0.8),,
                legend.text = element_text(size = 20),
                legend.key.size = unit(30, "pt"), 
                plot.title = element_text("t1"),
                axis.ticks = element_blank(),
                axis.ticks.length = unit(0, "pt")
              )
print(fap_t2)

ggsave("outputs/Micro/Micro_visual/FAP_an_samp2.pdf", plot = fap_t2, width = 20, height = 10, units = "in")

fap_t3 <- func3_b$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                xlab("Relative abundance (%) \nTest: ANOVA") +
                ylab(NULL) +
                scale_fill_manual(
                  values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                  labels = c("AWD", "MSD", "CON")) +
                annotate('text', x = 18, y = 3, label = "Sampling 3 \n(Growing season)", size = 7, color = "black") + 
                # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                theme(
                  # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                  legend.title = element_blank(),
                  axis.text.y = element_text(size = 16),
                  axis.text.x = element_text(size = 16),
                  legend.position=c(0.8, 0.8),,
                  legend.text = element_text(size = 20),
                  legend.key.size = unit(30, "pt"), 
                  plot.title = element_text("t3"),
                  axis.ticks = element_blank(),
                  axis.ticks.length = unit(0, "pt")
                )
print(fap_t3)

ggsave("outputs/Micro/Micro_visual/FAP_an_samp3.pdf", plot = fap_t3, width = 20, height = 10, units = "in")

fap_t4 <- func4_b$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                  add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                  xlab("Relative abundance (%) \nTest: ANOVA") +
                  ylab(NULL) +
                  scale_fill_manual(
                    values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                    labels = c("AWD", "MSD", "CON")) +
                  annotate('text', x = 18, y = 4, label = "Sampling 4 \n(Growing season)", size = 7, color = "black") + 
                  # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                  #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                  #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                  #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                  #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                  theme(
                    # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                    legend.title = element_blank(),
                    axis.text.y = element_text(size = 16),
                    axis.text.x = element_text(size = 16),
                    legend.position=c(0.8, 0.8),,
                    legend.text = element_text(size = 20),
                    legend.key.size = unit(30, "pt"), 
                    plot.title = element_text("t4"),
                    axis.ticks = element_blank(),
                    axis.ticks.length = unit(0, "pt")
                  )
print(fap_t4)

ggsave("outputs/Micro/Micro_visual/FAP_an_samp4.pdf", plot = fap_t4, width = 20, height = 10, units = "in")

fap_t5 <- func5_b$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                    add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                    xlab("Relative abundance (%) \nTest: ANOVA") +
                    ylab(NULL) +
                    scale_fill_manual(
                      values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                      labels = c("AWD", "MSD", "CON")) +
                    annotate('text', x = 13, y = 3.5, label = "Sampling 5 \n(Fallow season)", size = 7, color = "black") + 
                    # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                    #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                    #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                    #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                    #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                    theme(
                      # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                      legend.title = element_blank(),
                      axis.text.y = element_text(size = 16),
                      axis.text.x = element_text(size = 16),
                      legend.position=c(0.85, 0.6),,
                      legend.text = element_text(size = 20),
                      legend.key.size = unit(30, "pt"), 
                      plot.title = element_text("t5"),
                      axis.ticks = element_blank(),
                      axis.ticks.length = unit(0, "pt")
                    )
print(fap_t5)

ggsave("outputs/Micro/Micro_visual/FAP_an_samp5.pdf", plot = fap_t5, width = 20, height = 10, units = "in")

fap_t6 <- func6_b$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                      add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab("Relative abundance (%) \nTest: ANOVA") +
                      ylab(NULL) +
                      scale_fill_manual(
                        values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                        labels = c("AWD", "MSD", "CON")) +
                      annotate('text', x = 18, y = 3, label = "Sampling 6 \n(Fallow season)", size = 7, color = "black") + 
                      # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                      #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                      #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                      #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                      #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                      theme(
                        # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.title = element_blank(),
                        axis.text.y = element_text(size = 16),
                        axis.text.x = element_text(size = 16),
                        legend.position=c(0.8, 0.8),,
                        legend.text = element_text(size = 20),
                        legend.key.size = unit(30, "pt"), 
                        plot.title = element_text("t6"),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(fap_t6)

ggsave("outputs/Micro/Micro_visual/FAP_an_samp6.pdf", plot = fap_t6, width = 20, height = 10, units = "in")

## a.iii. KW Plots #### 

func1_b2 <- trans_diff$new(dataset = dataset_t1, method = "KW_dunn", group = "tractament", taxa_level = "all")
func2_b2 <- trans_diff$new(dataset = dataset_t2, method = "KW_dunn", group = "tractament", taxa_level = "all")
func3_b2 <- trans_diff$new(dataset = dataset_t3, method = "KW_dunn", group = "tractament", taxa_level = "all")
func4_b2 <- trans_diff$new(dataset = dataset_t4, method = "KW_dunn", group = "tractament", taxa_level = "all")
func5_b2 <- trans_diff$new(dataset = dataset_t5, method = "KW_dunn", group = "tractament", taxa_level = "all")
func6_b2 <- trans_diff$new(dataset = dataset_t6, method = "KW_dunn", group = "tractament", taxa_level = "all")


fap_t5_2 <- func5_b2$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                      add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                      xlab("Relative abundance (%) \nTest: KW_dunn") +
                      ylab(NULL) +
                      scale_fill_manual(
                        values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                        labels = c("AWD", "MSD", "CON")) +
                      annotate('text', x = 13, y = 3.5, label = "Sampling 5 \n(Fallow season)", size = 7, color = "black") + 
                      # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                      #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                      #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                      #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                      #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                      theme(
                        # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                        legend.title = element_blank(),
                        axis.text.y = element_text(size = 16),
                        axis.text.x = element_text(size = 16),
                        legend.position=c(0.85, 0.6),,
                        legend.text = element_text(size = 20),
                        legend.key.size = unit(30, "pt"), 
                        plot.title = element_text("t5"),
                        axis.ticks = element_blank(),
                        axis.ticks.length = unit(0, "pt")
                      )
print(fap_t5_2)

ggsave("outputs/Micro/Micro_visual/FAP_kw_samp5.pdf", plot = fap_t5_2, width = 20, height = 10, units = "in")


fap_t6_2 <- func6_b2$plot_diff_abund( #use_number = c(1, 2, 4, 5, 7, 10, 13, 19, 20),
                        add_sig = T, color_values = c("#002B5B", "#03C988", "#FF5D5D")) + 
                        xlab("Relative abundance (%) \nTest: KW_dunn") +
                        ylab(NULL) +
                        scale_fill_manual(
                          values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#FF5D5D"),
                          labels = c("AWD", "MSD", "CON")) +
                        annotate('text', x = 18, y = 3, label = "Sampling 6 \n(Fallow season)", size = 7, color = "black") + 
                        # scale_x_discrete(labels = c("methanotrophy" = "Methanotrophy", "methanogenesis_by_CO2_reduction_with_H2" =
                        #                               "Methanogenesis by \nCO2 reduction with H2", "hydrogenotrophic_methanogenesis" = "Hydrogenotrophic \nmethanogenesis",
                        #                             "methanogenesis" = "Methanogenesis", "methylotrophy" = "Methylotrophy", "sulfate_respiration" = "Sulfate respiration",
                        #                             "respiration_of_sulfur_compounds" = "Respiration of \nsulfur compounds", "dark_hydrogen_oxidation" = 
                        #                               "Dark Hydrogen \noxidation", "nitrogen_fixation" = "Nitrogen fixation")) +
                        theme(
                          # plot.margin = margin(l = 50, r = 0, t = 10, b = 0, unit = "pt"),
                          legend.title = element_blank(),
                          axis.text.y = element_text(size = 16),
                          axis.text.x = element_text(size = 16),
                          legend.position=c(0.8, 0.8),,
                          legend.text = element_text(size = 20),
                          legend.key.size = unit(30, "pt"), 
                          plot.title = element_text("t6"),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(0, "pt")
                        )
print(fap_t6_2)

ggsave("outputs/Micro/Micro_visual/FAP_kw_samp6.pdf", plot = fap_t6_2, width = 20, height = 10, units = "in")

# Arrange test:
All_fap_sampl <- grid.arrange(arrangeGrob(plot_fap_t1_2, plot_fap_t2_2, plot_fap_t3_2, plot_fap_t4_2, plot_fap_t5_2, plot_fap_t6_2, nrow = 1, ncol = 6, widths = c(0.25, 0.15, 0.15, 0.15, 0.15, 0.15))) 
ggsave("outputs/Micro/Micro_visual/All_fap_sampl.pdf", plot = All_fap_sampl ,width = 30, height = 10)  



# 2. PCo Beta diversity ####

## 2.1. Beta - Complete season ####

##### BETA DIVERSITY  #ordination using PCoA (principal coordinates analysis)
dataset_beta <- clone(dataset)
filter_microtable <- function(dataset_beta, min_abundance = 10, min_prevalence = 0.2) {
                        # Apply minimum abundance threshold to OTU table
                        dataset$otu_table <- dataset$otu_table[rowSums(dataset$otu_table) > min_abundance, ]
                        # Apply minimum prevalence threshold to OTU table
                        min_samples <- ncol(dataset$otu_table) * min_prevalence
                        dataset$otu_table <- dataset$otu_table[, colSums(dataset$otu_table > 0) >= min_samples]
                        # Filter taxonomic table based on the filtered OTU table
                        dataset$tax_table <- dataset$tax_table[rownames(dataset$otu_table), ]
                        return(dataset)
                      }

microtable_1 <- filter_microtable(dataset, min_abundance = 10, min_prevalence = 0.2)

## View the filtered microtable-class object
print(microtable_1)
#microtable-class object:
#sample_table have 15 rows and 22 columns
#otu_table have 3705 rows and 15 columns
#tax_table have 3705 rows and 8 columns
## return dataset$beta_diversity
microtable_1$cal_betadiv(bray = TRUE)
class(microtable_1$beta_diversity)
#
## save dataset$alpha_diversity to a directory
# write.xlsx(microtable$beta_diversity, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/INITIAL-TOP/BETA_DIVERSITY.xlsx", rowNames = FALSE)         

#If method parameter is not provided, the function automatically calculates Bray-curtis, Jaccard, weighted Unifrac and unweighted unifrac matrixes (Lozupone and Knight 2005).
# unifrac = FALSE means do not calculate unifrac metric
# require GUniFrac package installed
dataset$cal_betadiv(unifrac = FALSE)
# return dataset$beta_diversity
class(dataset$beta_diversity)

### 2.1.1. PCoA w/ Ellipses per stage  ####

###### Beta diversity calculation
testbeta <- trans_beta$new(dataset = dataset, group = "stages", measure = "bray")
# PCoA, PCA, DCA and NMDS are available
testbeta$cal_ordination(method = "PCoA")
class(testbeta$res_ordination)
# Save the results to an Excel file
# write.xlsx(b1$res_ordination$scores, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/INITIAL-TOP/BETA_DIVERSITY_Calc.xlsx", rowNames = FALSE)

# plot the PCoA result with confidence ellipse on Stages
testbeta_plot <- testbeta$plot_ordination(plot_color = "stages", plot_shape = "tractament", point_size = 5, point_alpha = .2, plot_type = c("point", "ellipse"), ellipse_chull_fill = FALSE)
testbeta_plot
# Customize the plot #scale_color_manual(values = c("#4daf4a", "#ffae19", "mediumpurple1"))+
testbeta_plot <- testbeta_plot + 
                  theme_classic() +
                  theme(axis.title.y = element_text(size = 10),
                        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
                        plot.title = element_text(size = 14, margin = margin(b = 20)),
                        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
                        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
                  ggtitle("16S - Beta Diversity")

testbeta_plot

ggsave("outputs/Micro/Micro_visual/PCoA_COMPL.pdf", plot = testbeta_plot ,width = 8, height = 8) 


### 2.1.2. PCoA w/ Ellipses per stages_treat (ALL)  ####

testbeta_plot2 <- testbeta$plot_ordination(plot_color = "stages_treat", plot_shape = "tractament", point_size = 5, point_alpha = .2, plot_type = c("point", "ellipse"), ellipse_chull_fill = FALSE)
testbeta_plot2
# Customize the plot #scale_color_manual(values = c("#4daf4a", "#ffae19", "mediumpurple1"))+
testbeta_plot2 <- testbeta_plot2 + 
                  theme_classic() +
                  theme(axis.title.y = element_text(size = 10),
                        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
                        plot.title = element_text(size = 14, margin = margin(b = 20)),
                        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
                        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
                  ggtitle("16S - Beta Diversity")
testbeta_plot2

ggsave("outputs/Micro/Micro_visual/PCoA_COMPL2.pdf", plot = testbeta_plot2 ,width = 8, height = 8) 

### 2.1.3. PCoA w/ Ellipses per stages_treat (one plot per stage)  ####

testbeta_plot3 <- testbeta$plot_ordination(plot_color = "stages_treat", plot_shape = "stages_treat", point_size = 5, point_alpha = .4, plot_type = "point", ellipse_chull_fill = FALSE, color_values = c("#002B5B", "#03C988", "#FF5D5D"))

testbeta_plot3 <- testbeta_plot3 + 
                  theme_classic()  +
                  # scale_color_manual(
                    # values = c("initial_CONV" = "#002B5B", "initial_MSD" = "#03C988", "initial_AWD" = "#FF5D5D",
                    #            "growing_CONV" = "#002B5B", "growing_MSD" = "#03C988", "growing_AWD" = "#FF5D5D",
                    #            "post-harvest_CONV" = "#002B5B", "post-harvest_MSD" = "#03C988", "post-harvest_AWD" = "#FF5D5D"),
                    # labels = c("initial_CONV" = "IN - CON", "initial_MSD" = "IN - MSD", "initial_AWD" = "IN - AWD",
                    #            "growing_CONV" = "GS - CON", "growing_MSD" = "GS - MSD", "growing_AWD" = "GS - AWD",
                    #            "post-harvest_CONV" = "FS - CON", "post-harvest_MSD" = "FS - MSD", "post-harvest_AWD" = "FS - AWD")) +
                  theme(axis.title.y = element_text(size = 10),
                        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
                        plot.title = element_text(size = 14, margin = margin(b = 20)),
                        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
                        legend.text = element_text(size = 10))  # Adjust the size of the legend text
                  # + ggtitle("16S - Beta Diversity")
testbeta_plot3

selected_stage_ini <- c("initial_CONV", "initial_MSD", "initial_AWD")  
selected_stage_GS <- c("growing_CONV", "growing_MSD", "growing_AWD") 
selected_stage_FS <- c("post-harvest_CONV", "post-harvest_MSD", "post-harvest_AWD") 

y_limits_beta <- c(-0.3, 0.5) 
y_breaks_beta <- seq(-0.2, 0.4, by = 0.2) 

testbeta_plot_ini <- testbeta_plot3 + 
                  stat_ellipse(data = subset(testbeta_plot3$data, stages_treat %in% selected_stage_ini)) +
                  scale_color_manual(
                    values = c("initial_CONV" = "#002B5B", "initial_MSD" = "#03C988", "initial_AWD" = "#FF5D5D",
                               "growing_CONV" = "#002B5B", "growing_MSD" = "#03C988", "growing_AWD" = "#FF5D5D",
                               "post-harvest_CONV" = "#002B5B", "post-harvest_MSD" = "#03C988", "post-harvest_AWD" = "#FF5D5D"),
                    labels = c("initial_CONV" = "IN - CON", "initial_MSD" = "IN - MSD", "initial_AWD" = "IN - AWD"),
                    breaks = c("initial_CONV", "initial_MSD", "initial_AWD")) +
                  xlab("") +
                  scale_shape_manual(
                    values = c("initial_CONV" = 16, "initial_MSD" = 16, "initial_AWD" = 16,
                               "growing_CONV" =  17, "growing_MSD" = 17, "growing_AWD" = 17,
                               "post-harvest_CONV" =  18, "post-harvest_MSD" = 18, "post-harvest_AWD" = 18),  
                    labels = c("initial_CONV" = "IN - CON", "initial_MSD" = "IN - MSD", "initial_AWD" = "IN - AWD"),
                    breaks = c("initial_CONV", "initial_MSD", "initial_AWD")) +
                  guides(shape = guide_legend(title = NULL),
                         color = guide_legend(title = NULL)) +
                  theme(legend.position = c(0.2, 0.8)) + 
                  scale_y_continuous(limits = y_limits_beta, breaks = y_breaks_beta)

testbeta_plot_GS <- testbeta_plot3 + 
                  stat_ellipse(data = subset(testbeta_plot3$data, stages_treat %in% selected_stage_GS)) +
                  scale_color_manual(
                    values = c("initial_CONV" = "#002B5B", "initial_MSD" = "#03C988", "initial_AWD" = "#FF5D5D",
                               "growing_CONV" = "#002B5B", "growing_MSD" = "#03C988", "growing_AWD" = "#FF5D5D",
                               "post-harvest_CONV" = "#002B5B", "post-harvest_MSD" = "#03C988", "post-harvest_AWD" = "#FF5D5D"),
                    labels = c("growing_CONV" = "GS - CON", "growing_MSD" = "GS - MSD", "growing_AWD" = "GS - AWD"),
                    breaks = c("growing_CONV", "growing_MSD", "growing_AWD")) +
                  scale_shape_manual(
                    values = c("initial_CONV" = 16, "initial_MSD" = 16, "initial_AWD" = 16,
                               "growing_CONV" =  17, "growing_MSD" = 17, "growing_AWD" = 17,
                               "post-harvest_CONV" =  18, "post-harvest_MSD" = 18, "post-harvest_AWD" = 18), 
                    labels = c("growing_CONV" = "GS - CON", "growing_MSD" = "GS - MSD", "growing_AWD" = "GS - AWD"),
                    breaks = c("growing_CONV", "growing_MSD", "growing_AWD")) +
                  guides(shape = guide_legend(title = NULL),
                         color = guide_legend(title = NULL)) +
                  theme(legend.position = c(0.2, 0.8),
                        axis.text.y = element_blank(),
                        axis.title.y = element_blank()) + 
                  scale_y_continuous(limits = y_limits_beta, breaks = y_breaks_beta)


testbeta_plot_FS <- testbeta_plot3 + 
                  stat_ellipse(data = subset(testbeta_plot3$data, stages_treat %in% selected_stage_FS)) +
                  scale_color_manual(
                    values = c("initial_CONV" = "#002B5B", "initial_MSD" = "#03C988", "initial_AWD" = "#FF5D5D",
                               "growing_CONV" = "#002B5B", "growing_MSD" = "#03C988", "growing_AWD" = "#FF5D5D",
                               "post-harvest_CONV" = "#002B5B", "post-harvest_MSD" = "#03C988", "post-harvest_AWD" = "#FF5D5D"),
                    labels = c("post-harvest_CONV" = "FS - CON", "post-harvest_MSD" = "FS - MSD", "post-harvest_AWD" = "FS - AWD"),
                    breaks = c("post-harvest_CONV", "post-harvest_MSD", "post-harvest_AWD")) +
                  xlab("") +
                  scale_shape_manual(
                    values = c("initial_CONV" = 16, "initial_MSD" = 16, "initial_AWD" = 16,
                               "growing_CONV" =  17, "growing_MSD" = 17, "growing_AWD" = 17,
                               "post-harvest_CONV" =  18, "post-harvest_MSD" = 18, "post-harvest_AWD" = 18), 
                    labels = c("post-harvest_CONV" = "FS - CON", "post-harvest_MSD" = "FS - MSD", "post-harvest_AWD" = "FS - AWD"),
                    breaks = c("post-harvest_CONV", "post-harvest_MSD", "post-harvest_AWD")) +
                  guides(shape = guide_legend(title = NULL),
                         color = guide_legend(title = NULL)) +
                  theme(legend.position = c(0.2, 0.8),
                        axis.text.y = element_blank(),
                        axis.title.y = element_blank()) + 
                  scale_y_continuous(limits = y_limits_beta, breaks = y_breaks_beta)

testbeta_plot_arr <- grid.arrange(testbeta_plot_ini, testbeta_plot_GS, testbeta_plot_FS, ncol = 3) 

ggsave("outputs/Micro/Micro_visual/PCoA_stages_treat.pdf", plot = testbeta_plot_arr, width = 12, height = 8) 

### 2.1.4. PERMANOVA & ANOSIM  ####

## PerMANOVA 
testbeta$cal_manova(manova_all = TRUE)
testbeta$res_manova

##diferencias entre grupos
testbeta$cal_manova(manova_all = FALSE)
testbeta$res_manova

#From v1.0.0, ANOSIM method is also available.
# the group parameter is not necessary when it is provided in creating the object
testbeta$cal_anosim(group = "tractament", paired = TRUE)
testbeta$res_anosim

## 2.2. Beta - Initial stage ####

dataset_INI <- clone(dataset)
# select the group
dataset_INI$sample_table <- subset(dataset_INI$sample_table, stages == "initial") 

##### BETA DIVERSITY  #ordination using PCoA (principal coordinates analysis)
dataset_INIb <- clone(dataset_INI)
filter_microtable <- function(dataset_INIb, min_abundance = 10, min_prevalence = 0.2) {
  
  # Apply minimum abundance threshold to OTU table
  dataset_INI$otu_table <- dataset_INI$otu_table[rowSums(dataset_INI$otu_table) > min_abundance, ]
  
  # Apply minimum prevalence threshold to OTU table
  min_samples <- ncol(dataset_INI$otu_table) * min_prevalence
  dataset_INI$otu_table <- dataset_INI$otu_table[, colSums(dataset_INI$otu_table > 0) >= min_samples]
  
  # Filter taxonomic table based on the filtered OTU table
  dataset_INI$tax_table <- dataset_INI$tax_table[rownames(dataset_INI$otu_table), ]
  
  return(dataset_INI)
}

microtable <- filter_microtable(dataset_INI, min_abundance = 10, min_prevalence = 0.2)

## View the filtered microtable-class object
print(microtable)
#microtable-class object:
#sample_table have 45 rows and 22 columns
#otu_table have 8580 rows and 45 columns
#tax_table have 8580 rows and 8 columns
## return dataset$beta_diversity
microtable$cal_betadiv(bray = TRUE)
class(microtable$beta_diversity)

dataset_INI$cal_betadiv(unifrac = FALSE) # dataset_INI previously defined in CERESTRES_BACTERIS_microeco_STAGES
# return dataset$beta_diversity
class(dataset_INI$beta_diversity)

beta_INI <- trans_beta$new(dataset = dataset_INI, group = "tractament", measure = "bray")

beta_INI$cal_ordination(method = "PCoA") # PCoA, PCA, DCA and NMDS are available
class(beta_INI$res_ordination)

# plot the PCoA result with confidence ellipses
ggbeta_INI <- beta_INI$plot_ordination(plot_color = "tractament",  point_size = 5, point_alpha = .2, plot_type = c("point", "ellipse"), ellipse_chull_fill = FALSE, color_values = c("#002B5B", "#03C988", "#C31616"))
ggbeta_INI

# Customize the plot

y_limits_betaIN <- c(-0.4, 0.3) 
y_breaks_betaIN <- seq(-0.4, 0.3, by = 0.1) 

ggbeta_INI <- ggbeta_INI + 
                theme_classic() +
                scale_color_manual(values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#C31616"),
                     labels = c("CONV" = "CON", "MSD" = "MSD", "AWD" = "AWD")) +
                annotate('text', x = -0.2, y =  -0.4, label = "IN", size = 4, color = "black") +
                theme(
                      axis.title.y = element_text(size = 10),
                        axis.text.x = element_text(vjust = 0.5), #, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
                        plot.title = element_text(size = 14), #, margin = margin(b = 20)),
                        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
                        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
                scale_y_continuous(limits = y_limits_betaIN, breaks = y_breaks_betaIN) +
                guides(color = guide_legend(title = NULL)) +
                theme(legend.position = c(0.87, 0.10))

print(ggbeta_INI)

ggsave("outputs/Micro/Micro_visual/PCoA_INI.pdf", plot = ggbeta_INI ,width = 8, height = 8) 

## PerMANOVA 
beta_INI$cal_manova(manova_all = TRUE)
beta_INI$res_manova

##diferencias entre grupos
beta_INI$cal_manova(manova_all = FALSE)
beta_INI$res_manova

#From v1.0.0, ANOSIM method is also available.
# the group parameter is not necessary when it is provided in creating the object
beta_INI$cal_anosim(group = "tractament", paired = TRUE)
beta_INI$res_anosim

## 2.3. Beta - Growing season ####

################# SUBSET OF SAMPLES

dataset_GRO <- clone(dataset)
# select the group
dataset_GRO$sample_table <- subset(dataset_GRO$sample_table, stages == "growing") 

##### BETA DIVERSITY  #ordination using PCoA (principal coordinates analysis)
dataset_GROb <- clone(dataset_GRO)
filter_microtable <- function(dataset_GROb, min_abundance = 10, min_prevalence = 0.2) {
  
  # Apply minimum abundance threshold to OTU table
  dataset_GRO$otu_table <- dataset_GRO$otu_table[rowSums(dataset_GRO$otu_table) > min_abundance, ]
  
  # Apply minimum prevalence threshold to OTU table
  min_samples <- ncol(dataset_GRO$otu_table) * min_prevalence
  dataset_GRO$otu_table <- dataset_GRO$otu_table[, colSums(dataset_GRO$otu_table > 0) >= min_samples]
  
  # Filter taxonomic table based on the filtered OTU table
  dataset_GRO$tax_table <- dataset_GRO$tax_table[rownames(dataset_GRO$otu_table), ]
  
  return(dataset_GRO)
}

microtable <- filter_microtable(dataset_GRO, min_abundance = 10, min_prevalence = 0.2)

## View the filtered microtable-class object
print(microtable)
#microtable-class object:
# sample_table have 89 rows and 22 columns
# otu_table have 18216 rows and 164 columns
# tax_table have 18216 rows and 8 columns
## return dataset$beta_diversity
microtable$cal_betadiv(bray = TRUE)
class(microtable$beta_diversity)

dataset_GRO$cal_betadiv(unifrac = FALSE) # dataset_GRO previously defined in CERESTRES_BACTERIS_microeco_STAGES
# return dataset$beta_diversity
class(x = dataset_GRO$beta_diversity)

beta_GRO <- trans_beta$new(dataset = dataset_GRO, group = "tractament", measure = "bray")

beta_GRO$cal_ordination(method = "PCoA") # PCoA, PCA, DCA and NMDS are available
class(beta_GRO$res_ordination)

# plot the PCoA result with confidence ellipses
ggbeta_GRO <- beta_GRO$plot_ordination(plot_color = "tractament", point_size = 5, point_alpha = .2, plot_type = c("point", "ellipse"), ellipse_chull_fill = FALSE, color_values = c("#002B5B", "#03C988", "#C31616"))
ggbeta_GRO

# Customize the plot
ggbeta_GRO <- ggbeta_GRO + 
                theme_classic() +
                scale_color_manual(values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#C31616"),
                                   labels = c("CONV" = "CON", "MSD" = "MSD", "AWD" = "AWD")) +
                annotate('text', x = -0.37, y =  -0.4, label = "GS", size = 8, color = "black") +
                theme(
                  axis.title.y = element_text(size = 15),
                  axis.title.x = element_text(size = 15),
                  axis.text.x = element_text(vjust = 0.5, size = 12), 
                  axis.text.y = element_text(size = 12),
                  plot.title = element_text(size = 14), 
                  legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
                  legend.text = element_text(size = 15)) + # Adjust the size of the legend text
                scale_y_continuous(limits = y_limits_betaIN, breaks = y_breaks_betaIN) +
                guides(color = guide_legend(title = NULL)) +
                theme(legend.position = c(0.87, 0.10))
                        
ggbeta_GRO

ggsave("outputs/Micro/Micro_visual/PCoA_GS.pdf", plot = ggbeta_GRO ,width = 8, height = 8) 

## PerMANOVA 
beta_GRO$cal_manova(manova_all = TRUE)
beta_GRO$res_manova

##diferencias entre grupos
beta_GRO$cal_manova(manova_all = FALSE)
beta_GRO$res_manova

#From v1.0.0, ANOSIM method is also available.
# the group parameter is not necessary when it is provided in creating the object
beta_GRO$cal_anosim(group = "tractament", paired = TRUE)
beta_GRO$res_anosim

## 2.4. Beta - Fallow season ####

dataset_POST <- clone(dataset)
# select the group
dataset_POST$sample_table <- subset(dataset_POST$sample_table, stages == "post-harvest") 

##### BETA DIVERSITY  #ordination using PCoA (principal coordinates analysis)
dataset_FSb <- clone(dataset_POST)
filter_microtable <- function(dataset_FSb, min_abundance = 10, min_prevalence = 0.2) {
  
  # Apply minimum abundance threshold to OTU table
  dataset_POST$otu_table <- dataset_POST$otu_table[rowSums(dataset_POST$otu_table) > min_abundance, ]
  
  # Apply minimum prevalence threshold to OTU table
  min_samples <- ncol(dataset_POST$otu_table) * min_prevalence
  dataset_POST$otu_table <- dataset_POST$otu_table[, colSums(dataset_POST$otu_table > 0) >= min_samples]
  
  # Filter taxonomic table based on the filtered OTU table
  dataset_POST$tax_table <- dataset_POST$tax_table[rownames(dataset_POST$otu_table), ]
  
  return(dataset_POST)
}

microtable <- filter_microtable(dataset_POST, min_abundance = 10, min_prevalence = 0.2)

## View the filtered microtable-class object
print(microtable)
#microtable-class object:
#sample_table have 45 rows and 22 columns
#otu_table have 8580 rows and 45 columns
#tax_table have 8580 rows and 8 columns
## return dataset$beta_diversity
microtable$cal_betadiv(bray = TRUE)
class(microtable$beta_diversity)

dataset_POST$cal_betadiv(unifrac = FALSE) # dataset_POST previously defined in CERESTRES_BACTERIS_microeco_STAGES
# return dataset$beta_diversity
class(dataset_POST$beta_diversity)

beta_FS <- trans_beta$new(dataset = dataset_POST, group = "tractament", measure = "bray")

beta_FS$cal_ordination(method = "PCoA") # PCoA, PCA, DCA and NMDS are available
class(beta_FS$res_ordination)

# plot the PCoA result with confidence ellipses
ggbeta_FS <- beta_FS$plot_ordination(plot_color = "tractament", point_size = 5, point_alpha = .2, plot_type = c("point", "ellipse"), ellipse_chull_fill = FALSE, color_values = c("#002B5B", "#03C988", "#C31616"))
ggbeta_FS

# Customize the plot
ggbeta_FS <- ggbeta_FS + 
          theme_classic() +
          annotate('text', x = -0.30, y =  -0.4, label = "FS", size = 8, color = "black") +
          theme(
            axis.title.y = element_text(size = 15),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(vjust = 0.5, size = 12), 
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 14), 
            legend.position ="none") + # Adjust the size of the legend text 
          scale_y_continuous(limits = y_limits_betaIN, breaks = y_breaks_betaIN) 

ggbeta_FS

ggsave("outputs/Micro/Micro_visual/PCoA_FS.pdf", plot = ggbeta_FS ,width = 8, height = 8) 

## PerMANOVA 
beta_FS$cal_manova(manova_all = TRUE)
beta_FS$res_manova

##diferencias entre grupos
beta_FS$cal_manova(manova_all = FALSE)
beta_FS$res_manova

#From v1.0.0, ANOSIM method is also available.
# the group parameter is not necessary when it is provided in creating the object
beta_FS$cal_anosim(group = "tractament", paired = TRUE)
beta_FS$res_anosim

# Plot arrange
testbeta_plot_arr2 <- grid.arrange(ggbeta_INI, ggbeta_GRO, ggbeta_FS, ncol = 3) 
testbeta_plot_arrGSFS <- grid.arrange(ggbeta_GRO, ggbeta_FS, ncol = 2) 

ggsave("outputs/Micro/Micro_visual/PCoA_stages_treat2.pdf", plot = testbeta_plot_arr2, width = 12, height = 8) 
ggsave("outputs/Micro/Micro_visual/PCoA_stages_treatGSFS.pdf", plot = testbeta_plot_arrGSFS, width = 12, height = 8) 

# 3. Fallow season: Plots for Relative Abundance per f(x) group ####

## 3.1. Methanotrophs ####

### 3.1.1. Methanotrophs - all ####

# Note: GLMM did not require log transformation for relative abundance 

Metr_FS <- Metr_lab %>% 
              filter(Stage == "FS")

Metr_FSavg <- Metr_FS %>% 
              group_by(Treat) %>%
              summarize(mean_Relab = mean(Rel_ab), se_Relab = sd(Rel_ab) / sqrt(n()))

Metr_plot <- ggplot(Metr_FS, aes(Treat, Rel_ab, group = Treat, colour = Treat, fill = Treat)) +
              geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
              scale_colour_manual(values = c("#002B5B", "#03C988", "#C31616")) +
              theme_bw() +
              annotate('text', x = 2, y =  0.5, size = 5, color = "black",  label="bold('Methanotrophs - All')", parse=TRUE) +
              scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#C31616"), guide = "none") +
              ylab("Relative abundance (%)") +
              scale_y_sqrt() +
              # coord_cartesian(ylim = c(0, 0.50)) +
              theme(
                axis.title.x = element_blank(), legend.position = "none", axis.title.y = element_blank(),
                axis.text.y = element_text(margin = margin(r = 0), size = 13), panel.border = element_rect(size = 1), axis.text.x = element_text(size = 14)) +
              geom_point(data = Metr_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, colour = "black", size = 11) +
              geom_point(data = Metr_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, size = 10) +
              geom_errorbar(data = Metr_FSavg, aes(x = Treat, y = mean_Relab, ymin = mean_Relab - se_Relab, ymax = mean_Relab + se_Relab), width = 0.3, size = 1) +
              geom_signif(comparisons = list(c("CON", "MSD"), c("MSD", "AWD"), c("CON", "AWD")), annotations = c("n.s.", "n.s.", "*"),
                          y_position = c(0.585, 0.57, 0.61), tip_length = 0.02, vjust = 0, textsize = 5, color = "black") 

Metr_plot

### 3.1.2. Methanotrophs - alpha ####

Metr_alpha_FS <- Metr_alph_lab2 %>% 
              filter(Stage == "FS")

Metr_alpha_FSavg <- Metr_alpha_FS %>% 
              group_by(Treat) %>%
              summarize(mean_Relab = mean(Rel_ab), se_Relab = sd(Rel_ab) / sqrt(n()))

Metr_alpha_plot <- ggplot(Metr_alpha_FS, aes(Treat, Rel_ab, group = Treat, colour = Treat, fill = Treat)) +
              geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
              scale_colour_manual(values = c("#002B5B", "#03C988", "#C31616")) +
              theme_bw() +
              annotate('text', x = 2, y =  0.6, size = 5, color = "black",  label="bold('      Methanotrophs \nAlphaproteobacteria')", parse=TRUE) +
              scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#C31616"), guide = "none") +
              ylab("Relative abundance (%)") +
              scale_y_sqrt() +
              coord_cartesian(ylim = c(0, 0.7)) +
              theme(
                axis.title.x = element_blank(), legend.position = "none", axis.title.y = element_blank(),
                axis.text.y = element_text(margin = margin(r = 0), size = 13), panel.border = element_rect(size = 1), axis.text.x = element_blank()) +
              geom_point(data = Metr_alpha_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, colour = "black", size = 11) +
              geom_point(data = Metr_alpha_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, size = 10) +
              geom_errorbar(data = Metr_alpha_FSavg, aes(x = Treat, y = mean_Relab, ymin = mean_Relab - se_Relab, ymax = mean_Relab + se_Relab), width = 0.3, size = 1) +
              geom_signif(comparisons = list(c("CON", "MSD"), c("MSD", "AWD"), c("CON", "AWD")), annotations = c("n.s.", "n.s.", "n.s."),
                          y_position = c(0.55, 0.6, 0.67), tip_length = 0.02, vjust = 0, textsize = 5, color = "black") 

Metr_alpha_plot

### 3.1.3. Methanotrophs - gamma ####

Metr_gamma_FS <- Metr_gamm_lab2 %>% 
              filter(Stage == "FS")

Metr_gamma_FSavg <- Metr_gamma_FS %>% 
              group_by(Treat) %>%
              summarize(mean_Relab = mean(Rel_ab), se_Relab = sd(Rel_ab) / sqrt(n()))

Metr_gamma_plot <- ggplot(Metr_gamma_FS, aes(Treat, Rel_ab, group = Treat, colour = Treat, fill = Treat)) +
              geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
              scale_colour_manual(values = c("#002B5B", "#03C988", "#C31616")) +
              theme_bw() +
              annotate('text', x = 2, y =  0.4, size = 5, color = "black",  label="bold('      Methanotrophs \nGammaproteobacteria')", parse=TRUE) +
              scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#C31616"), guide = "none") +
              ylab("Relative abundance (%)") +
              scale_y_sqrt() +
              coord_cartesian(ylim = c(0.005, 0.5)) +  # Set the y-axis limits
              theme(
                axis.title.x = element_blank(), legend.position = "none", axis.title.y = element_blank(),
                axis.text.y = element_text(margin = margin(r = 0), size = 12), panel.border = element_rect(size = 1), axis.text.x = element_text(size = 14)) +
              geom_point(data = Metr_gamma_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, colour = "black", size = 11) +
              geom_point(data = Metr_gamma_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, size = 10) +
              geom_errorbar(data = Metr_gamma_FSavg, aes(x = Treat, y = mean_Relab, ymin = mean_Relab - se_Relab, ymax = mean_Relab + se_Relab), width = 0.3, size = 1) +
              geom_signif(comparisons = list(c("CON", "MSD"), c("MSD", "AWD"), c("CON", "AWD")), annotations = c("n.s.", "*", "*"),
                          y_position = c(0.43, 0.55, 0.48), tip_length = 0.02, vjust = 0, textsize = 5, color = "black") 
Metr_gamma_plot

## 3.2. Methanogens ####
# Note: GLMM required log transformation for relative abundance 

Metg_FS <- Metg_lab2 %>% 
              filter(Stage == "FS")

Metg_FSavg <- Metg_FS %>% 
              group_by(Treat) %>%
              summarize(mean_Relab_log = mean(Rel_ab_log), se_Relab_log = sd(Rel_ab_log) / sqrt(n()),
                        mean_Relab = mean(Rel_ab), se_Relab = sd(Rel_ab) / sqrt(n()))

# Log transformed
Metg_log_plot  <- ggplot(Metg_FS, aes(Treat, Rel_ab_log, group = Treat, colour = Treat, fill = Treat)) +
              geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
              scale_colour_manual(values = c("#002B5B", "#03C988", "#C31616")) +
              theme_bw() +
              annotate('text', x = 2, y =  -2, size = 5, color = "black",  label="bold('Methanogens')", parse=TRUE) +
              scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#C31616"), guide = "none") +
              ylab("Relative abundance (log)") +
  # scale_y_sqrt() +
              theme(
                axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_blank(), legend.position = "none", 
                axis.text.y = element_text(margin = margin(r = 0)), panel.border = element_rect(size = 1)) +
              geom_point(data = Metg_FSavg, aes(x = Treat, y = mean_Relab_log), shape = 19, colour = "black", size = 11) +
              geom_point(data = Metg_FSavg, aes(x = Treat, y = mean_Relab_log), shape = 19, size = 10) +
              geom_errorbar(data = Metg_FSavg, aes(x = Treat, y = mean_Relab_log, ymin = mean_Relab_log - se_Relab_log, 
                                                   ymax = mean_Relab_log + se_Relab_log), width = 0.3, size = 1)

Metg_log_plot

# Non Log transformed
Metg_plot <- ggplot(Metg_FS, aes(Treat, Rel_ab, group = Treat, colour = Treat, fill = Treat)) +
          geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
          scale_colour_manual(values = c("#002B5B", "#03C988", "#C31616")) +
          theme_bw() +
          annotate('text', x = 2, y =  10.7, size = 5, color = "black",  label="bold('Methanogens')", parse=TRUE) +
          scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#C31616"), guide = "none") +
          ylab("Relative abundance (%)") +
          scale_y_sqrt() +
          theme(
            axis.title.y = element_text(margin = margin(r = 12), size = 15), axis.title.x = element_blank(), legend.position = "none", 
            axis.text.y = element_text(margin = margin(r = 0), size = 13), panel.border = element_rect(size = 1), axis.text.x = element_text(size = 14)) +
          geom_point(data = Metg_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, colour = "black", size = 11) +
          geom_point(data = Metg_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, size = 10) +
          geom_errorbar(data = Metg_FSavg, aes(x = Treat, y = mean_Relab, ymin = mean_Relab - se_Relab, ymax = mean_Relab + se_Relab), width = 0.3, size = 1)  +
          geom_signif(comparisons = list(c("CON", "MSD"), c("MSD", "AWD"), c("CON", "AWD")), annotations = c("***", "n.s.", "**"),
                      y_position = c(2.3, 2.45, 2.6), tip_length = 0.02, vjust = 0, textsize = 5, color = "black") 

Metg_plot

## 3.3. Methylotrophs ####

Mety_FS <- Mety_lab2 %>% 
  filter(Stage == "FS")

Mety_FSavg <- Mety_FS %>% 
  group_by(Treat) %>%
  summarize(mean_Relab_log = mean(Rel_ab_log), se_Relab_log = sd(Rel_ab_log) / sqrt(n()),
            mean_Relab = mean(Rel_ab), se_Relab = sd(Rel_ab) / sqrt(n()))

# Non Log transformed
Mety_plot <- ggplot(Mety_FS, aes(Treat, Rel_ab, group = Treat, colour = Treat, fill = Treat)) +
          geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
          scale_colour_manual(values = c("#002B5B", "#03C988", "#C31616")) +
          theme_bw() +
          annotate('text', x = 2, y =  0.75, size = 5, color = "black",  label="bold('Methylotrophs')", parse=TRUE) +
          scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#C31616"), guide = "none") +
          ylab("Relative abundance (%)") +
          scale_y_sqrt() +
          theme(
            axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "none", 
            axis.text.y = element_text(margin = margin(r = 0), size = 12), panel.border = element_rect(size = 1), axis.text.x = element_text(size = 12)) +
          geom_point(data = Mety_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, colour = "black", size = 11) +
          geom_point(data = Mety_FSavg, aes(x = Treat, y = mean_Relab), shape = 19, size = 10) +
          geom_errorbar(data = Mety_FSavg, aes(x = Treat, y = mean_Relab, ymin = mean_Relab - se_Relab, ymax = mean_Relab + se_Relab), width = 0.3, size = 1)  +
          geom_signif(comparisons = list(c("CON", "MSD"), c("MSD", "AWD"), c("CON", "AWD")), annotations = c("n.s.", "n.s.", "n.s."),
                      y_position = c(0.65, 0.68, 0.71), tip_length = 0.02, vjust = 0, textsize = 4, color = "black") 

Mety_plot

# Plot arrange
FuncGroups_plot_arr <- grid.arrange(Metg_plot, Metr_plot, Mety_plot, ncol = 3) 
FuncGroups_plot_arr2 <- ((Metg_plot | Metr_plot | (Metr_alpha_plot / Metr_gamma_plot) | Mety_plot))

ggsave("outputs/Micro/Micro_visual/FuncGroups_plot_arr.pdf", plot = FuncGroups_plot_arr, width = 12, height = 6) 
ggsave("outputs/Micro/Micro_visual/FuncGroups_plot_arr2.pdf", plot = FuncGroups_plot_arr2, width = 18, height = 9) 


# 4. Growing season: Plots for Relative Abundance per f(x) group ####

### 4.1.1. Methanotrophs - all ####

# Note: GLMM did not require log transformation for relative abundance 

# Metr_GS <- Metr_lab %>% 
#                 filter(Stage == "GS")
# 
# Metr_GSavg <- Metr_GS %>% 
#                 group_by(Treat) %>%
#                 summarize(mean_Relab = mean(Rel_ab), se_Relab = sd(Rel_ab) / sqrt(n()))
# 
# Metr_plot_GS <- ggplot(Metr_GS, aes(Treat, Rel_ab, group = Treat, colour = Treat, fill = Treat)) +
#                 geom_point(position = position_jitterdodge (0.80, jitter.width = 0.2, jitter.height = 0), alpha = 0.2, shape = 21,colour = "black",size = 10)+
#                 scale_colour_manual(values = c("#002B5B", "#03C988", "#C31616")) +
#                 theme_bw() +
#                 annotate('text', x = 2, y =  0.5, size = 5, color = "black",  label="bold('Methanotrophs - All')", parse=TRUE) +
#                 scale_fill_manual(values = c(CON = "#002B5B", MSD = "#03C988", AWD = "#C31616"), guide = "none") +
#                 ylab("Relative abundance (%)") +
#                 scale_y_sqrt() +
#                 # coord_cartesian(ylim = c(0, 0.50)) +
#                 theme(
#                   axis.title.x = element_blank(), legend.position = "none", axis.title.y = element_blank(),
#                   axis.text.y = element_text(margin = margin(r = 0), size = 12), panel.border = element_rect(size = 1), axis.text.x = element_text(size = 12)) +
#                 geom_point(data = Metr_GSavg, aes(x = Treat, y = mean_Relab), shape = 19, colour = "black", size = 11) +
#                 geom_point(data = Metr_GSavg, aes(x = Treat, y = mean_Relab), shape = 19, size = 10) +
#                 geom_errorbar(data = Metr_GSavg, aes(x = Treat, y = mean_Relab, ymin = mean_Relab - se_Relab, ymax = mean_Relab + se_Relab), width = 0.3, size = 1) +
#                 geom_signif(comparisons = list(c("CON", "MSD"), c("MSD", "AWD"), c("CON", "AWD")), annotations = c("n.s.", "n.s.", "*"),
#                             y_position = c(0.585, 0.57, 0.61), tip_length = 0.02, vjust = 0, textsize = 4, color = "black") 
# 
# Metr_plot_GS

# 5. BACTOT & mcrA - Fallow Season ####

env_FS <-trans_env$new(dataset = dataset_POST, env_cols = c("BACTOT", "ITS", "ITS_BACTOT", "AOB", "AOB_BACTOT", "mcrA", "mcrA_BACTOT"))

##cal_diff function is used to test the significance of variables across groups 
##wilcox
env_FS$cal_diff(group = "tractament", method = "wilcox")
env_FS$res_diff

##anova
env_FS$cal_diff(method = "anova", group = "tractament")
env_FS$res_diff

anova_model <- env_FS$res_diff[[1]]  
summary(anova_model)

tmp_env_FS <- list()
for (i in colnames(env_FS$data_env)) {
              tmp_env_FS[[i]] <- env_FS$plot_diff(measure = i, plot_color = "tractament", add_sig_text = TRUE, add_sig_text_size = 3, xtext_size = 9, color_values = c("#002B5B", "#03C988", "#C31616")) +
                theme(plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
                      axis.title.y = element_text(size = 10),  # Change y-axis title size
                      axis.text.y = element_text(size = 10))  # Change y-axis text size
            }

## 4.1. BACTOT ####

BACT_FS <- env_FS$plot_diff(measure = "BACTOT", add_sig_text_size= 5,  fill = "tractament", color = "black", add_sig = T)  +
                theme_bw() +
                labs(y = "Total bacterial abundance<br><span style='font-size:14pt'>(16S rRNA gene copies per gram of soil)</span>") +
                # scale_y_continuous(trans='log10')+
                scale_fill_manual(values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#C31616"))  +
                scale_x_discrete(labels = c("CONV" = "CON", "MSD" = "MSD", "AWD" = "AWD"))  +
                theme(
                  legend.position = "none", 
                  axis.title.y = element_markdown(size = 15, hjust = 0.5),
                  # axis.text.x = element_text(size = 12, angle=0),
                  axis.text.y = element_text(size =  13, angle = 90,  hjust=0.2),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank())

BACT_FS

## 4.2. mcrA ####

mcrA_FS <- env_FS$plot_diff(measure = "mcrA", add_sig_text_size= 5,  fill = "tractament", color = "black", add_sig = F)  +
                theme_bw() +
                labs(y = "Total methanogenic archaeal abundance<br><span style='font-size:14pt'>(mcrA gene copies per gram of soil)</span>") +
                # scale_y_continuous(trans='log10')+
                scale_fill_manual(values = c("CONV" = "#002B5B", "MSD" = "#03C988", "AWD" = "#C31616"))  +
                scale_x_discrete(labels = c("CONV" = "CON", "MSD" = "MSD", "AWD" = "AWD"))  +
                theme(
                  legend.position = "none", 
                  axis.title.y = element_markdown(size = 15, hjust = 0.5),
                  axis.text.x = element_text(size = 14, angle= 0),
                  axis.text.y = element_text(size = 13, angle = 90,  hjust = 0.2))

mcrA_FS

BACT_mcrA_arr <- (BACT_FS / mcrA_FS)
FuncGroups_plot_arr3 <- (((BACT_FS / mcrA_FS) | Metg_plot | Metr_plot | (Metr_alpha_plot / Metr_gamma_plot)))
ggsave("outputs/Micro/Micro_visual/FuncGroups_plot_arr3.pdf", plot = FuncGroups_plot_arr3, width = 18, height = 9) 


