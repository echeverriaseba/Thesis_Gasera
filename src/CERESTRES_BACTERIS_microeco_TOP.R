#https://chiliubio.github.io/microeco_tutorial/

# Load the microeco package again
library(microeco)
library(readxl)
library(dplyr)
library(magrittr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(openxlsx)
library(writexl)

# Set the working directory
setwd("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/RAW DATA")
file.path("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/RAW DATA")
list.files()

# Set the path to the OTU table
otu_mat<- read_excel("OTU_TABLE_CERESTRES_BACT.xlsx")
head(otu_mat)

# Set the path to the TAXONOMY table
tax_mat<- read_excel("TAX_TABLE_CERESTRES_BAC.xlsx")
##make the taxonomic information unified (unify the taxonomic prefix)
#tax_mat %<>% tidy_taxonomy   (triga molt temps, mirar-ho!)

# Set the path to the SAMPLES table
sample_mat <- read_excel("METADATA_TABLE_CERESTRES_BAC_for RDA.xlsx")
head(sample_mat)

# Preparing tables to be a phyloseq/microeco object
otu_mat <- otu_mat %>%
  tibble::column_to_rownames("otu") 

samples_df <- sample_mat %>% 
  tibble::column_to_rownames("sample") 

tax_df <- as.data.frame(tax_mat)

# Standardize row names in OTU and taxonomy tables
rownames(otu_mat) <- rownames(tax_df)

# Check if row names match now
identical(rownames(otu_mat), rownames(tax_mat))

# Create MICROECO object
dataset <- microtable$new(otu_table = otu_mat)
dataset <- microtable$new(otu_table = otu_mat, sample_table = samples_df)
dataset <- microtable$new(sample_table = samples_df, otu_table = otu_mat, tax_table = tax_df)
dataset$otu_table
dataset$sample_table
dataset$tax_table
dataset
#microtable-class object:
#sample_table have 165 rows and 9 columns
#otu_table have 3789 rows and 165 columns
#tax_table have 3789 rows and 7 columns

# Filter some taxa considered pollution ## this will remove the lines containing the taxa word regardless of taxonomic ranks 
dataset$filter_pollution(taxa = c("mitochondria", "chloroplast"))

#filtramos tambien los UNCLASSIFIED BACTERIA & UNCLASSIFIED ARCHAEA
dataset$filter_pollution(taxa = c("unclassified_Bacteria", "unclassified_Archaea"))

# Rarefaction by minimum reads - mirar el minim i posarlo a sample size
dataset$sample_sums() %>% range  #[1] 32270 73026
dataset$rarefy_samples(sample.size = 32270)
dataset$sample_sums() %>% range ##check the sequence numbers
dataset$save_table(dirpath = "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO", sep = ",")

# To make the OTU and sample information consistent across all files in the dataset object, we use function tidy_dataset to trim the dataset.
dataset$tidy_dataset
dataset
#microtable-class object:
#sample_table have 164 rows and 17 columns
#otu_table have 3639 rows and 164 columns
#tax_table have 3639 rows and 7 columns

#save the relative abundance at each taxonomic level
dataset$cal_abund()
head(dataset$taxa_abund$Phylum)
write.xlsx(dataset$taxa_abund$Phylum, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/RELABUND_PHYLUM2.xlsx", rowNames = TRUE)
write.xlsx(dataset$taxa_abund$Class, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/RELABUND_CLASS.xlsx", rowNames = TRUE)
write.xlsx(dataset$taxa_abund$Order, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/RELABUND_ORDER.xlsx", rowNames = TRUE)
write.xlsx(dataset$taxa_abund$Family, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/RELABUND_FAMILY.xlsx", rowNames = TRUE)
write.xlsx(dataset$taxa_abund$Genus, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/RELABUND_GENUS.xlsx", rowNames = TRUE)


##### MUESTRAS SUPERFICIALES

################# SUBSET OF SAMPLES
# remember first clone the whole group_TOP
dataset_TOP <- clone(dataset)
# select the group
dataset_TOP$sample_table <- subset(dataset_TOP$sample_table, profunditat == "sup")
#to trim all the basic files
dataset_TOP$tidy_dataset()
dataset_TOP
#sample_table have 90 rows and 17 columns
#otu_table have 3292 rows and 90 columns
#tax_table have 3292 rows and 7 columns

##### TAXONOMY 
library(ggh4x)

##### RELATIVE ABUNDANCE ASV BY SAMPLES
t1_TOP <- trans_abund$new(dataset = dataset_TOP, taxrank = "Phylum", ntaxa = 20)
str(t1_TOP)
t2_TOP <- trans_abund$new(dataset = dataset_TOP, taxrank = "Class", ntaxa = 20)
t3_TOP <- trans_abund$new(dataset = dataset_TOP, taxrank = "Order", ntaxa = 20)
t4_TOP <- trans_abund$new(dataset = dataset_TOP, taxrank = "Family", ntaxa = 20)
t5_TOP <- trans_abund$new(dataset = dataset_TOP, taxrank = "Genus", ntaxa = 90)

# PHYLUM
#RELATIVE ABUNDANCE PHYLUM GROUP_BAR PLOT
g1_TOP <- t1_TOP$plot_bar(others_color = "grey70", facet=c("tractament", "data"),legend_text_italic = FALSE)
g1_TOP
# Modify theme to rotate x-axis text by 45 degrees
g1_TOP <- g1_TOP +
  theme_classic() +
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis text by 45 degrees
  ggtitle("16S - ASV - Relative Abundance (%)")
g1_TOP
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/PHYLUM_REL_ABUND.jpg", plot = g1_TOP, width = 18, height = 10, units = "in")

# RELATIVE ABUNDANCE PHYLUM GROUP_BOXPLOT
g1.2_TOP <- t1_TOP$plot_box(group = "treatment", xtext_angle = 45)
g1.2_TOP + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Phylum - Relative Abundance (%)")
g1.2_TOP
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/BOXPLOT_PHYLUM_REL_ABUND.jpg", plot = g1.2_TOP, width = 8, height = 6, units = "in")

# RELATIVE ABUNDANCE PHYLUM GROUP_BOXPLOT_TRACTAMENT
g1.3_TOP <- t1_TOP$plot_box(group = "tractament", xtext_angle = 45)
g1.3_TOP + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Phylum - Relative Abundance (%)")
g1.3_TOP
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/BOXPLOT_PHYLUM_REL_ABUND_tract.jpg", plot = g1.3_TOP, width = 8, height = 6, units = "in")

##heatmap with the high abundant PHYLUM 
g1.4_TOP <- t1_TOP$plot_heatmap(facet = "treatment", xtext_keep = FALSE, withmargin = FALSE, plot_breaks = c(0.01, 0.1, 1, 10))
g1.4_TOP
g1.4_TOP + theme(axis.text.y = element_text(face = 'italic'))
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/HEATMAP_phylum_GROUP.jpg", plot = g1.4_TOP, width = 20, height = 6, units = "in")

## DONUT plot - PHYLUM relative abundance
t1.1_TOP <- trans_abund$new(dataset = dataset, taxrank = "Phylum", ntaxa = 10, groupmean = "tractament")
g1.5_TOP <- t1.1_TOP$plot_donut(label = TRUE)
# Customize the plot
g1.5_TOP <- g1.5_TOP + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Main Phylums")
g1.5_TOP
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/MAIN_PHYLUMS_donutplot.jpg", plot = g1.5_TOP,width = 8, height = 6, units = "in")

# FAMILY
#RELATIVE ABUNDANCE FAMILY GROUP_BAR PLOT
g4_TOP <- t4_TOP$plot_bar(others_color = "grey70", facet=c("tractament", "data"),legend_text_italic = FALSE)
g4_TOP
# Modify theme to rotate x-axis text by 45 degrees
g4_TOP <- g4_TOP +
  theme_classic() +
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis text by 45 degrees
  ggtitle("16S - ASV - Relative Abundance (%)")
g4_TOP
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/FAMILY_REL_ABUND.jpg", plot = g4_TOP, width = 18, height = 10, units = "in")

# RELATIVE ABUNDANCE FAMILY GROUP_BOXPLOT
g4.2_TOP <- t4_TOP$plot_box(group = "treatment", xtext_angle = 45)
g4.2_TOP + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Phylum - Relative Abundance (%)")
g4.2_TOP
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/BOXPLOT_FAMILY_REL_ABUND.jpg", plot = g4.2_TOP, width = 8, height = 6, units = "in")

# RELATIVE ABUNDANCE FAMILY GROUP_BOXPLOT_TRACTAMENT
g4.3_TOP <- t4_TOP$plot_box(group = "tractament", xtext_angle = 45)
g4.3_TOP + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Phylum - Relative Abundance (%)")
g4.3_TOP
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/BOXPLOT_FAMILY_REL_ABUND_tract.jpg", plot = g4.3_TOP, width = 8, height = 6, units = "in")

##heatmap with the high abundant FAMILY
g4.4_TOP <- t4_TOP$plot_heatmap(facet = "treatment", xtext_keep = FALSE, withmargin = FALSE, plot_breaks = c(0.01, 0.1, 1, 10))
g4.4_TOP
g4.4_TOP + theme(axis.text.y = element_text(face = 'italic'))
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/HEATMAP_family_GROUP.jpg", plot = g4.4_TOP, width = 20, height = 6, units = "in")


############# ALPHA DIVERSITY
#TO CALCULATE ALPHA DIVERSITY. If you want to add Faith's phylogenetic diversity, use PD = TRUE, this will be a little slow
dataset_TOP$cal_alphadiv(PD = FALSE)
##RESULTS
dataset_TOP$alpha_diversity

##segons tractament
a1 <- trans_alpha$new(dataset = dataset_TOP, group = "tractament")
a1$data_alpha
a1$data_stat   ##group statistics
# save dataset$alpha_diversity to a directory
write.xlsx(dataset_TOP$alpha_diversity, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPHA_DIVERSITY.xlsx", rowNames = FALSE)                                                                         
write.xlsx(a1$data_alpha, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPHA_DIVERSITY_v2.xlsx", rowNames = FALSE)                                                                         
write.xlsx(a1$data_stat, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPHA_DIVERSITY_group statistics.xlsx", rowNames = FALSE)                                                                         

#test the differences among groups using Kruskal-Wallis Rank Sum Test (overall test when groups > 2), Wilcoxon Rank Sum Tests (for paired groups), Dunn’s Kruskal-Wallis Multiple Comparisons (for paired groups when groups > 2) and anova with multiple comparisons
# Test differences by KRUSKAL-WALLIS & WILCOX by treatment
a1$cal_diff(method = "KW")
a1$res_diff
write.xlsx(a1$res_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPHA_DIVERSITY_KW_output.xlsx", rowNames = FALSE)

a1$cal_diff(method = "wilcox")
a1$res_diff
write.xlsx(a1$res_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPA_DIV_WILCOX_output.xlsx", rowNames = FALSE)

### ALPHA DIVERSITY PLOTS
# SHANNON INDEX - Perform post hoc test with the method: duncan.test
ga1 <- a1$plot_alpha(measure = "Shannon", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
ga1

# Customize the plot
ga1 <- ga1 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Shannon Index")
ga1
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/SHANNON_plot_Wilcox.jpg", plot = ga1, width = 8, height = 6, units = "in")

## INVERSED SIMPSON - Perform post hoc test with the method: duncan.test
ga1.1 <- a1$plot_alpha(measure = "InvSimpson", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
# Customize the plot
ga1.1 <- ga1.1 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Inversed Simpson Index")
ga1.1
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/INV_SIMPSON_plot.jpg", plot = ga1.1, width = 8, height = 6, units = "in")

## CHAO1 - Perform post hoc test with the method: duncan.test
ga1.3 <- a1$plot_alpha(measure = "Chao1", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
# Customize the plot
ga1.3 <- ga1.3 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Chao1 Index")
ga1.3
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/CHAO1_plot_wilcox.jpg", plot = ga1.3, width = 8, height = 6, units = "in")

## PIELOU - Perform post hoc test with the method: duncan.test
ga1.4 <- a1$plot_alpha(measure = "Pielou", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
# Customize the plot
ga1.4 <- ga1.4 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Pielou's Index")
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/PIELOU_plot_wilcox.jpg", plot = ga1.4, width = 8, height = 6, units = "in")



#######ALPHA DIVERSITY PLOTS_ TRACTAMENT*DATA
##segons tractament*data
a2 <- trans_alpha$new(dataset = dataset_TOP, group = "treatment")
a2$data_alpha
a2$data_stat   ##group statistics
# save dataset$alpha_diversity to a directory
write.xlsx(a2$data_stat, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPHA_DIVERSITY_group statistics_treat.xlsx", rowNames = FALSE)                                                                         

# Test differences by KRUSKAL-WALLIS & DUNN (there are too many groups for wilcox) by treatment*data
a2$cal_diff(method = "KW")
a2$res_diff
# Save the results to an Excel file
write.xlsx(a2$res_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPHA_DIVERSITY_KW_output_treatyear.xlsx", rowNames = FALSE)

a2$cal_diff(method = "KW_dunn", KW_dunn_letter = FALSE)
a2$res_diff
write.xlsx(a2$res_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ALPA_DIV_KW_DUNN_output_treatyear.xlsx", rowNames = FALSE)
#a2$cal_diff(method = "KW_dunn", KW_dunn_letter = TRUE)

##to remove the 'ns' in the label
a2$res_diff %<>% base::subset(Significance !="ns")

# SHANNON INDEX - Perform post hoc test with the method: duncan.test
ga2 <- a2$plot_alpha(measure = "Shannon", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
ga2

# Customize the plot
ga2 <- ga2 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Shannon Index")
ga2
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/SHANNON_plot_Wilcox_treatyear.jpg", plot = ga2, width = 16, height = 6, units = "in")

## INVERSED SIMPSON - Perform post hoc test with the method: duncan.test
ga2.1 <- a2$plot_alpha(measure = "InvSimpson", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
# Customize the plot
ga2.1 <- ga2.1 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Inversed Simpson Index")
ga2.1
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/INV_SIMPSON_plot_treatyear.jpg", plot = ga2.1, width = 16, height = 6, units = "in")

## CHAO1 - Perform post hoc test with the method: duncan.test
ga2.3 <- a2$plot_alpha(measure = "Chao1", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
# Customize the plot
ga2.3 <- ga2.3 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Chao1 Index")
ga2.3
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/CHAO1_plot_wilcox_treatyear.jpg", plot = ga2.3, width = 16, height = 6, units = "in")

## PIELOU - Perform post hoc test with the method: duncan.test
ga2.4 <- a2$plot_alpha(measure = "Pielou", y_increase = 0.25, add_sig_text_size = 4, boxplot_add = "jitter", order_x_mean = FALSE)
# Customize the plot
ga2.4 <- ga2.4 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 8)) + # Adjust the size of the legend text
  ggtitle("16S - Pielou's Index")
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/PIELOU_plot_wilcox_treatyear.jpg", plot = ga2.4, width = 16, height = 6, units = "in")


##### BETA DIVERSITY  #ordination using PCoA (principal coordinates analysis)
#dataset_beta <- clone(dataset)
#filter_microtable <- function(dataset_bet, min_abundance = 10, min_prevalence = 0.2) {
#  
#  # Apply minimum abundance threshold to OTU table
#  dataset$otu_table <- dataset$otu_table[rowSums(dataset$otu_table) > min_abundance, ]
#  
#  # Apply minimum prevalence threshold to OTU table
#  min_samples <- ncol(dataset$otu_table) * min_prevalence
#  dataset$otu_table <- dataset$otu_table[, colSums(dataset$otu_table > 0) >= min_samples]
#  
#  # Filter taxonomic table based on the filtered OTU table
#  dataset$tax_table <- dataset$tax_table[rownames(dataset$otu_table), ]
#  
#  return(dataset)
#}
#
#microtable <- filter_microtable(dataset_bet, min_abundance = 10, min_prevalence = 0.2)
#
## View the filtered microtable-class object
#print(microtable)
#
## return dataset$beta_diversity
#microtable$cal_betadiv(bray = TRUE)
#class(microtable$beta_diversity)
#
## save dataset$alpha_diversity to a directory
#write.xlsx(microtable$beta_diversity, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/BETA_DIVERSITY.xlsx", rowNames = FALSE)         

######################
#If method parameter is not provided, the function automatically calculates Bray-curtis, Jaccard, weighted Unifrac and unweighted unifrac matrixes (Lozupone and Knight 2005).
# unifrac = FALSE means do not calculate unifrac metric
# require GUniFrac package installed
dataset_TOP$cal_betadiv(unifrac = FALSE)
# return dataset$beta_diversity
class(dataset_TOP$beta_diversity)

###### Beta diversity calculation
b1 <- trans_beta$new(dataset = dataset_TOP, group = "tractament", measure = "bray")
# PCoA, PCA, DCA and NMDS are available
b1$cal_ordination(ordination = "PCoA")
class(b1$res_ordination)
# Save the results to an Excel file
write.xlsx(b1$res_ordination$scores, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/BETA_DIVERSITY_Calc.xlsx", rowNames = FALSE)
# plot the PCoA result with confidence ellipse
gb1 <- b1$plot_ordination(plot_color = "tractament", plot_shape = "data", point_size = 5, point_alpha = .2, plot_type = c("point", "ellipse"), ellipse_chull_fill = FALSE)
gb1
# Customize the plot #scale_color_manual(values = c("#4daf4a", "#ffae19", "mediumpurple1"))+
gb1 <- gb1 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
      legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("16S - Beta Diversity")
gb1
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/BetaDiversity_v2.jpg", plot = gb1, width = 8, height = 6, units = "in")

## PerMANOVA to the differential test of distances for all groups
library(stats)
#install.packages("remotes")
#remotes::install_github("vegandevs/vegan")

##PerMANOVA(Anderson 2001) can be applied to the differential test of distances among groups via the cal_manova function developed based on the adonis2 function of vegan package.
b1$cal_manova(manova_all = TRUE)
b1$res_manova
write.xlsx(b1$res_manova, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/PERMANOVA.xlsx", rowNames = FALSE)
##diferencias entre grupos
b1$cal_manova(manova_all = FALSE)
b1$res_manova
write.xlsx(b1$res_manova, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/PERMANOVA2.xlsx", rowNames = FALSE)

#From v1.0.0, ANOSIM method is also available.
# the group parameter is not necessary when it is provided in creating the object
b1$cal_anosim(group = "tractament", paired = TRUE)
b1$res_anosim
write.xlsx(b1$res_anosim, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ANOSIM_tractament.xlsx", rowNames = FALSE)


############## EXTRAS######
#PERMDISP(Anderson et al. 2011) is implemented to test multivariate homogeneity of groups dispersions (variances) based on the betadisper function of vegan package.
# for the whole comparison and for each paired groups
#b1$cal_betadisper()
#b1$res_betadisper

#### plot and compare the group distances
## Calculate and plot sample distances within groups
b1$cal_group_distance(within_group = TRUE)
# perform Wilcoxon Rank Sum and Signed Rank Tests
b1$cal_group_distance_diff(method = "wilcox")
# plot_group_order parameter can be used to adjust orders in x axis
gb1.2 <- b1$plot_group_distance(boxplot_add = "mean")
# Customize the plot
gb1.2 <- gb1.2 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("16S - Bray-Curtis Distances")
gb1.2
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/Bray-Curtis_distances.jpg", plot = gb1.2, width = 8, height = 6, units = "in")
# Save the results to an Excel file
write.xlsx(b1$res_group_distance_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/Bray-Curtis_distances.xlsx", rowNames = FALSE)

## Calculate and plot sample distances between groups
b1$cal_group_distance(within_group = FALSE)
b1$cal_group_distance_diff(method = "wilcox")
gb1.3 <- b1$plot_group_distance(boxplot_add = "mean")
# Customize the plot
gb1.3 <- gb1.3 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("16S - Bray-Curtis Distances")
gb1.3
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/Bray-Curtis_Grouped.jpg", plot = gb1.3, width = 8, height = 6, units = "in")
# Save the results to an Excel file
write.xlsx(b1$res_group_distance_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/Grouped_betadiv.xlsx", rowNames = FALSE)

###################################################


#######treatment*data
b2 <- trans_beta$new(dataset = dataset_TOP, group = "treatment", measure = "bray")
# PCoA, PCA, DCA and NMDS are available
b2$cal_ordination(ordination = "PCoA")
class(b2$res_ordination)
# Save the results to an Excel file
write.xlsx(b2$res_ordination$scores, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/BETA_DIVERSITY_Calc_treatyear.xlsx", rowNames = FALSE)

##PerMANOVA(Anderson 2001) can be applied to the differential test of distances among groups via the cal_manova function developed based on the adonis2 function of vegan package.
b2$cal_manova(manova_all = TRUE)
b2$res_manova
write.xlsx(b2$res_manova, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/PERMANOVA_TREATYEAR.xlsx", rowNames = FALSE)
##diferencias entre grupos
b2$cal_manova(manova_all = FALSE)
b2$res_manova
write.xlsx(b2$res_manova, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/PERMANOVA2_TREATYEAR.xlsx", rowNames = FALSE)

####when there are too many comparaison pairs, the by_group parameter can be used
b2$cal_manova(manova_set = "tractament + data")
b2$res_manova
b2$cal_manova(manova_all = FALSE, group = "data", by_group="tractament")
b2$res_manova

#From v1.0.0, ANOSIM method is also available.
# the group parameter is not necessary when it is provided in creating the object
b2$cal_anosim(group = "treatment", paired = TRUE)
b2$res_anosim
write.xlsx(b2$res_anosim, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ANOSIM_treatyear.xlsx", rowNames = FALSE)

######################## EXTRES ###############################
#### plot and compare the group distances
## Calculate and plot sample distances within groups
b2$cal_group_distance(within_group = TRUE)
# perform Wilcoxon Rank Sum and Signed Rank Tests
b2$cal_group_distance_diff(method = "KW_dunn")
b2$res_group_distance_diff
# plot_group_order parameter can be used to adjust orders in x axis
gb2.2 <- b2$plot_group_distance(boxplot_add = "mean")
# Customize the plot
gb2.2 <- gb2.2 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("16S - Bray-Curtis Distances")
gb2.2
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/Bray-Curtis_distances_treatyear.jpg", plot = gb2.2, width =14, height = 6, units = "in")
# Save the results to an Excel file
b2$cal_group_distance_diff(method = "KW_dunn", KW_dunn_letter = FALSE)
write.xlsx(b2$res_group_distance_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/Bray-Curtis_distances_treatyear.xlsx", rowNames = FALSE)

################

###  DIFFERENTIAL ABUNDANCE TEST (TRANS_DIFF CLASS 6.1)
#It can find significant taxa in determining community differences across groups.
#Currently, trans_diff class has multiple famous differential test approaches or wrapped methods to better capture the important biomarkers: Kruskal-Wallis Rank Sum Test (for groups > 2), Wilcoxon Rank Sum Tests (for each paired group),
#Dunn’s Kruskal-Wallis Multiple Comparisons (for paired group in cases groups > 2), t-test, ANOVA, Scheirer Ray Hare test, linear regression, metastat(White, Nagarajan, and Pop 2009), LEfSe(Segata et al. 2011), RF (random forest + differential test), metagenomeSeq(Paulson et al. 2013), DESeq2 (Love, Huber, and Anders 2014), ALDEx2 (Fernandes et al. 2014), ANCOM-BC2 (Lin and Peddada 2020), LinDA (Zhou et al. 2022), beta regression (Cribari-Neto and Zeileis 2010), linear mixed-effects model and generalized linear mixed model.

##run lefse analysis
#d1 <- trans_diff$new(dataset = dataset_TOP, method = "lefse", group = "tractament", alpha = 0.01, lefse_subgroup = NULL)
d1 <- trans_diff$new(dataset = dataset_TOP, method = "lefse", group = "tractament", alpha = 0.01,  p_adjust_method = "none", taxa_level = "Genus",  lefse_subgroup = NULL)
head(d1$res_abund)
d1$res_diff[1:5, c(1,3,4,6)]

# Threshold is used for the LDA score selection.
gd1 <- d1$plot_diff_bar(threshold = 1.8)
gd1
# Customize the plot
gd1 <- gd1 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("16S - LEfSe")
gd1

# Save the results 
write.xlsx(d1$res_abund, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LEfSe_Abund_table.xlsx", rowNames = FALSE)
write.xlsx(d1$res_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LEfSe_Test_Results.xlsx", rowNames = FALSE)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LefSe.jpg", plot = gd1, width = 8, height = 10, units = "in")

###otras opciones de graficas
gd1.1 <-d1$plot_diff_abund(use_number=1:30)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LefSe_v2.jpg", plot = gd1.1, width = 8, height = 10, units = "in")

###juntar les dues grafiques
gd1 <-d1$plot_diff_bar(threshold = 1.8)
gd1 <- d1$plot_diff_abund(select_taxa = d1$plot_diff_bar_taxa)
gd1.1
gd1 <- gd1 + theme(legend.position = "none")
gd1.1 <-gd1.1 + theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())
gridExtra::grid.arrange(gd1, gd1.1, ncol=2, nrow=1, widths=c(2, 1.7))

####AFEGIR SIGNIFICANCIA
###METHOD WILCOX
d2 <- trans_diff$new(dataset = dataset_TOP, method = "wilcox", group = "tractament", taxa_level = "Genus", filter_thres = 0.001)
d2$res_diff 
d2$res_diff %<>% subset (Significance %in% c( "***","**", "*"))
gd1.2 <-d2$plot_diff_abund(use_number = 1:40, add_sig = T, add_sig_label = "Significance")

gd1.2 <- gd1.2 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("16S - differential relative abundance - Wilcox")
gd1.2
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/differential relative abund_wilcox.jpg", plot = g16.2, width = 8, height = 10, units = "in")

##comparativa entre dos grups
#d2$res_diff$Comparison
#d2$plot_diff_abund(use_number = 1:40, select_group = "AWD - CONV", , add_sig = T, add_sig_label = "Significance")

##METHOD KW_DUNN
d2.2 <- trans_diff$new(dataset = dataset_TOP, method = "wilcox", group = "tractament", taxa_level = "Genus", filter_thres = 0.001)
d2.2$res_diff 
g16.3 <-d2.2$plot_diff_abund(use_number = 1:40, add_sig = T, coord_flip = F)
g16.3

###METHOD ANOVA (no funciona)
#d2.2 <- trans_diff$new(dataset = dataset_TOP, method = "anova", group = "tractament", taxa_level = "Genus", filter_thres = 0.001)
#d2.2$res_diff 
#g16.3 <-d2.2$plot_diff_abund(use_number = 1:5, add_sig = T, coord_flip = F)
#g16.3


#####TREATMENT*YEAR
d1.1 <- trans_diff$new(dataset = dataset_TOP, method = "lefse", group = "treatment", alpha = 0.01,  p_adjust_method = "none", taxa_level = "Genus",  lefse_subgroup = NULL)
head(d1.1$res_abund)
d1.1$res_diff[1:5, c(1,3,4,6)]

# Threshold is used for the LDA score selection.
g16.1 <- d1.1$plot_diff_bar(threshold = 3.3)
g16.1
# Customize the plot
g16.1 <- g16.1 + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.5, margin = margin(t = 0.4, unit = "pt")),  # Adjust margin
        plot.title = element_text(size = 14, margin = margin(b = 20)),
        legend.key.size = unit(1, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("16S - LEfSe")
g16.1

# Save the results 
#write.xlsx(d1$res_abund, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LEfSe_Abund_table_treatyear.xlsx", rowNames = FALSE)
#write.xlsx(d1$res_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LEfSe_Test_Results_treatyear.xlsx", rowNames = FALSE)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LefSe_treatyear.jpg", plot = g16.1, width = 8, height = 10, units = "in")

###altres opciones de grafiques
g16.1 <-d1$plot_diff_abund(use_number=1:30)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/LefSe_v2.jpg", plot = g16.1, width = 8, height = 10, units = "in")

#####################################################################

######################### ENVIRONMENTAL AND PHYSICAL-CHEMICAL PARAMETER
###

##add environmental data
e1 <-trans_env$new(dataset = dataset_TOP, env_cols = c("BACTOT", "ITS", "ITS_BACTOT", "AOB", "AOB_BACTOT", "mcrA", "mcrA_BACTOT") )

##cal_diff function is used to test the significance of variables across groups 
##wilcox
e1$cal_diff(group = "tractament", method = "wilcox")
e1$res_diff

##anova
e1$cal_diff(method = "anova", group = "tractament")
e1$res_diff
##place all the plots into a list
tmp <- list()
for (i in colnames(e1$data_env)) {
  tmp[[i]] <- e1$plot_diff(measure = i, add_sig_text = TRUE, add_sig_text_size = 3, xtext_size = 9) +
    theme(plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
          axis.title.y = element_text(size = 10),  # Change y-axis title size
          axis.text.y = element_text(size = 10))  # Change y-axis text size
}

# Plot the grid of plots with significance represented by letters and brackets   ####els BACTOT son signif i no els marca!
plot(gridExtra::arrangeGrob(grobs = tmp, ncol = 3, top = "16S - dbRDA - qPCR PARAMETERS", main = "Main Title"))
# Save the plot as an image file  ###no es guarda sol ##1000*1500

# Save the results to an Excel file
write.xlsx(e1$res_diff, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENVIRONMENTAL_qPCR_wilcox.xlsx", rowNames = FALSE)


#####VARIABLES AMB DOS FACTORS
e1$cal_diff(method = "anova", group = "tractament", by_group = "data")
e1$res_diff
ge1<-e1$plot_diff(measure="BACTOT", add_sig_text_size= 5)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_BACTOT.jpg", plot = ge1, width = 12, height = 8, units = "in")
ge2<-e1$plot_diff(measure="ITS", add_sig_text_size= 5)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_ITS.jpg", plot = ge2, width = 12, height = 8, units = "in")
ge3<-e1$plot_diff(measure="AOB", add_sig_text_size= 5)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_AOB.jpg", plot = ge3, width = 12, height = 8, units = "in")
ge4<-e1$plot_diff(measure="mcrA", add_sig_text_size= 5)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_mcrA.jpg", plot = ge4, width = 12, height = 8, units = "in")

ge5<-e1$plot_diff(measure="ITS_BACTOT", add_sig_text_size= 5)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_ITSBACTOT.jpg", plot = ge5, width = 12, height = 8, units = "in")
ge6<-e1$plot_diff(measure="AOB_BACTOT", add_sig_text_size= 5)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_AOBBACTOT.jpg", plot = ge6, width = 12, height = 8, units = "in")
ge7<-e1$plot_diff(measure="mcrA_BACTOT", add_sig_text_size= 5)
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_MCRABACTOT.jpg", plot = ge7, width = 12, height = 8, units = "in")

######AUTOCORRELATIONS AMONG VARIABLES
library(GGally)
gf<-e1$cal_autocor()
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_autocorrelat.jpg", plot = gf, width = 12, height = 8, units = "in")

gf1<-e1$cal_autocor(group="tractament")
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/ENV_autocorrelat_tract.jpg", plot = gf1, width = 12, height = 8, units = "in")

######
#####MANTEL TEST
library(vegan)
library(geosphere)
library("mice")
library(Matrix)
library(lme4)

##MANTEL TEST CAN BE USED  TO CHECK WHETHER THERE IS SIGNIFICANT CORRELATIONS BETWEEN ENVIRONMNETAL VARIABLES AND DISTANCE MATRIX
e1 <-trans_env$new(dataset = dataset_TOP, env_cols = c("BACTOT", "ITS", "ITS_BACTOT", "AOB", "AOB_BACTOT", "mcrA", "mcrA_BACTOT"), complete_na = TRUE )
e1$cal_mantel(use_measure = "bray",  na.rm=TRUE)
# return t1$res_mantel
head(e1$res_mantel)
# Save the results to an Excel file
write.xlsx(e1$res_mantel, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/MANTEL_TEST_ENV.xlsx", rowNames = FALSE)

##mantel test for different groups
e1$cal_mantel(by_group = "tractament", use_measure = "bray")
# return t1$res_mantel
head(e1$res_mantel)
# Save the results to an Excel file
write.xlsx(e1$res_mantel, "C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/MANTEL_TEST_ENV_bygroup.xlsx", rowNames = FALSE)

#####
##### dbRDA analysis

##use bray-curtis distance for dbRDA
e1$cal_ordination(method="dbRDA", use_measure = "bray")
#show the original results
e1$trans_ordination()
ge <- e1$plot_ordination(plot_color = "tractament", plot_shape = "data", plot_type = c("point", "ellipse"))
ge <- ge + 
  theme_classic() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0.9, hjust = 1, margin = margin(t = 6, r = 6)),  # Adjust the vertical position of x-axis labels
        plot.title = element_text(size = 12, margin = margin(b = 10)),  # Reduce bottom margin of the plot area
        legend.key.size = unit(1.5, "lines"),  # Adjust the size of the symbols in the legend
        legend.text = element_text(size = 10)) + # Adjust the size of the legend text
  ggtitle("RDA of qPCR parameters")
ge
# Save the plot as an image file
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/RDA_qPCR.jpg", plot = ge, width = 8, height = 6, units = "in")


###check the significance of the ordination model
e1$cal_ordination_anova()
e1$res_ordination_terms

##to get the contribution of each variable to the model
e1$cal_ordination_envfit()
e1$res_ordination_envfit

####RDA at the taxonomic level
e1$cal_ordination(method = "RDA", taxa_level = "Genus")
##select 10 features and adjust the arrow length
e1$trans_ordination(show_taxa = 20, adjust_arrow_length = TRUE, max_perc_env = 1.5, max_perc_tax = 1.5, min_perc_env = 0.2, min_perc_tax = 0.2)
ge2<-e1$plot_ordination(plot_color = "tractament", plot_shape = "data")
ge2
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/RDA_qPCR_genus.jpg", plot = ge2, width = 8, height = 6, units = "in")


#####CORRELATIONS BETWEEN ENVIRONMENTAL VARIABLES AND TAXA
e1$cal_cor(use_data = "Genus", p_adjust_method="fdr", p_adjust_type = "Env")
e1$res_cor
e1$plot_cor()  ###too many genera

####filter significant genera
e1$plot_cor(filter_feature = c("", "*", "**"))

###still too many genera. Correlation between environmental factors and important taxa detected in biomarker analysis
#first, create trans_diff object as a demostration
e2<-trans_diff$new(dataset= dataset_TOP, method="rf", group = "tractament", taxa_level = "Genus")
#then create trans_env object
e3<-trans_env$new(dataset = dataset_TOP, env_cols = c("BACTOT", "ITS", "ITS_BACTOT", "AOB", "AOB_BACTOT", "mcrA", "mcrA_BACTOT"), complete_na = TRUE )
#use other_taxa to select taxa needed
e3$cal_cor(use_data="other", p_adjust_method="fdr", other_taxa= e2$res_diff$Taxa[1:20])
ge3<-e3$plot_cor()
ge3
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/correlation_qPCR_genus.jpg", plot = ge3, width = 8, height = 6, units = "in")

##by groups
e3$cal_cor(by_group="tractament", use_data="other", p_adjust_method="fdr", other_taxa= e2$res_diff$Taxa[1:20])
ge4<-e3$plot_cor()
ge4
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/correlation_qPCR_genus_bygroup.jpg", plot = ge4, width = 8, height = 6, units = "in")

###################################################
######## FUNCIONAL PROFILES

###create object of trans_func
f1 <-trans_func$new(dataset_TOP)
f1$for_what <- 'prok'
#default database for prokariote
f1$cal_spe_func(prok_database="FAPROTAX")
###return results
f1$res_spe_func
###calculate percentatge of OTUs having the same trait for communities
##here do not consider the abundance
f1$cal_spe_func_perc(abundance_weighted = FALSE)
f1$res_spe_func_perc
f1$trans_spe_func_perc()
f1$plot_spe_func_perc()
gf<-f1$plot_spe_func_perc()
gf
ggsave("C:/Users/Mcarreras/OneDrive - IRTA/2023/R studio/FONS CLIMATIC/CERESTRES/BACTERIS/MICROECO/TOP/functional profile.jpg", plot = gf, width = 20, height = 6, units = "in")

###change the group list
f1$func_group_list
####detailed information of prokaryotic traits
f1$show_prok_func("methanogenesis")

####################### TAX4FUN
