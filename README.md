# Thesis_Gasera
GHG emission rate calculation out of Gasera samplings

1. R Project: Thesis_Gasera
2. /src: R scripts.
3. /data: data inputs (e.g. .csv, .xlsx)
4. /outputs: analysis outputs (e.g., plots, .csv, etc.)

## /src: R scripts
Here the main scripts used for data preparation, plotting and statistical analyses are described. Other scripts can be found in this folder containing coding tests. 

### Main: GHG emission calculation
- Field sampling data in "data/CERESTRES_Gasera_results". All output ".meas" files from Gasera field samplings should be stored here.
- Gasera files nomenclature:
  - Digits/Letters 1 - 3: Experiment (e.g. CER for "CERESTRES")
  - Digits/Letters 4 - 6: Plot (e.g. P01)
  - Digits/Letters 7 - 8: Chamber type (i.e. TR for transparent chambers, DK for dark ones)
  - Digits/Letters 9: _
  - Digits/Letters 10 - onwards: Date, time and gasera code (e.g. 2023-06-20_10-10-49_030077)
  - Example: CERP01DK_2023-06-20_10-10-49_030077
- for in loops for CH4, N2O and CO2 emission calculations.
- Accumulated emissions during all the growing season.

### Gasera_vs_Chrom: Water level calculations and emission comparisson among methods
- Inputs:
  a. Emission_rates_w_corrections_2023.RData: Chromatography GHG rates. Previously calculated in Rproject Thesis_Chromatography.
  b. Gasera_emission_rates_2023.RData: Gasera GHG rates. Previously calculated in Main script.
- Comparisson plots.
