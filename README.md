# Climate change mitigation through irrigation strategies during rice growing season is off-set in fallow season
Open access article: [Journal of Environmental Management]([https://doi.org/10.1016/j.agee.2025.109719](https://doi.org/10.1016/j.jenvman.2025.125060)).

Contact: secheverriap@gmail.com (do not hesitate to contact in case you have any doubts using this material).

1. R Project: Thesis_Gasera
2. /src: R scripts.
3. /data: data inputs (e.g. .csv, .xlsx)
4. /outputs: analysis outputs (e.g., plots, .csv, etc.)

## /src: R scripts
Here the main scripts used for data preparation, plotting and statistical analyses are described. Other scripts can be found in this folder containing coding tests. 

### Main: GHG emission calculation from Gasera samplings
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

### Data_prep: Creating Master_GHG_2023 dataframe with emissions from Chromatography and Gasera, physiochemichal data and Water level calculations
- Inputs:
  a. Emission_rates_w_corrections_2023.RData: Chromatography GHG rates. Previously calculated in Rproject Thesis_Chromatography.
  b. Gasera_emission_rates_2023.RData: Gasera GHG rates. Previously calculated in Main script.
  
### Data_visual: Plotting data from Data_prep

### Data_stats: Data analysis
1. Research question and hypotheses
2. Data exploration according to: A protocol for data exploration to avoid common statistical problems - Zuur et al., 2010.
- 2.1. Check for outliers.
- 2.2. Independence of explanatory variables: collinearity and variance inflation.
    - 2.2.1. Correlation plot
    - 2.2.2. Variance Inflation
    - 2.2.3. Dendrogram with independent variables (covariates)
- 2.3. Relationships between Y and X variables?
 3. Statistical Modeling
- 3.1. Initial model and link functio
