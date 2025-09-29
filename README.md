# Climate change mitigation through irrigation strategies during rice growing season is off-set in fallow season
Open access article: [Journal of Environmental Management](https://doi.org/10.1016/j.jenvman.2025.125060).

Contact: secheverriap@gmail.com. Please do not hesitate to contact in case you have any doubts using this material.

## Index
1. R Project: Thesis_Gasera
2. /src: R scripts.
3. /data: data inputs (e.g., .csv, .xlsx). All data colected across the sampling campaign 2023.
4. /outputs: analysis outputs (e.g., plots, .csv, etc.)

## /src: R scripts
Description of the main scripts used for data preparation, plotting and statistical analyses: 

### Main_w_corrections_2023: Calculating CH<sub>4</sub> emission rates from sampled GHG concentration through gas chromatography. Rates calculated from complete (all 4 timesteps - vials) and alternative (drop-one-timestep) models.
- Creating "Chrom_w_corrections_2023" dataframe, which calculates GHG concentrations in mg m<sup>-2</sup> from C-ppm chromatography results.
- Calculating emission rates through lm(), also R<sup>2</sup> for each lm() to apply posterior model corrections.
- Fitting 4 alternative "drop-one-timestep" models (each one removing one timestep) and then selecting that which achieves higher R<sup>2</sup> and positive rate (lm slope).
- outputs:
  - Emission_rates_w_corrections_2023 dataframe: Chromatography GHG rates for all GHG (CH<sub>4</sub>, N<sub>2</sub>O and CO<sub>2</sub>). Rates (flux), R<sup>2</sup> and p-vaues for all fitted models (complete and alternative. E.g.: Column Chrom_N2O_flux_Alt4 shows all N<sub>2</sub>O rates from alternative model 4, calculated removing T3 (concentration of vial collected at time 30 minutes). Finally, selected model calculations are shown in columns:
    - "Chrom_GHG_model": Selected model, that with higher R<sup>2</sup> above defined threshold (0.7).
    - "Chrom_GHG_flux_corrected": Flux from the selected model.
    - "Chrom_R2_GHG_corrected": R<sup>2</sup> from the selected model.
    - "Chrom_Logic_GHG": Logic for selecting a particular model. In case none of them surpasses the defined R<sup>2</sup> threshold, or if they do but the rate is negative, then flux is assumed to be 0.
  - Diagnostic plots: Pdf file with concentrations in time for each of the fitted models and for all GHG (e.g., N2O_Plots_w_corrections_2023.pdf).

### Data_prep:
- Inputs:
  - Emission_rates_w_corrections_2023 dataframe: Chromatography GHG rates. Previously calculated in Main_w_corrections_2023 script.

  
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
