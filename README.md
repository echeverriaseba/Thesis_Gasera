# Climate change mitigation through irrigation strategies during rice growing season is off-set in fallow season
Open access article: [Journal of Environmental Management](https://doi.org/10.1016/j.jenvman.2025.125060).

Contact: secheverriap@gmail.com (do not hesitate to contact in case you have any doubts using this material).

## Index
1. R Project: Thesis_Gasera
2. /src: R scripts.
3. /data: data inputs (e.g. .csv, .xlsx). All data colected across the sampling campaign 2023.
4. /outputs: analysis outputs (e.g., plots, .csv, etc.)

## /src: R scripts
Description of the main scripts used for data preparation, plotting and statistical analyses: 

### Main_w_corrections_2023: Calculating original CH<sub>4</sub> emission rates from sampled GHG concentration through gas chromatography.
- Creating "Chrom_w_corrections_2023" dataframe, which calculates GHG concentrations in mg m<sup>-2</sup> from C-ppm chromatography results.
- Calculating emission rates through lm(), also R<sup>2</sup> for each lm() to apply posterior model corrections.
- Fitting 4 alternative "3-values" models (each one removing one time-step) and then selecting that which achieves higher R<sup>2</sup> and positive rate (lm slope).

### Data_prep:
- Inputs:
  a. Emission_rates_w_corrections_2023 dataframe: Chromatography GHG rates. Previously calculated in Main_w_corrections_2023 script.

  
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
