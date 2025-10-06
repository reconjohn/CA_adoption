# Code for: "Rooftop solar, electric vehicle, and heat pump adoption in rural areas in the United States."

The following includes descriptions of data, code, and functions used for data analyses and visualizations for the paper titled, "Rooftop solar, electric vehicle, and heat pump adoption in rural areas in the United States."

Please cite as: Min, Yohan & Mayfield, Erin. Rooftop solar, electric vehicle, and heat pump adoption in rural areas in the United States. Energy Research & Social Science 2023;105:103292. https://doi.org/10.1016/j.erss.2023.103292.


## File Descriptions

`Official_function.R`: This file contains the necessary libraries, data, and functions required for the analysis.

`official_data.R`: This script executes the code for generating data for figures and conducting analyses.

`official_code.R`: This script executes the code for generating figures and conducting analyses.

`official_data.Rdata`: This data file includes Residential Energy Consumption Survey (RECS) data at the household level, state-level data, energy burden data, Republican vote rates, and boundary information for mapping.

`vermont.Rdata`: This data file contains Vermont-specific data at the town level for the year 2020.

`RECS.Rdata`: This data file contains estimated adoption rate values based on other predictors for urban and rural areas at the household level.

`A_comparison.Rdata`: This data file contains estimated adoption rate values based on the adoption of other technologies for urban and rural areas at the household level.

## Function Descriptions

`sim_countf`: This function computes the adoption rate difference of a technology between scenarios when other technologies are in effect and when they are not in Vermont.
- Input: Vermont data at the town level and the name of the technology.
- Output: Estimated adoption rate difference between scenarios where the technology is in effect or not, with a 95% confidence interval.

`sim_plot`: This function visualizes the results from `sim_countf`.
- Input: Output of `sim_countf` and the name of the technology.
- Output: A figure that visualizes the effect of the adoption of other technologies on the adoption of the specified technology.

`b_countf`: This function computes the adoption rate of a technology based on various energy burden values in Vermont.
- Input: Vermont data at the town level and the name of the technology.
- Output: Estimated adoption rate of the technology for various energy burden values, with a 95% confidence interval.

`r_countf`: This function computes the adoption rate of a technology based on various Republican vote rate values in Vermont.
- Input: Vermont data at the town level and the name of the technology.
- Output: Estimated adoption rate of the technology for various Republican vote rate values, with a 95% confidence interval.

`s_countf`: This function computes the adoption rate difference of a technology between scenarios when other predictors are in effect and when they are not in Vermont.
- Input: Vermont data at the town level and the name of the technology.
- Output: Estimated adoption rate difference between scenarios where other predictors are in effect or not, with a 95% confidence interval.

`s_plot`: This function visualizes the results from `s_countf`.
- Input: Output of `s_countf` and the name of the technology.
- Output: A figure that visualizes the effect of other predictors on the adoption of the specified technology.

`countf`: This function computes the adoption rate of a technology based on various values of the predictor in Vermont.
- Input: Vermont data at the town level, the name of the technology, and the name of a predictor.
- Output: Estimated adoption rate of the technology for various values of the predictor, with a 95% confidence interval.