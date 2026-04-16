# Project: Policy levers, adoption gaps, and grid planning in California for equitable energy transition

This repository contains the R scripts for a comprehensive analysis of technology adoption in California. The project models adoption patterns by validating survey data, identifying key drivers using machine learning, analyzing the marginal effects of time-varying drivers like social influence and technology improvements, and predicting future adoption at the tract level, using Multilevel Regression with Post-stratification (MRP).

---

## Project Workflow

The analysis is structured to be run in a logical sequence. Scripts build upon each other, starting with setup and data exploration, moving to specific model components, and concluding with synthesis and analysis.

**Typical workflow:**

0. `Prep.R`: Processes raw data. 
1. `TEST.R`: Data validation including missing and correlation, and exploratory analysis.
2. `Function.R` and `Data.R`: Run first. Initializes the environment and processed data. 
3. `ML.R`: Feature importance and machine learning model execution. 
4. `State_mean.R:`: State-wide baseline analysis.
5. `Sub_effect.R`: Detailed analysis of specific drivers. 
6. `Spatial_effect.R`: Geographic distributions, spatial correlations, including MRP results.  


---

## Script Descriptions

### 1. `Prep.R` and `TEST.R`

**Purpose:** Processing raw data with exploratory data analysis and quality control.

- A sandbox for initial data investigation.
- Contains data summaries, statistical tests, and visualizations.
- Checks data distributions, missing values, and outliers.
- Performs initial correlation analysis (Pearson’s $r$) to identify preliminary relationships among variables.
- Helps form initial hypotheses for formal testing.


---


### 2. `Function.R` and `Data.R`

**Purpose:** Central setup, library management, and data processing.

- Foundational script for the entire project; must be sourced at the beginning of any session.
- Loads all required R packages (e.g., `dplyr`, `ggplot2`, `sf`).
- Loads raw or pre-processed data (`.csv`, `.rds`, `.shp`) into the global environment.
- Defines custom helper functions for data cleaning, plotting, and modeling.

---


### 3. `ML.R`

**Purpose:** Machine learning analysis for feature importance

- Performs machine learning to identify key adoption drivers.
- Builds predictive models (e.g., GLM, Lasso, and Gradient Boosting).

---


### 4. `State_mean.R`

**Purpose:** State-wide average analysis

- Calculates state-level adoption baselines in waterfall formats.
- Measure impacts of different levels by Disadvantaged Community (DAC) assignment.

---

### 5. `Sub_effect.R`

**Purpose:** Specific variable & marginal effect analysis.

- Analyzes the marginal effects of specific time-varying drivers.
- Focuses on the impact of social contagion (peer effects) and technology improvements (e.g., EV range anxiety) on adoption propensity.
- Handles the decomposition of aggregated effects.


---

### 6. `Spatial_effect.R`

**Purpose:** Spatial analysis of patterns

- Generates spatial projections of adoption probabilities.
- Produces multi-panel maps comparing DAC vs. Non-DAC disparities (e.g., Gini coefficients and standard deviation).
- Visualizes adoption "hubs" and spatial autocorrelation patterns across the study domain.



project-root/
├── .gitignore
├── README.md
├── data/
│   └── data.RData
├── syntax/
│   ├── Data.R
│   ├── file1.R
│   ├── file2.R
│   ├── ...
│   └── other_scripts.R
└── (all other files and folders ignored by .gitignore)
