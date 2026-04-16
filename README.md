# Project: Policy levers, adoption gaps, and grid planning in California for equitable energy transition

This repository contains the R scripts for a comprehensive analysis of technology adoption in California. The project models adoption patterns by validating survey data using Multilevel Regression with Post-stratification (MRP), analyzing the marginal effects of time-varying drivers like social influence and infrastructure improvements, and identifying key drivers using machine learning.

---

## Project Workflow

The analysis is structured to be run in a logical sequence. Scripts build upon each other, starting with setup and data exploration, moving to specific model components, and concluding with synthesis and feature analysis.

**Typical workflow:**


1. `Function.R`: Run first. Initializes the environment and processes raw data. 
2. `TEST.R`: Data validation and exploratory analysis.
3. `ML.R`: Summarizing data, feature importance and machine learning model execution. 
4. `State_mean.R:`: State-wide baseline analysis.
5. `Sub_effect.R`, `Spatial_effect.R`: Detailed analysis of specific drivers and geographic distributions, including MRP results.  


---

## Script Descriptions

### 1. `Function.R`

**Purpose:** Central setup, library management, and data processing.

- Foundational script for the entire project; must be sourced at the beginning of any session.
- Loads all required R packages (e.g., `dplyr`, `ggplot2`, `sf`).
- Loads raw or pre-processed data (`.csv`, `.rds`, `.shp`) into the global environment.
- Defines custom helper functions for data cleaning, plotting, and modeling.

---

### 2. `TEST.R`

**Purpose:** Exploratory Data Analysis (EDA) and Quality Control.

- A sandbox for initial data investigation.
- Contains data summaries, statistical tests, and visualizations.
- Checks data distributions, missing values, and outliers.
- Performs initial correlation analysis (Pearson’s $r$) to identify preliminary relationships between technology classes (PS, EV, HP, IC).
- Helps form initial hypotheses for formal testing.


---

### 3. `ML.R`

**Purpose:** Machine learning analysis for feature importance

- Summarizes the dataset for predictive modeling.
- Performs machine learning to identify key adoption drivers.
- Builds predictive models (e.g., GLM, Lasso, and Gradient Boosting).

---


### 4. `State_mean.R`

**Purpose:** State-wide Average Analysis

- Calculates state-level adoption baselines in waterfall formats.
- Measure impacts of different levels by Disadvantaged Community (DAC) assignment.

---

### 5. `Sub_effect.R`

**Purpose:** Specific Variable & Marginal Effect Analysis.

- Analyzes the marginal effects of specific time-varying drivers.
- Focuses on the impact of social contagion (peer effects) and infrastructure improvements (e.g., EV range anxiety) on adoption propensity.
- Handles the decomposition of aggregated effects (referencing SI Figure S13).


---

### 6. `Spatial_effect.R`

**Purpose:** Spatial analysis of patterns

- Generates spatial projections of adoption probabilities.
- Produces multi-panel maps comparing DAC vs. Non-DAC disparities (e.g., Gini Coefficients and Standard Deviation).
- Visualizes adoption "hubs" and spatial autocorrelation patterns across the study domain.



---
