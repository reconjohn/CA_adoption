# Project: Policy levers, adoption gaps, and grid impacts in California for equitable energy transition

This repository contains the R scripts for a comprehensive analysis of technology adoption in California. The project models adoption patterns by validating survey data, analyzing the impacts of subsidies and peer effects, examining spatial distributions, and identifying key drivers using machine learning. The final output synthesizes these findings into predictive "what-if" scenarios.

---

## Project Workflow

The analysis is structured to be run in a logical sequence. Scripts build upon each other, starting with setup and data exploration, moving to specific model components, and concluding with synthesis and feature analysis.

**Typical workflow:**

1. `Function.R`: Run first to load libraries, data, and helper functions.  
2. `function_test.R`: Run to explore and understand the raw data.  
3. `MRP.R`: Run to validate the survey data against MRP estimates.  
4. `ML.R`: Run to identify the relative importance of the variables using machine learning algorithms.  
5. `subsidy.R`, `peer_effect.R`, `spatial.R`: Core analysis scripts modeling individual factors (subsidies, social influence, geography).  
6. `scenario.R`: Run to build predictive scenarios.


---

## Script Descriptions

### 1. `Function.R`

**Purpose:** Central setup and utility script.

- Foundational script for the entire project; must be sourced at the beginning of any session.
- Loads all required R packages (e.g., `dplyr`, `ggplot2`, `sf`).
- Loads raw or pre-processed data (`.csv`, `.rds`, `.shp`) into the global environment.
- Defines custom helper functions for data cleaning, plotting, and modeling.

---

### 2. `function_test.R`

**Purpose:** Exploratory Data Analysis (EDA)

- A sandbox for initial data investigation.
- Contains data summaries, statistical tests, and visualizations.
- Checks data distributions, missing values, and outliers.
- Helps form initial hypotheses for formal testing.

---

### 3. `MRP.R`

**Purpose:** Data validation with the estimates from Multilevel Regression & Poststratification (MRP)

- Validates the representativeness of the survey data.
- Compares MRP-generated means with raw survey sample means.

---

### 4. `ML.R`

**Purpose:** Machine learning analysis for feature importance

- Performs machine learning to identify key adoption drivers.
- Builds predictive models (e.g., GLM, Lasso, and Gradient Boosting).

---


### 5. `subsidy.R`

**Purpose:** Subsidy impact analysis

- Quantifies the effect of subsidies on technology adoption.
- Measure impacts of different subsidy levels by income level or Disadvantaged Community (DAC) assignment.

---

### 6. `peer_effect.R`

**Purpose:** Social influence analysis

- Models the dynamic nature of peer effects on adoption.
- Tracks how social influence differ by urbanization, DAC, and technology.

---

### 7. `spatial.R`

**Purpose:** Spatial analysis of patterns

- Maps geographic distribution of adoption rates and model results.
- Uses spatial statistics (e.g., Moran’s I) to test for autocorrelation.
- Identifies adoption "hot spots" and links findings to specific locations.


---

### 8. `scenario.R`

**Purpose:** Predictive scenario building

- Combines impacts of subsidies, peer effects, and other drivers.
- Simulates "what-if" scenarios for policy planning (e.g., future adoption under increased subsidies).