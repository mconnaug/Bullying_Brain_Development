# Longitudinal Structural MRI Analysis: Statistical Scripts

## Overview

This repository contains the R script used for the statistical analysis in the study titled **"The Impact of Bullying on Brain Development Across Adolescence and Early Adulthood: A Longitudinal Structural MRI Analysis."** The study investigates the effects of bullying on brain development during critical periods of growth using longitudinal structural MRI data.

## Contents

- **LME_Model_Metrics.R**: This R script performs various statistical analyses, including data cleaning, modeling, and visualization, to assess the impact of bullying on brain development. It includes:
  - Loading necessary libraries.
  - Reading and preprocessing the dataset.
  - Running Linear Mixed-Effects (LME) models.
  - Analyzing model outputs.
  - Generating summary tables and visualizations.

## Prerequisites

This script relies on several R packages, which need to be installed before running the script. You can install them using the following command:

```r
install.packages(c("gamm4", "mediation", "tidyverse", "lmerTest", "knitr", "lavaan", "psych", "MBESS", "greybox", "lme4", "haven", "dplyr", "ggplot2", "ggseg", "effects", "rstatix", "broom", "ggpubr", "effectsize", "ggeffects", "plyr", "methods", "stargazer", "sjPlot", "nlme", "data.table", "MuMIn", "lmtest", "officer", "readxl", "RColorBrewer", "dvmisc", "qwraps2", "AICcmodavg", "simr", "emmeans", "stats", "mutoss", "powerlmm"))
```

## Data

Raw Data Available:

https://github.com/imagen2/imagen_mri

The dataset used in this study is expected to be loaded from an external file (e.g., `IMAGEN_MASTER.xlsx`). Ensure that the dataset is correctly formatted and located in the appropriate directory as specified in the script.

## Usage

1. **Prepare your environment**: Make sure all required libraries are installed.
2. **Load the data**: Adjust the file paths in the script to point to your local dataset.
3. **Run the script**: Execute the script in R or RStudio. The script will:
   - Load and preprocess the data.
   - Fit the Linear Mixed-Effects models.
   - Generate and save the statistical outputs and visualizations.

## Results

The script generates various outputs, including:
- **Model summaries**: Detailed statistics for the fitted models.
- **Plots**: Visual representations of the results, such as effect sizes and predicted values.

## Contact

For any questions or issues, please contact [Your Name] at [Your Email].

---

You can modify the contact information or any other specific details as necessary. This `README.md` provides a clear guide for users on how to use the script and understand its purpose.I'll help you create a `README.md` file based on the contents of the script you provided. I'll first read the script to understand its purpose and then draft a suitable README.

Let me start by reviewing the script.

Based on the content of the script, here's a draft for the `README.md` file:

---

# Longitudinal Structural MRI Analysis: Statistical Scripts

## Overview

This repository contains the R script used for the statistical analysis in the study titled **"The Impact of Bullying on Brain Development Across Adolescence and Early Adulthood: A Longitudinal Structural MRI Analysis."** The study investigates the effects of bullying on brain development during critical periods of growth using longitudinal structural MRI data.

## Contents

- **LME_Model_Metrics.R**: This R script performs various statistical analyses, including data cleaning, modeling, and visualization, to assess the impact of bullying on brain development. It includes:
  - Loading necessary libraries.
  - Reading and preprocessing the dataset.
  - Running Linear Mixed-Effects (LME) models.
  - Analyzing model outputs.
  - Generating summary tables and visualizations.

## Prerequisites

This script relies on several R packages, which need to be installed before running the script. You can install them using the following command:

```r
install.packages(c("gamm4", "mediation", "tidyverse", "lmerTest", "knitr", "lavaan", "psych", "MBESS", "greybox", "lme4", "haven", "dplyr", "ggplot2", "ggseg", "effects", "rstatix", "broom", "ggpubr", "effectsize", "ggeffects", "plyr", "methods", "stargazer", "sjPlot", "nlme", "data.table", "MuMIn", "lmtest", "officer", "readxl", "RColorBrewer", "dvmisc", "qwraps2", "AICcmodavg", "simr", "emmeans", "stats", "mutoss", "powerlmm"))
```

## Data

The dataset used in this study is expected to be loaded from an external file (e.g., `IMAGEN_MASTER.xlsx`). Ensure that the dataset is correctly formatted and located in the appropriate directory as specified in the script.

## Usage

1. **Prepare your environment**: Make sure all required libraries are installed.
2. **Load the data**: Adjust the file paths in the script to point to your local dataset.
3. **Run the script**: Execute the script in R or RStudio. The script will:
   - Load and preprocess the data.
   - Fit the Linear Mixed-Effects models.
   - Generate and save the statistical outputs and visualizations.

## Results

The script generates various outputs, including:
- **Model summaries**: Detailed statistics for the fitted models.
- **Plots**: Visual representations of the results, such as effect sizes and predicted values.

## Contact

For any questions or issues, please contact Michael Connaughton at michaelconnaughton@rcsi.com.

