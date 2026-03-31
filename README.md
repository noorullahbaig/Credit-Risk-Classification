# Credit Risk Classification

## Overview

Credit Risk Classification is an R-based analysis project focused on separating good and bad credit outcomes from demographic and financial attributes. The repository centers on a single analysis script that combines data cleaning, exploratory charts, statistical testing, and logistic regression.

## Problem

The analysis investigates how attributes such as personal status, gender, and employment duration relate to credit classification outcomes.

## Approach

- Load the credit dataset in R
- Clean and recast selected variables as factors
- Produce descriptive charts for the main attributes under study
- Apply statistical tests such as chi-square and proportion tests
- Fit logistic-regression models for interpretation
- Summarize results in the accompanying PDF report

## Repo Structure

- `code.R` - main analysis workflow
- `Analysis.pdf` - written report / project output

## How to Run or Reproduce

1. Install the R packages referenced in `code.R`.
2. Update the CSV path at the top of `code.R` so it points to your local copy of the dataset.
3. Run the script in RStudio or with:

```bash
Rscript code.R
```

## Limitations

- The dataset CSV is not committed to this repository.
- `code.R` currently uses an absolute local file path and needs to be adjusted before running on another machine.
