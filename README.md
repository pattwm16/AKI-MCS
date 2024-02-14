# AKI-MCS: Acute kidney injury in mechanical circulatory support – does renal replacement therapy make a difference?

## Investigators

- **Dr. Sascha Ott**, Consultant for Cardiac Intensive Care Medicine at the German Heart Center Berlin
- **Will Patterson**, Medical Student (4th year) at the Cleveland Clinic

This repo will hold the analysis files for the Charité – Universitätsmedizin Berlin ECPELLA observational analysis.

## Scripts

- [clean.R](scripts/clean.R): Cleans, preprocesses, and reshapes raw data for downstream analysis.
- [data_with_icu_los.R](scripts/data_with_icu_los.R): Creates a modified version of the original dataset, enriched with descriptive labels.
- [table1.R](tbls/table1.R): Constructs table 1.
- [table2.R](tbls/table2.R): Constructs table 2.
- [matching.R](scripts/matching.R): TBD.
- [weighting.R](scripts/weighting.R): TBD.
- [primary_hypothesis1.R](scripts/primary_hypothesis1.R): Runs the logistic regression for the primary aim, with diagnostics.
- [secondary_hypothesis1.R](scripts/secondary_hypothesis1.R): Runs the logistic regression for the first secondary aim, with diagnostics.
- [secondary_hypothesis2.R](scripts/secondary_hypothesis2.R): Runs the logistic regression for the second secondary aim, with diagnostics.

## How to Run the Analysis

There is a wrapper script for ease of reproducibility. First, ensure your working directory is set to the parent folder (`aki-mcs`). Running `compile.R` will first run the cleaning scripts, then construct descriptive tables of the data, and finally will run the primary and secondary analyses.

Results and diagnostic plots for the regression analyses are found in [regs](regs). Each regression has its own diagnostic folder with plots demonstrating each assumption. Regression tables are also avaliable for each regression, one folder level up.