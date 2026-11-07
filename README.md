# Economic Burden of Pneumonia in Sub-Saharan Africa: Cost-of-Illness Analysis and Economic Evaluation Studies

R code, data, figures and tables behind the systematic review. Every number, table and figure produced by the analysis comes from one input file, `data/costs_input.csv`, which holds the cost figures extracted from the included studies, plus two World Bank indicator files used to standardise costs to 2025 US dollars.

## What is in each folder

| Folder | What it holds | Open it with |
|---|---|---|
| `R/` | The three R scripts, numbered in the order they run (`01` to `03`). | R or RStudio |
| `data/` | `costs_input.csv` (extracted costs, in the currency and price year reported by each study) and `worldbank/` (the World Bank consumer price index and official exchange rate files, as downloaded). | Excel or R |
| `figures/` | The figures as images. `Figure1` to `Figure9` are the figure numbers in the manuscript. | Any image viewer |
| `tables/` | The results as CSV files: `costs_2025USD.csv` (every cost standardised to 2025 US dollars) and `Table_S0` to `Table_S7` (the numbers behind the manuscript tables and the sensitivity analysis), plus `analysis_sets_used.csv` (the costs the distributions were fitted to). | Excel |
| `r_results/` | The plain text R printed while each script ran, in the order R computed it. Useful for checking a number against the console output. | Any text editor |
| `working_files/` | Created when the scripts run (unpacked World Bank files). It can be ignored and is not stored in the repository. | R only |

## Which file gives which result

| Manuscript item | File |
|---|---|
| Figure 1, PRISMA 2020 flow diagram | `figures/Figure1_PRISMA_flow.png` (also `.pdf`), drawn by `R/03_prisma_diagram.R` |
| Table 4, cost per episode by setting and perspective | `tables/Table_S1_by_setting_perspective.csv` |
| Figures 2 and 3, cost by country and by setting | `figures/Figure2_cost_by_country.png`, `figures/Figure3_boxplot_setting_perspective.png`, with `tables/Table_S2_by_country.csv` |
| Table 6, Figures 4 to 6, distribution fits | `tables/Table_S3_distribution_fits.csv`, `figures/Figure4_fit_outpatient.png`, `figures/Figure5_fit_inpatient.png`, `figures/Figure6_QQ_plots.png` |
| Figure 7, credibility estimates | `tables/Table_S4_credibility.csv`, `figures/Figure7_credibility.png` |
| Table 7, Figure 8, annual aggregate provider cost (gamma base case) | `tables/Table_S5_aggregate_loss.csv`, `figures/Figure8_aggregate_loss.png` |
| Table 7, lognormal severity sensitivity rows | `tables/Table_S5b_severity_sensitivity.csv` |
| Figure 9, scenarios | `tables/Table_S6_scenarios.csv`, `figures/Figure9_scenarios.png` |
| Section 3.8, sensitivity to the conversion method | `tables/Table_S7_conversion_sensitivity.csv` |

## How to run

1. Install R (version 4.3.3 was used; other recent versions should also work). Only base R and the MASS package (installed with R) are needed.
2. Open R with this folder as the working directory (in RStudio: Session > Set Working Directory > Choose Directory, then pick this folder; `getwd()` should print its path).
3. Run `source("run_all.R")`, or `Rscript run_all.R` in a terminal. Every output is written into `figures/`, `tables/` and `r_results/`. The full run takes about a minute.

The random numbers in the aggregate model use a fixed seed, so the simulated results repeat exactly.

## Scripts, in run order

| Script | What it does | Printed output (`r_results/`) |
|---|---|---|
| `01_convert_to_2025_USD.R` | Converts every extracted cost to 2025 US dollars by local inflation then exchange rate, with a United States inflation alternative, and flags rows where assumptions were needed | `01_convert_to_2025_USD.txt` |
| `02_summary_and_actuarial_models.R` | Summary tables, figures, fits of the exponential, gamma and lognormal distributions by maximum likelihood, Buhlmann credibility, the compound Poisson aggregate model with value at risk and tail value at risk (gamma base case, repeated with the lognormal), and the scenarios | `02_summary_and_actuarial_models.txt` |
| `03_prisma_diagram.R` | The PRISMA 2020 flow diagram (the counts are typed in the script) | `03_prisma_diagram.txt` |
