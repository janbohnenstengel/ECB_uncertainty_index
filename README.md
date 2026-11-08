# ECB Uncertainty Index

A text-based measure of uncertainty in ECB monetary policy communication. A fine-tuned CentralBankRoBERTa model (based on Pfeifer & Marohl, 2023) classifies every sentence of the ECB's monetary policy statements as expressing uncertainty or not; the sentence scores are aggregated into one uncertainty index per Governing Council meeting. The index is then compared with dictionary-based measures and with other established uncertainty indicators.

## Approach

1. **Training data.** A sample of sentences from ECB monetary policy statements was labelled (uncertain / not uncertain) by an LLM, and the labels were spot-checked by humans (human-checked silver labels).
2. **Model.** A RoBERTa model is fine-tuned on these labels. The split into training, validation and test sets is random by statement, so that sentences from the same statement never appear in different sets; sentences repeated verbatim across statements are removed from validation and test.
3. **Index.** The fine-tuned model scores every sentence of every statement. The statement-level index is the mean predicted probability of uncertainty (`unc_mean_prob`), and a 5-meeting moving average of it.
4. **Comparisons.** The index is compared with:
   - a simple self-made uncertainty dictionary and the Loughran–McDonald dictionary (Loughran & McDonald, 2011), applied to the same statements
   - the Economic Policy Uncertainty index (Baker et al., 2016), averaged over Germany, France, Italy and Spain
   - the VSTOXX, the implied volatility index of the Euro Stoxx 50
   - disagreement among professional forecasters: the cross-sectional variance and standard deviation of one-year-ahead HICP inflation forecasts in the ECB Survey of Professional Forecasters (SPF)

## Matching ECB meetings to SPF rounds

Each ECB meeting is assigned to the **first SPF round whose response deadline falls strictly after the meeting**, i.e. the first survey in which forecasters could have reacted to that statement. A meeting is never matched to a deadline that has already passed. The SPF deadlines (1999Q1–2025Q4) are listed in `match_ecb_spf.R`; meetings after the last deadline remain unmatched.

## Repository structure

```
data/                  Input and intermediate data (not tracked, see below)
  RoBERTa/             Statement-level RoBERTa uncertainty index
  dictionary/          Dictionary-based indices
  EPU/                 Economic Policy Uncertainty data
  SPF/                 SPF individual forecasts and aggregated dispersion
  silver_labels/       Labelled sentences for fine-tuning
code/                  All necessary scripts (R and Python)
cbroberta_models/      Fine-tuned model checkpoints (folders tracked, weights not)
plots/                 Figures
```

## Pipeline

| Script | Purpose |
|---|---|
| `01_scrape_statements.R` | Scrapes ECB Monetary Policy Statements with Q&As; necessary adjustments described in script. |
| `01_scrape_accounts.R` | Scrapes ECB Monetary Policy Accounts; currently not used further; necessary adjustments described in script. |
| `02_text_cleaning.R` | Clean texts, split statements from Q&As, save all separately |
| `03_simple_dictionary_index.R` | Builds small dictionary and creates uncertainty indicator for statements based on it. |
| `03_LM_dictionary_index.R` | Creates uncertainty indicator for statements based on Loughran-McDonald dictionary. |
| `04_silver_labels_sample.R` | Splits texts into sentences, saves this and creates subsample for finetuning. |
| `05_ollama_create_silver_labels.R` | Uses a DeepSeek model via Ollama to leabel the silver label subsample. |
| 06_RoBERTa_finetune.py` | Finetunes RoBERTa for ECB language and uncertainty given silver labels. |
| `07_RoBERTa_label_all.py` | Uses fine-tuned RoBERTa to label all sentences. |
| `08_RoBERTa_aggregate_indicator.R` | Aggregates RoBERTa output to meeting level and creates indicator. |
| `09_compare.R` | Creates graphs comparing the indicators |
| `comparison_indices/SPF.R` | Cleans and aggregates SPF to desired format. |
| `comparison_indices/match_SPF_RoBERTa.R` | Matches quartely SPF rounds with ECB Council meetings since last survey deadline. |
| `comparison_indices/EPU_euro_area.R` | Creates EPU average over Europes big 4 economies. |


## Data

Data files and model weights are not included in the repository. To reproduce the results, place the following in `data/`:

- **ECB monetary policy statements** (ECB website)
- **SPF individual forecasts** (ECB Survey of Professional Forecasters, one `YYYYQ#.csv` file per round) in `data/SPF/SPF_individual_forecasts/`
- **Economic Policy Uncertainty index** for Europe (policyuncertainty.com) in `data/EPU/`
- **VSTOXX** historical prices as `data/VSTOXX_EUR_Historical.csv`


## Requirements

### R

Packages are loaded with [`librarian`](https://cran.r-project.org/package=librarian), which installs missing packages automatically. All scripts assume the `tidyverse` (including `dplyr`, `ggplot2`, `readr`, `stringr`, `purrr`, `lubridate`).

| Purpose | Packages |
|---|---|
| Core | `tidyverse`, `here`, `librarian` |
| Data import and handling | `readxl`, `haven`, `data.table`, `zoo`, `fuzzyjoin` |
| Web scraping and APIs | `rvest`, `RSelenium`, `httr`, `httr2`, `jsonlite`, `glue` |
| Text processing and dictionaries | `tm`, `quanteda`, `tidytext`, `tokenizers`, `SnowballC`, `Matrix` |
| Python interface | `reticulate` |
| Plot fonts | `showtext`, `sysfonts` |

```r
install.packages(c("librarian", "tidyverse", "here", "readxl", "haven", "data.table", "zoo",
                   "fuzzyjoin", "rvest", "RSelenium", "httr", "httr2", "jsonlite", "glue",
                   "tm", "quanteda", "tidytext", "tokenizers", "SnowballC", "Matrix",
                   "reticulate", "showtext", "sysfonts"))
```

`RSelenium` additionally needs Java and a browser driver to run.

### Python

Used for fine-tuning the model and scoring the statements.

```bash
pip install pandas numpy torch transformers datasets evaluate scikit-learn
```

All R paths are built with the `here` package relative to the project root, which is marked by a `.here` file. Scripts can therefore be run from any working directory inside the project.

## References
Loughran, Tim and McDonald, Bil (2011). When is a Liability not a Liability? Textual Analysis, Dictionaries, and 10-Ks, Journal of Finance, 66:1, 35-65. (Available at SSRN: http://ssrn.com/abstract=1331573.)

Pfeifer, Moritz and Marohl, Vincent P. (2023). CentralBankRoBERTa: A fine-tuned large language model for central bank communications, The Journal of Finance and Data Science, Volume 9, 100114, ISSN 2405-9188, https://doi.org/10.1016/j.jfds.2023.100114.

Baker, Scott R. and Bloom, Nicholas and Davis, Steven J. (2016). Measuring Economic Policy Uncertainty, The Quarterly Journal of Economics, Volume 131, Issue 4, November 2016, Pages 1593–1636, https://doi.org/10.1093/qje/qjw024
