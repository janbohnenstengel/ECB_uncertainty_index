# Cross-sectional dispersion of 1-year-ahead SPF inflation forecasts
# Output: var_long_target.rds (one row per survey quarter)


# Setup --------------------------------------------------------------------------------------------
librarian::shelf(here,
                 dplyr,
                 readr,
                 stringr,
                 purrr,
                 zoo,
                 sysfonts,
                 showtext)

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

spf_folder <- here("data", "SPF", "SPF_individual_forecasts")
out_file   <- here("data", "SPF", "var_long_target.rds")


# Helpers ------------------------------------------------------------------------------------------

# Survey quarter from the file name, e.g. "2023Q1.csv" -> 2023 Q1
survey_qtr_from_file <- function(path) {
  as.yearqtr(str_extract(basename(path), "\\d{4}Q[1-4]"), format = "%YQ%q")
}

# One-year-ahead (rolling horizon) target month of a survey round:
# Q1 -> Dec same year, Q2 -> Mar next year, Q3 -> Jun next year, Q4 -> Sep next year
target_month_for <- function(survey_qtr) {
  y <- as.integer(format(survey_qtr, "%Y"))
  q <- as.integer(format(survey_qtr, "%q"))
  year  <- ifelse(q == 1, y, y + 1)
  month <- c(12, 3, 6, 9)[q]
  as.Date(sprintf("%d-%02d-01", year, month))
}

# "2023Dec" -> 2023-12-01; anything without a year and a month name -> NA
parse_target_month <- function(x) {
  x   <- str_squish(as.character(x))
  yr  <- str_extract(x, "\\d{4}")
  mon <- match(tolower(str_extract(x, "(?i)(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)")),
               tolower(month.abb))
  ok  <- !is.na(yr) & !is.na(mon)
  as.Date(ifelse(ok, sprintf("%s-%02d-01", yr, mon), NA))
}

# Read the headline HICP inflation block (the first block) of one survey file
read_inflation_block <- function(path) {
  lines      <- read_lines(path, n_max = 80)
  header_row <- which(str_detect(lines, "TARGET_PERIOD"))[1]
  if (is.na(header_row)) stop("No TARGET_PERIOD header in ", basename(path))

  df <- read_csv(path, skip = header_row - 1, name_repair = "minimal",
                 col_types = cols(.default = col_character()))

  df <- df[, c("TARGET_PERIOD", "FCT_SOURCE", "POINT")]

  # The file continues with further blocks (core inflation, GDP, unemployment, ...).
  # Keep only the rows before the first of them.
  is_block_start <- df$TARGET_PERIOD == "TARGET_PERIOD" |
    str_detect(df$TARGET_PERIOD,
               "(?i)GROWTH|UNEMPLOYMENT|CORE|ASSUMPTIONS|EXPECTATIONS|CHANGE IN")
  end <- which(is_block_start)[1]
  if (!is.na(end) && end > 1) df <- df[seq_len(end - 1), ]

  df |>
    transmute(
      id         = FCT_SOURCE,
      survey_qtr = survey_qtr_from_file(path),
      target     = parse_target_month(TARGET_PERIOD),
      forecast   = as.numeric(POINT)
    )
}


# Individual 1-year-ahead forecasts ----------------------------------------------------------------

spf_files <- list.files(spf_folder, pattern = "^\\d{4}Q[1-4]\\.csv$", full.names = TRUE)
if (length(spf_files) == 0) stop("No YYYYQ#.csv files found in ", spf_folder)

spf_long <- map_dfr(spf_files, read_inflation_block) |>
  filter(!is.na(id), !is.na(forecast), target == target_month_for(survey_qtr)) |>
  select(-target) |>
  distinct(id, survey_qtr, .keep_all = TRUE)   # one forecast per forecaster and round


# Cross-sectional variance per survey round --------------------------------------------------------

var_long_target <- spf_long |>
  group_by(survey_qtr) |>
  summarise(
    n        = n(),
    variance = if (n >= 2) var(forecast) else NA_real_,
    sd       = if (n >= 2) sd(forecast) else NA_real_,
    .groups  = "drop"
  ) |>
  arrange(survey_qtr) |>
  mutate(target_month = target_month_for(survey_qtr))

saveRDS(var_long_target, out_file)
write_csv(var_long_target, here("data", "SPF", "SPF_aggregate_quarterly.csv"))


# Plot ---------------------------------------------------------------------------------------------

SPF_plot <- var_long_target |>
    ggplot(aes(x = survey_qtr)) +
    geom_line(aes(y = variance), alpha = 0.6) +
    labs(title = "Disagreement among Professional Forecasters for Inflation 1 Year Ahead",
         y = "Cross Sectional Variance",
         x = "Quarters") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size=60),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 38),
          axis.title.x = element_text(size = 44))

ggsave(here("plots", "SPF.png"), SPF_plot, bg = "white", dpi = 320, height = 5, width = 8)