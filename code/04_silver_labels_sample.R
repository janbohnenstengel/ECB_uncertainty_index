# Setup --------------------------------------------------------------------------------------------
rm(list=ls())

librarian::shelf(tidyverse,
                 stringr,
                 tokenizers,
                 readr,
                 purrr,
                 httr2,
                 jsonlite,
                 here)

# Load Data ----------------------------------------------------------------------------------------
df <- read.csv2(here("data", "clean_text", "clean_statements_Gerhard.csv"), sep = ",")

## Fix clustered sentences
fix_missing_space <- function(x) {
  x |>
    # 1) Simple case: ".A" -> ". A" (also for ! ?)
    str_replace_all("([.!?])([A-ZÄÖÜ])", "\\1 \\2") |>
    # 2) Cases like '."A' or '.)A' or '.”A' -> add space after punctuation
    str_replace_all("([.!?])([\"'”’\\)\\]\\}])([A-ZÄÖÜ])", "\\1\\2 \\3") |>
    # 3) Also handle cases like ".\nA" -> ". A"
    str_replace_all("([.!?])\\s*\\n\\s*([A-ZÄÖÜ])", "\\1 \\2")
}

df$clean_statement <- fix_missing_space(df$clean_statement)

## Create corpus - Split in sentences
corpus <- df |>
  mutate(sentence = tokenize_sentences(clean_statement)) |>
  unnest(sentence) |>
  mutate(
    sentence = str_trim(sentence),
    nchar = nchar(sentence)) |>
  filter(nchar >= 40) |> # remove very short boilerplate fragments
  mutate(statement_id = id,
         sentence_id = row_number(),
         year = year(date)) |>
  select(-c(id, clean_statement))

sum(corpus$nchar>400)
which(corpus$nchar>400)

write.csv(corpus, file = here("data", "clean_text", "statement_sentences.csv"), sep = ",")


# Sample Sentences to Label ------------------------------------------------------------------------

set.seed(20260126)

corpus <- corpus |>
  mutate(year = year(as.Date(date)))

table(corpus$year)

N <- 5600

# allocate roughly equally across years (robust to changing statement style over time)
years <- sort(unique(corpus$year))
n_per_year <- floor(N / length(years))

samp <- corpus |>
  group_by(year) |>
  slice_sample(n = n_per_year) |>
  ungroup()

samp <- samp |>
  distinct(sentence, .keep_all = TRUE)
# keep only what you need for labeling, plus provenance
label_sample <- samp |>
  select(statement_id, sentence_id, date, year, president, sentence) |>
  mutate(prompt_version = "unc_v1", run_seed = 20260126)

# Check
any(duplicated(samp$sentence))
table(samp$year) |> summary()

# saveRDS(label_sample, "data/label_sample_5000.rds")
write.csv(label_sample, here("data", "RoBERTa", "silver_labels", "label_sample_5000.csv"), row.names = FALSE)
