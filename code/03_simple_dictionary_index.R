# Setup --------------------------------------------------------------------------------------------
rm(list=ls())

librarian::shelf(tidyverse,
                 data.table,
                 purrr,
                 stringr,
                 httr,
                 jsonlite,
                 lubridate,
                 readr,
                 tm,
                 Matrix,
                 SnowballC,
                 quanteda,
                 tidytext,
                 reticulate,
                 fuzzyjoin,
                 here)

# Load Data ----------------------------------------------------------------------------------------
statements <- read.csv(here("data", "clean_text", "clean_statements_Gerhard.csv"))

QandAs <- read.csv(here("data", "clean_text", "clean_QandAs_Gerhard.csv"))

accounts <- read.csv(here("data", "clean_text", "clean_accounts_Gerhard.csv"))


#  Preprocessing data ------------------------------------------------------------------------------

preprocess_text <- function(text) {
  text <- tolower(text)                  # lowercase
  text <- removePunctuation(text)        # remove punctuation
  words <- unlist(str_split(text, "\\s+"))            # split into words
  words <- words[!words %in% stopwords("en")]         # remove stopwords
  words <- wordStem(words)                             # stem words
  text <- paste(words, collapse = " ")
}

statements$processed_statement <- sapply(statements$clean_statement, preprocess_text)

QandAs$processed_qa <- sapply(QandAs$clean_qa, preprocess_text)

accounts$processed_account <- sapply(accounts$clean_account, preprocess_text)


# Dictionary ---------------------------------------------------------------------------------------
risk_dictionary <- c(
  "uncertain", "uncertainty", "risk", "risks", "volatile", "volatility",
  "concern", "concerns", "doubt", "doubts", "instability", "fragile",
  "turbulent", "unpredictable", "fluctuation", "fluctuations"
)

# Stemming the dictionary
risk_dictionary_stemmed <- wordStem(risk_dictionary) |>
    unique()


# Build index --------------------------------------------------------------------------------------
count_uncertainty_norm <- function(text, dictionary) {
  words <- unlist(str_split(text, "\\s+"))
  score <- sum(words %in% dictionary)
  return(score / length(words))  # normalized by text length
}

statements$statement_simple_dictionary_score <- sapply(statements$processed_statement,
                                                       count_uncertainty_norm,
                                                       dictionary = risk_dictionary_stemmed)

QandAs$QandA_simple_dictionary_score <- sapply(QandAs$processed_qa, count_uncertainty_norm,
                             dictionary = risk_dictionary_stemmed)

accounts$account_simple_dictionary_score <- sapply(accounts$processed_account, count_uncertainty_norm,
                                 dictionary = risk_dictionary_stemmed)

# Save data ----------------------------------------------------------------------------------------
statement_export <- statements |>
    select(id, date, president, statement_simple_dictionary_score)

write.csv(statement_export, here("data", "dictionary", "simple_dictionary_index_statements.csv"))

# Plot ---------------------------------------------------------------------------------------------

simple_plot <- statement_export |>
    ggplot(aes(x=as.Date(date), y=statement_simple_dictionary_score)) +
    geom_line() +
    labs(title = "ECB Statement Uncertainty Score (based on Self-Made Dictionary)",
         x = "",
         y = "Normalized word count") +
    scale_x_date(breaks = function(lims) seq(as.Date("2002-01-01"), lims[2], by = "5 years"),
                 date_minor_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.02))) +
    theme_bw()

windows()
simple_plot
ggsave(here("plots", "simple_dictionary_index_statements.png", simple_plot, bg = "white",
       dpi = 320, height = 5, width = 8))