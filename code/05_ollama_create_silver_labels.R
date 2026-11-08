# Setup --------------------------------------------------------------------------------------------
librarian::shelf(httr,
    jsonlite,
    glue,
    haven,
    dplyr,
    stringr,
    lubridate,
    httr,
    readr,
    quiet = TRUE
)

### NOTE!
### Adjust ollama_uncertainty function when rerunning on own computer!
###

# Load Data ----------------------------------------------------------------------------------------
sent_df <- read_csv(here("data", "RoBERTa", "silver_labels", "label_sample_5000.csv")) |>
    mutate(date = as.Date(date))


# Set up LLM ---------------------------------------------------------------------------------------
system_role <- glue("You are classifying sentences from European Central Bank communication.
Return only one digit: 0 or 1.
Label 1 if the sentence expresses uncertainty, risk, conditionality, or lack of commitment.
This includes hedging or conditional language such as may, might, could, depends, risks,
uncertain outlook, subject to, data-dependent, or similar formulations.
If the sentence does NOT express uncertainty, label it 0.
Do not provide any explanation or additional text.
")

# ollama uncertainty
ollama_uncertainty <- function(sentence, model = "deepseek-r1") {
    body <- list(
        prompt = paste(system_role, "Sentence:", sentence, "Label:"),
        max_tokens = 5,
        temperature = 0, # for reproducibility
        stream = FALSE,
        model = model
    )

    resp <- POST(
        url = "http://localhost:11434/api/generate",
        body = body,
        encode = "json",
        timeout(120)
    )

    txt <- content(resp)$response |> str_trim()
    lab <- str_extract(txt, "[01]")
    as.integer(lab)
}


# Label Sentences and aggregate by meeting ---------------------------------------------------------
sent_df <- sent_df |>
    mutate(uncertain = NA_integer_) # ensure column exists (overwrite if already present)

for (i in seq_len(nrow(sent_df))) {
    sent_df$uncertain[i] <- ollama_uncertainty(sent_df$sentence[i], model = "deepseek-r1")

    if (i == 1) cat("Progress:\n")
    if (i %% 20 == 0) cat(round(i / nrow(sent_df) * 100), "%\n")
}

# daily uncertainty index
daily_uncertainty <- sent_df |>
    group_by(date, president) |>
    summarise(
        n_sentences = n(),
        uncertainty_share = mean(uncertain, na.rm = TRUE),
        .groups = "drop"
    )


# Save Outputs -------------------------------------------------------------------------------------
write_csv(sent_df,
          here("data", "RoBERTa", "silver_labels", "uncertainty_statements_sentence_sample.csv"))
write_csv(daily_uncertainty,
          here("data", "RoBERTa", "silver_labels", "uncertainty_statements_daily_sample.csv"))