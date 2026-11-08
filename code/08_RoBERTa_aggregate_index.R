# Setup --------------------------------------------------------------------------------------------
rm(list=ls())

librarian::shelf(tidyverse,
                 stringr,
                 tokenizers,
                 readr,
                 purrr,
                 httr2,
                 jsonlite,
                 lubridate,
                 here,
                 showtext,
                 sysfonts)

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()


# Import Sentence-Level Labeled Data ---------------------------------------------------------------
df <- readr::read_csv(here("data", "RoBERTa", "all_sentences_labeled_RoBERTa.csv"),
                      show_col_types = FALSE)

df <- df |>
     mutate(date = as_date(date))

# Aggregate to Meeting Level -----------------------------------------------------------------------

statement_idx <- df |>
  mutate(n_tokens = stringr::str_count(sentence, "\\S+")) |>
  group_by(statement_id) |>   # <-- change to meeting_id or date as needed
  summarise(
    date = first(as_date(date)),      # optional
    year = as.numeric(year(date)),
    n_sent = n(),
    tokens = sum(n_tokens, na.rm = TRUE),
    unc_share_50 = mean(uncertain_prob >= 0.50, na.rm = TRUE),
    unc_share_60 = mean(uncertain_prob >= 0.60, na.rm = TRUE),
    unc_share_70 = mean(uncertain_prob >= 0.70, na.rm = TRUE),
    unc_mean_prob = mean(uncertain_prob, na.rm = TRUE),
    unc_token_share = sum(n_tokens * (uncertain_pred == 1), na.rm = TRUE) / tokens,
    .groups = "drop"
  )

statement_idx <- statement_idx |>
  arrange(date) |>
  mutate(
    unc_mean_prob_ma3 = zoo::rollmean(unc_mean_prob, 3, fill = NA, align = "right"),
    unc_mean_prob_ma5 = zoo::rollmean(unc_mean_prob, 5, fill = NA, align = "right")
  )

write.csv(statement_idx, here("data", "RoBERTa", "aggregate_RoBERTa_uncertainy_index.csv"))

# Plots --------------------------------------------------------------------------------------------
## Absolute label plot -----------------------------------------------------------------------------
plot_label_index <- statement_idx |>
    ggplot(aes(x = date, y = unc_share_70)) +
    geom_line() +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0,0.5)) +
    labs(title = "Index from Absolute Labels (RoBERTa)") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size=60),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 38),
          axis.title.x = element_blank())

ggsave(here("plots", "RoBERTa_abs_statements_over_time.png"), plot_label_index, bg = "white",
       dpi = 320, height = 5, width = 8)
# windows()
# plot_label_index

## Probability Plot --------------------------------------------------------------------------------
plot_prob_index <- statement_idx |>
    ggplot(aes(x = date, y = unc_mean_prob)) +
    geom_line() +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0,0.5)) +
    labs(title = "Index from Probabilities (RoBERTa)") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size=60),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 38),
          axis.title.x = element_blank())

ggsave(here("plots", "RoBERTa_prob_statements_over_time.png"), plot_prob_index, bg = "white",
       dpi = 320, height = 5, width = 8)

## Smoothened-Prop Plot
plot_prob_index_smooth <- statement_idx |>
    ggplot(aes(x = date, y = unc_mean_prob_ma5)) +
    geom_line() +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0,3)) +
    labs(title = "Index from Smoothened Probabilities (RoBERTa)") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size=60),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 38),
          axis.title.x = element_blank())

ggsave(here("plots", "RoBERTa_smooth_prob_statements_over_time.png"), plot_prob_index_smooth,
       bg = "white", dpi = 320, height = 5, width = 8)

cor(statement_idx$unc_mean_prob, statement_idx$unc_mean_prob_ma5, use="complete.obs")