# Setup --------------------------------------------------------------------------------------------
rm(list = ls())

librarian::shelf(tidyverse,
                 lubridate,
                 tidytext,
                 showtext,
                 sysfonts,
                 here)

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# Load Data ----------------------------------------------------------------------------------------
RoBERTa_index <- read.csv2(here("data", "RoBERTa", "aggregate_RoBERTa_uncertainy_index.csv"),
                           sep = ",")

# RoBERTa_index_2 <- read.csv2(here("data", "RoBERTa", "aggregate_RoBERTa_uncertainy_index_2.csv"),
#                                   sep = ",")

dictionary_index_basic <- read.csv2(here("data", "dictionary",
                                    "simple_dictionary_index_statements.csv"), sep = ",")

dictionary_index_loughran <- read.csv2(here("data", "dictionary",
                                            "LM_dictionary_index_statements.csv"), sep = ",") |>
    select(-X)

EPU <- read.csv2(here("data", "EPU", "EPU_DE_FR_IT_ES.csv"), sep = ",")

SPF <- read.csv2(here("data", "SPF", "SPF_aggregate_quarterly.csv"))

RoBERTa_vs_SPF <- read_csv(here("data", "SPF", "RoBERTa_SPF_matched.csv"), show_col_types = FALSE)

VSTOXX <- read.csv2(here("data", "VSTOXX_EUR_Historical.csv"), sep =",") |>
    select(c(Date, Price)) |>
    rename(vstoxx_price = Price,
           date = Date) |>
    mutate(date = mdy(date),
           vstoxx_price = as.numeric(vstoxx_price),
           year_month = format(date, "%Y-%m"))

# VSTOXX plot --------------------------------------------------------------------------------------
VSTOXX_plot <- VSTOXX |>
    ggplot(aes(x = as.Date(date))) +
    geom_line(aes(y = vstoxx_price), alpha = 0.6) +
    labs(title = "VSTOXX: Historical Chart",
         y = "VSTOXX") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size = 50),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 38),
          axis.title.x = element_blank())

ggsave(here("plots", "VSTOXX.png"), VSTOXX_plot, bg = "white", dpi = 320, height = 5, width = 8)

# Comparisons --------------------------------------------------------------------------------------

## RoBERTa - Simple Dictionary ---------------------------------------------------------------------
RoBERTa_simpleDictionary <- left_join(RoBERTa_index, dictionary_index_basic, by = "date") |>
    select(c(date, president, unc_mean_prob, unc_mean_prob_ma5, statement_simple_dictionary_score)) |>
    mutate(unc_mean_prob = as.numeric(unc_mean_prob),
           unc_mean_prob_ma5 = as.numeric(unc_mean_prob_ma5),
           statement_simple_dictionary_score = as.numeric(statement_simple_dictionary_score))

cor(RoBERTa_simpleDictionary$unc_mean_prob, RoBERTa_simpleDictionary$statement_simple_dictionary_score)

cor.test(RoBERTa_simpleDictionary$unc_mean_prob, RoBERTa_simpleDictionary$statement_simple_dictionary_score,
         alternative = "two.sided", method = "Pearson", exact = TRUE)

## RoBERTa - Advanced Dictionary -------------------------------------------------------------------
RoBERTa_advancedDictionary <- left_join(RoBERTa_index, dictionary_index_loughran, by = "date") |>
    select(c(date, president, unc_mean_prob, unc_mean_prob_ma5, statement_dictionary_score)) |>
    mutate(unc_mean_prob = as.numeric(unc_mean_prob),
           unc_mean_prob_ma5 = as.numeric(unc_mean_prob_ma5),
           statement_dictionary_score = as.numeric(statement_dictionary_score),
           unc_mean_prob_z = scale(unc_mean_prob)[,1],
           statement_dictionary_score_z = scale(statement_dictionary_score)[,1])
        
cor(RoBERTa_advancedDictionary$unc_mean_prob, RoBERTa_advancedDictionary$statement_dictionary_score)

cor.test(RoBERTa_advancedDictionary$unc_mean_prob, RoBERTa_advancedDictionary$statement_dictionary_score,
         alternative = "two.sided", method = "pearson", exact = TRUE)

RoBERTa_Dict_plot <- RoBERTa_advancedDictionary |>
    ggplot(aes(x = as.Date(date))) +
    geom_line(aes(y = unc_mean_prob_z, color = "Mean RoBERTa Probability")) +
    geom_line(aes(y = statement_dictionary_score_z, color = "Loughran-McDonald Dictionary Score"),
              alpha = 0.6) +
    scale_color_manual(values = c("Mean RoBERTa Probability" = "red",
                                  "Loughran-McDonald Dictionary Score" = "darkgreen")) +
    labs(title = "RoBERTa vs. Loughran-McDonald Dictionary Uncertainty Indicators",
         y = "Standad Deviations from Mean (z-Score)",
         color = "Indicator") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size = 55),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 34),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 44),
          legend.text = element_text(size = 34),
          legend.position = "bottom")
  
ggsave(here("plots", "comparisons", "RoBERTa_vs_Dictionary.png"), RoBERTa_Dict_plot, bg = "white",
       dpi = 320, height = 5, width = 8)

## RoBERTa - RoBERTa2 ------------------------------------------------------------------------------
# comp_roberta <- left_join(RoBERTa_index, RoBERTa_index_2, by = c("statement_id", "date", "year")) |>
#     mutate(unc_mean_prob.x = as.numeric(unc_mean_prob.x),
#            unc_mean_prob.y = as.numeric(unc_mean_prob.y))

# cor(comp_roberta$unc_mean_prob.x, comp_roberta$unc_mean_prob.y)

# RoBERTa_RoBERTa2_plot <- comp_roberta |>
#     ggplot(aes(x = as.Date(date))) +
#     geom_line(aes(y = unc_mean_prob.x, color = "RoBERTa (Prompt 1)")) +
#     geom_line(aes(y = unc_mean_prob.y, color = "RoBERTa (Prompt 2)")) +
#     scale_color_manual(values = c("RoBERTa (Prompt 1)" = "orange", "RoBERTa (Prompt 2)" = "firebrick")) +
#     labs(title = "RoBERTa Probabilities with Different Prompts",
#          y = "Probability of Resembling Uncertainty",
#          color = "Index") +
#     theme_bw(base_family = "Roboto Condensed") +
#     theme(plot.title = element_text(size = 55),
#           axis.title.y = element_text(size = 44),
#           axis.text = element_text(size = 34),
#           axis.title.x = element_blank(),
#           legend.title = element_text(size = 44),
#           legend.text = element_text(size = 34),
#           legend.position = "bottom")
  
# ggsave(here("plots", "comparisons", "RoBERTa_vs_RoBERTa2.png"), RoBERTa_RoBERTa2_plot,
#        bg = "white", dpi = 320, height = 5, width = 8)

## RoBERTa - EPU -----------------------------------------------------------------------------------
RoBERTa_index <- RoBERTa_index |>
    mutate(year_month = format(as.Date(date), "%Y-%m")) |>
    filter(year >= 2000)

EPU <- EPU |>
    mutate(year_month = format(as.Date(date), "%Y-%m"))

statement_dates <- RoBERTa_index$year_month

EPU <- EPU |>
    filter(year_month %in% statement_dates)

RoBERTa_EPU <- left_join(RoBERTa_index, EPU, by = "year_month") |>
    select(c(date.x, date.y, year_month, euro_epu, unc_mean_prob, unc_mean_prob_ma5)) |>
    mutate(unc_mean_prob = as.numeric(unc_mean_prob),
           unc_mean_prob_ma5 = as.numeric(unc_mean_prob_ma5),
           euro_epu = as.numeric(euro_epu),
           unc_mean_prob_ma5_z = scale(unc_mean_prob_ma5)[,1],
           euro_epu_z = scale(euro_epu)[,1])

cor(RoBERTa_EPU$euro_epu, RoBERTa_EPU$unc_mean_prob)
cor.test(RoBERTa_EPU$euro_epu, RoBERTa_EPU$unc_mean_prob,
         alternative = "two.sided", method = "pearson", exact = TRUE)

RoBERTa_EPU_plot <- RoBERTa_EPU |>
    ggplot(aes(x = as.Date(paste0(year_month, "-01")))) +
    geom_line(aes(y = euro_epu_z, color = "EPU")) +
    geom_line(aes(y = unc_mean_prob_ma5_z, color = "RoBERTa Probabilities (MA 5)")) +
    scale_color_manual(values = c("EPU" = "blueviolet", "RoBERTa Probabilities (MA 5)" = "red")) +
    labs(title = "RoBERTa MA(5) Probabilities vs. Economic Policy Uncertainty Index\n(Baker et al., 2016)",
         y = "Standard Deviations from Mean (z-Scores)",
         color = "Indicator") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size = 55, lineheight = 0.35),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 34),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 44),
          legend.text = element_text(size = 34),
          legend.position = "bottom")
  
ggsave(here("plots", "comparisons", "RoBERTa_vs_EPU.png"), RoBERTa_EPU_plot, bg = "white",
       dpi = 320, height = 5, width = 8)

## RoBERTa - VSTOXX --------------------------------------------------------------------------------
# Reload RoBERTa_index from the top!
RoBERTa_index <- RoBERTa_index |>
    mutate(year_month = format(as.Date(date), "%Y-%m"))

statement_dates <- unique(RoBERTa_index$year_month)

VSTOXX <- VSTOXX |>
    filter(year_month %in% statement_dates)

RoBERTa_vs_VSTOXX <- left_join(RoBERTa_index, VSTOXX, by = "year_month") |>
    relocate(c(year_month, unc_mean_prob, vstoxx_price), .before = X) |>
    filter(complete.cases(vstoxx_price)) |>
    mutate(unc_mean_prob = as.numeric(unc_mean_prob),
           unc_mean_prob_ma3 = as.numeric(unc_mean_prob_ma3),
           unc_mean_prob_ma5 = as.numeric(unc_mean_prob_ma5),
           vstoxx_price = as.numeric(vstoxx_price),
           unc_mean_prob_z = scale(unc_mean_prob)[,1],
           unc_mean_prob_ma5_z = scale(unc_mean_prob_ma5)[,1],
           vstoxx_price_z = scale(vstoxx_price)[,1])

RoBERTa_VSTOXX_plot <- RoBERTa_vs_VSTOXX |>
    ggplot(aes(x = as.Date(paste0(year_month, "-01")))) +
    geom_line(aes(y = unc_mean_prob_ma5_z, color = "RoBERTa Probabilities MA(5)")) +
    geom_line(aes(y = vstoxx_price_z, color = "VSTOXX Price (monthly)"), alpha = 0.6) +
    scale_color_manual(values = c("RoBERTa Probabilities MA(5)" = "red", "VSTOXX Price (monthly)" = "skyblue3")) +
    labs(title = "RoBERTa MA(5) Probabilities vs. VSTOXX",
         y = "Standard Deviations (z-Score)",
         color = "Indicator") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size = 55, lineheight = 0.35),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 34),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 44),
          legend.text = element_text(size = 34),
          legend.position = "bottom")

ggsave(here("plots" , "comparisons", "RoBERTa_vs_VSTOXX.png"), RoBERTa_VSTOXX_plot, bg = "white", dpi = 320,
       height = 5, width = 8)
    
cor(RoBERTa_vs_VSTOXX$unc_mean_prob, RoBERTa_vs_VSTOXX$vstoxx_price)

model2 <- lm(vstoxx_price ~ unc_mean_prob,
             data = RoBERTa_vs_VSTOXX)
summary(model2)

## Advanced Dictionary - VSTOXX --------------------------------------------------------------------
# load dictionary_index_loughran again from above
dictionary_index_loughran <- dictionary_index_loughran |>
    mutate(year_month = format(as.Date(date), "%Y-%m"))

statement_dates <- unique(dictionary_index_loughran$year_month)

VSTOXX <- VSTOXX |>
    filter(year_month %in% statement_dates)

advDict_vs_VSTOXX <- left_join(dictionary_index_loughran, VSTOXX, by = "year_month") |>
    relocate(c(year_month, statement_dictionary_score, vstoxx_price), .before = id) |>
    filter(complete.cases(vstoxx_price)) |>
    mutate(statement_dictionary_score = as.numeric(statement_dictionary_score),
           vstoxx_price = as.numeric(vstoxx_price),
           statement_dictionary_score_z = scale(statement_dictionary_score)[,1],
           vstoxx_price_z = scale(vstoxx_price)[,1])

advDict_VSTOXX_plot <- advDict_vs_VSTOXX |>
    ggplot(aes(x = as.Date(paste0(year_month, "-01")))) +
    geom_line(aes(y = statement_dictionary_score_z, color = "Dictionary Score"), alpha = 0.6) +
    geom_line(aes(y = vstoxx_price_z, color = "VSTOXX Price (monthly)")) +
    scale_color_manual(values = c("Dictionary Score" = "darkgreen", "VSTOXX Price (monthly)" = "navy")) +
    labs(title = "Dictionary Score vs. VSTOXX",
         y = "Standard Deviations (z-Score)",
         color = "Index") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size = 55, lineheight = 0.35),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 34),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 44),
          legend.text = element_text(size = 34),
          legend.position = "bottom")

ggsave(here("plots", "comparisons", "AdvDictionary_vs_VSTOXX.png"), advDict_VSTOXX_plot,
       bg = "white", dpi = 320, height = 5, width = 8)

cor(advDict_vs_VSTOXX$statement_dictionary_score, advDict_vs_VSTOXX$vstoxx_price)

## RoBERTa vs. SPF ---------------------------------------------------------------------------------
RoBERTa_vs_SPF_plot <- RoBERTa_vs_SPF |>
    ggplot(aes(x = as.Date(spf_deadline))) +
    geom_line(aes(y = sd, color = "SPF Standard Deviations"), alpha = 0.6) +
    geom_line(aes(y = u, color = "RoBERTa Probabilities")) +
    scale_color_manual(values = c("SPF Standard Deviations" = "goldenrod",
                                  "RoBERTa Probabilities" = "red")) +
    labs(title = "RoBERTa Probabilities vs. Professional Forecaster Disagreement",
         y = "Value",
         color = "Indicator") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size = 55, lineheight = 0.35),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 34),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 44),
          legend.text = element_text(size = 34),
          legend.position = "bottom")

ggsave(here("plots", "comparisons", "RoBERTa_vs_SPF.png"), RoBERTa_vs_SPF_plot, bg = "white",
       dpi = 320, height = 5, width = 8)

cor(RoBERTa_vs_SPF$sd, RoBERTa_vs_SPF$U_eq)
cor.test(RoBERTa_vs_SPF$sd, RoBERTa_vs_SPF$U_eq,
         alternative = "two.sided", method = "pearson", exact = TRUE)


head(RoBERTa_vs_SPF, 20)
