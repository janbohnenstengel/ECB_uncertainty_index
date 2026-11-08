# Setup --------------------------------------------------------------------------------------------
rm(list = ls())

librarian::shelf(tidyverse,
    data.table,
    readr,
    stringr,
    dplyr,
    here,
    quiet = TRUE
)


# Load data ----------------------------------------------------------------------------------------

## read rds data
statements <- readRDS(here("data", "scraped", "raw_statements_Jan.rds")) |>
    rename(raw_statement = statement)

QandAs <- readRDS(here("data", "scraped", "raw_QandAs_Jan.rds")) |>
    rename(raw_qa = qa)

accounts <- readRDS(here("data", "scraped", "raw_ECB_policy_accounts.rds")) |>
    rename(release_date = date,
           raw_account = scraped_text)

## change rds data to csv
## change to csv
write_csv(statements, here("data", "scraped", "raw_statements_Jan.csv"))
write_csv(QandAs, here("data", "scraped", "raw_QandAs_Jan.csv"))
write_csv(accounts, here("data", "scraped", "raw_ECB_policy_accounts.csv"))


# Clean Statements ---------------------------------------------------------------------------------

statements <- statements |>
    mutate(id = row_number()) |>
    relocate(id, .before = date) |>
    select(-title) |>
    mutate(
        president = str_extract(
            raw_statement,
            "^[A-Za-z .'-]+(?=,\\s*President of the (ECB|European Central Bank))"
        )
    ) |>
    relocate(president, .before = raw_statement) |>
    mutate(
        raw_statement = case_when(
            str_detect(raw_statement, regex("Ladies and gentlemen", ignore_case = TRUE)) ~
                str_replace(raw_statement, regex("^.*?(Ladies and gentlemen)", ignore_case = TRUE), "\\1"),
            str_detect(raw_statement, regex("Good afternoon", ignore_case = TRUE)) ~
                str_replace(raw_statement, regex("^.*?(Good afternoon)", ignore_case = TRUE), "\\1"),
            str_detect(raw_statement, regex("The Vice President and I", ignore_case = TRUE)) ~
                str_replace(raw_statement, regex("^.*?(The Vice President and I)", ignore_case = TRUE), "\\1"),
            TRUE ~ raw_statement
        ),
        raw_statement = str_replace(raw_statement, "^\\s+", "")
    )

# 2017-04-27 to 2018-04-26 no data on the president
# 2014-10-26 speech form vice president Vítor Constâncio and needs to be cleaned
# 2003-09-04 speech from vice president Lucas Papademos
# 2002-01-03 code did not clean it correctly
# 2001-04-11 column president: Introductory statement Willem F. Duisenberg ; needs to be changed to Willem F. Duisenberg
# 2001-03-01 column president: Introductory statement Willem F. Duisenberg ; needs to be changed to Willem F. Duisenberg
# 2001-02-01 column president: Introductory statement Willem F. Duisenberg ; needs to be changed to Willem F. Duisenberg
# 2000-10-05 no information of the spokes person, but most likely Duisenberg
# 2000-06-08 no information of the spokes person, but most likely Duisenberg

statements <- statements |>
    mutate(
        president = case_when(
            is.na(president) & id >= 63 & id <= 71 ~ "Mario Draghi",
            is.na(president) & id >= 91 & id <= 93 ~ "Vice President Vítor Constâncio",
            is.na(president) & id >= 222 & id <= 224 ~ "Vice President Lucas Papademos",
            is.na(president) & id >= 256 & id <= 261 ~ "Willem F. Duisneberg",
            id >= 240 & id <= 255 & president != "Willem F. Duisenberg" ~ "Willem F. Duisenberg",
            TRUE ~ president
        )
    )

## clean the last raw_statements
# 37 ... The Vice-President and I
# 92 ... Welcome to his press conference
# 241 ... Thank you Mr. Rato

cut_rules <- tibble::tibble(
    id = c(37, 92, 241),
    phrase_statement = c(
        "The Vice-President and I",
        "Welcome to this press conference",
        "Thank you Mr. Rato"
    )
)

statements <- statements |>
    left_join(cut_rules, by = "id") |>
    mutate(
        clean_statement = if_else(
            !is.na(phrase_statement) & str_detect(raw_statement, fixed(phrase_statement)),
            {
                pos <- str_locate(raw_statement, fixed(phrase_statement))[, 1]
                str_sub(raw_statement, pos)
            },
            raw_statement
        )
    ) |>
    select(-phrase_statement, -raw_statement)

# save as csv
write_csv(statements, here("data", "clean_text", "clean_statements_Gerhard.csv"))


# Clean Q&As ---------------------------------------------------------------------------------------
QandAs <- QandAs |>
    mutate(id = row_number()) |>
    relocate(id, .before = date) |>
    select(-title)

# cut by phrase
qa_cut_phrases <- c(
    "Question:",
    "for question.",
    "Q:"
)

QandAs <- QandAs |>
    mutate(
        raw_qa = str_remove(
            raw_qa,
            regex(
                paste0("(?s)^.*(?:", paste(qa_cut_phrases, collapse = "|"), ")\\s*"),
                ignore_case = FALSE
            )
        ),
        raw_qa = str_replace(raw_qa, "^\\s+", ""),
        raw_qa = str_replace(raw_qa, "^\\.\\s*", "")
    )

#### still needs to be cleaned
# 50 # exclude everything before: , I am happy to take questions. (This was the introductory speech)
# 62 # exclud everything before : With that, I hand over the first question to Mr Speciale.] (Introduction said that some questions are answerde in Latvia, but not from the government board members)
# 220 # exclude everything before: (Translation from French)

QandAs <- QandAs |>
    mutate(
        raw_qa = case_when(
            id == 50 ~ str_replace(raw_qa, regex("(?s)^.*?I am happy to take questions\\.?\\s*", ignore_case = FALSE), ""),
            id == 62 ~ str_replace(raw_qa, regex("(?s)^.*?With that, I hand over the first question to Mr Speciale\\.?\\]?\\s*", ignore_case = FALSE), ""),
            id == 220 ~ str_replace(raw_qa, regex("(?s)^.*?\\(Translation from French\\)\\s*", ignore_case = FALSE), ""),
            TRUE ~ raw_qa
        ),
        raw_qa = str_squish(raw_qa)
    )
QandAs <- QandAs |> rename(clean_qa = raw_qa)

# Add presidents to the QandAs and accounts
identical(statements$date, QandAs$date)

QandAs <- QandAs |>
    left_join(
        statements |> select(date, president),
        by = "date"
    ) |>
    relocate(president, .after = date)

# save as csv
write_csv(QandAs, here("data", "clean_text", "clean_QandAs_Gerhard.csv"))



# Clean Accounts -----------------------------------------------------------------------------------

accounts <- accounts |>
    mutate(id = row_number()) |>
    relocate(id, .before = release_date) |>
    select(-url)


all(str_detect(accounts$raw_account, fixed("1. Review of financial")))
accounts |>
    filter(!str_detect(raw_account, fixed("1. Review of financial"))) |>
    select(id, release_date, raw_account)

# 4   does not contain "1. Review of financial" # cut everything before: 25 June 202528 August 2025
# 37  does not contain "1. Review of financial" # cut everything before: Main on Wednesday, 7 July 2021

accounts <- accounts |>
    mutate(
        raw_account = case_when(
            id == 4 ~ str_remove(
                raw_account,
                regex("(?s)^.*?25\\s+June\\s+2025\\s*28\\s+August\\s+2025\\s*", ignore_case = FALSE)
            ),
            id == 37 ~ str_remove(
                raw_account,
                regex("(?s)^.*?Main\\s+on\\s+Wednesday,\\s*7\\s+July\\s+2021\\s*", ignore_case = FALSE)
            ),
            str_detect(raw_account, regex("Review of financial", ignore_case = TRUE)) ~
                str_replace(
                    raw_account,
                    regex("(?s)^.*?(Review of financial)", ignore_case = TRUE),
                    "\\1"
                ),
            TRUE ~ raw_account
        ),
        raw_account = str_squish(raw_account)
    ) |>
    rename(clean_account = raw_account)

# add presidents to accounts
identical(statements$date, accounts$release_date)

# if  2025-12-18 < x => 2019-12-12  -> Christine Lagarde
# if x < 2019-12-12 -> Mario Draghi

assign_president_accounts <- function(date_vec) {
    case_when(
        date_vec >= as.Date("2019-12-12") & date_vec <= as.Date("2025-12-18") ~ "Christine Lagarde",
        date_vec < as.Date("2019-12-12") ~ "Mario Draghi",
        TRUE ~ NA_character_
    )
}
accounts <- accounts |>
    mutate(president = assign_president_accounts(release_date)) |>
    relocate(president, .after = release_date)

# save as csv
write_csv(accounts, here("data", "clean_text", "clean_accounts_Gerhard.csv"))
