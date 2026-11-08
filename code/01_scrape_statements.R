# Setup --------------------------------------------------------------------------------------------
rm(list = ls())

librarian::shelf(tidyverse,
                 rvest,
                 purrr,
                 stringr,
                 RSelenium,
                 httr,
                 jsonlite,
                 lubridate,
                 readr,
                 here)

### NOTE!!!
### Note: To run this file, https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?name_of_publication=onetary%20policy%20statement
### must be open, and the link for in \verb{glue()} inside the "Get account URLs" must be adjusted
### (description in code chunk).

# Obtain Statement URLs ----------------------------------------------------------------------------

# Use XHR requests to find all meetings in chunk_*.json files
# Appearently the page is not static so rvest doesn't work
# With this following code we can pull titles and URLS from these .json-files

# base to construct absolute URLs
ecb_base <- "https://www.ecb.europa.eu"

# must include referer (from Inspector when you click on chunk_*.josn files) so page responds correctly
headers <- add_headers(
  Referer = "https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?name_of_publication=Monetary%20policy%20statement",
  `User-Agent` = "Mozilla/5.0"
)

# function to fetch chunk files and extract meetings
# IMPORTANT: part within /publications.en/.../data/ changes after every reload -> adjust manually
# For that: Load page, go to inspector, copy from one of the chunk URLs, insert to glue()
# Open URL: https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?name_of_publication=Monetary%20policy%20statement

parse_chunk_statements <- function(chunk_num) {
  url <- glue::glue("https://www.ecb.europa.eu/foedb/dbs/foedb/publications.en/1794060019/WOgWaVVM/data/0/chunk_{chunk_num}.json")
  
  res <- try(GET(url, headers), silent = TRUE)
  if (inherits(res, "try-error") || status_code(res) != 200) return(NULL)
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  items <- fromJSON(txt, simplifyVector = FALSE)
  
  out <- list()
  last_url <- NULL
  
  for (i in seq_along(items)) {
    item <- items[[i]]
    
    # If item is a URL starting with /press/press_conference/monetary-policy-statement
    # (filter all publications for policy statements)
    if (is.list(item) && length(item) > 0 && is.character(item[[1]]) &&
        str_starts(item[[1]], "/press/press_conference/monetary-policy-statement/")) {
      last_url <- paste0(ecb_base, item[[1]])
    }
    
    # Attach Title to URL
    if (!is.null(last_url) && is.list(item) && "Title" %in% names(item)) {
      out[[length(out) + 1]] <- tibble(
        title = item$Title,
        url = last_url
      )
      last_url <- NULL # reset to avoid reusing the same URL
    }
  }
  
  bind_rows(out)
}

# Loop through all chunks
all_statements <- map_dfr(0:80, parse_chunk_statements)

# Remove duplicates
all_statements <- distinct(all_statements)

all_statements <- all_statements |>
  filter(!grepl("ecb\\.ds", url))


# Add date column to meetings ----------------------------------------------------------------------
all_statements_date <- all_statements |>
  mutate(
    # Extract the 6 digits following 'mg' in the URL
    date_code = str_extract(url, "(?<=([/.])is)\\d{6}"),
    
    # Convert YYMMDD to Date
    date = as.Date(date_code, format = "%y%m%d")
  ) |>
  select(title, url, date) |>
  arrange(desc(date))

all_statements_date <- all_statements_date |>
  mutate(date = case_when(
    url == "https://www.ecb.europa.eu/press/press_conference/monetary-policy-statement/2021/html/ecb.sp210708~ab68c3bd9d.en.html" ~ as.Date("2021-07-08"),
    .default = date
  )) |>
  arrange(desc(date))


# Extract Texts ------------------------------------------------------------------------------------
scrape_statement_text <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Grab both sections and appendices (in document order)
    nodes <- page |> html_nodes("div.section, div.appendix")
    
    if (length(nodes) == 0) {
      return(tibble(url_text = url, text = NA_character_))
    }
    
    # Extract and clean text
    node_texts <- nodes |>
      map_chr(~ .x |> html_text(trim = TRUE) |> str_squish())
    
    # Keep only long blocks
    long_nodes <- node_texts[nchar(node_texts) >= 1000]
    
    text <- if (length(long_nodes) > 0) {
      paste(long_nodes, collapse = "\n\n")
    } else {
      NA_character_
    }
    
    tibble(
      url_text = url,
      text = text
    )
    
  }, error = function(e) {
    message("Failed to scrape: ", url)
    tibble(url_text = url, text = NA_character_)
  })
}


all_statements_text <- all_statements_date |>
  select(url, title, date) |>
  mutate(
    scraped = map(url, function(u) {
      scrape_statement_text(u) |> rename(scraped_text = text)
    })
  ) |>
  unnest(scraped) |>
  select(-url_text)

nchar(all_statements_text$scraped_text)

# Separators between statement and Q&A:
#   (***)
#   We are now at your disposal
#   We are now ready to take your questions
#   Transcript of the quetions asked and the answers given
#   I am now open to questions.


#  Splitting Statements and Q&As inside df ---------------------------------------------------------

split_pattern <- paste(
  "We are now ready to take your questions",
  "We are now at your disposal",
  "Transcript of the questions asked and the answers given",
  "\\*\\s*\\*\\s*\\*",
  sep = "|"
)


split_statement_qa <- function(scraped_text) {
  
  if (is.na(scraped_text)) {
    return(tibble(
      statement = NA_character_,
      qa = NA_character_,
      separator_found = FALSE
    ))
  }
  
  m <- str_locate(scraped_text, regex(split_pattern, ignore_case = TRUE))
  
  if (is.na(m[1])) {
    return(tibble(
      statement = scraped_text,
      qa = NA_character_,
      separator_found = FALSE
    ))
  }
  
  tibble(
    statement = str_trim(substr(scraped_text, 1, m[1] - 1)),
    qa = str_trim(substr(scraped_text, m[2] + 1, nchar(scraped_text))),
    separator_found = TRUE
  )
}


all_statements_split <- all_statements_text |>
  mutate(split = map(scraped_text, split_statement_qa)) |>
  unnest(split) |>
  select(-scraped_text)


# Clean Texts --------------------------------------------------------------------------------------
clean_ecb_text <- function(text) {
  
  text |>
    # Normalize line breaks & whitespace
    str_replace_all("[\r\n\t]+", " ") |>
    str_replace_all("\\s{2,}", " ") |>
    
    # Remove separators / artefacts
    str_replace_all("\\*\\s*\\*\\s*\\*", " ") |>
    str_replace_all("–|—", "-") |>   # normalize dashes
    
    # Remove non-breaking spaces
    str_replace_all("\u00A0", " ") |>
    
    str_trim()
}

all_statements_split_clean <- all_statements_split |>
  mutate(
    statement = clean_ecb_text(statement),
    qa = clean_ecb_text(qa)
  )

# This is debatable, but these are mostly non-policy statement press conferences or extraordinary
# events. Open for different solution. Sometimes, there's only Q&A tho, which would confound the
# rather neutral statement column, hence I delete them here
all_statements_split_clean <- all_statements_split_clean |>
  filter(!is.na(qa))


# Separate dfs for statements and Q&As -------------------------------------------------------------
statements_df <- all_statements_split_clean |>
  select(url, title, date, statement)

qa_df <- all_statements_split_clean |>
  filter(!is.na(qa)) |>
  select(url, title, date, qa)


# Save data ----------------------------------------------------------------------------------------

saveRDS(statements_df, here("data", "scraped", "raw_statements_Jan.rds"))
saveRDS(qa_df, here("data", "scraped", "raw_QandAs_Jan.rds"))