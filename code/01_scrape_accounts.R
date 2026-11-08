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

here::here()
here::dr_here()

### NOTE!!!
### Note: To run this file, https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?name_of_publication=onetary%20policy%20statement
### must be open, and the link for in \verb{glue()} inside the "Get account URLs" must be adjusted
### (description in code chunk).
###

# Obtain Account URLs ------------------------------------------------------------------------------

# Use XHR requests to find all meetings in chunk_*.json files
# Appearently the page is not static so rvest doesn't work
# With this following code we can pull titles and URLS from these .json-files

# base to construct absolute URLs
ecb_base <- "https://www.ecb.europa.eu"

# must include referer (from Inspector when you click on chunk_*.josn files) so page responds correctly
headers <- add_headers(
  Referer = "https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?name_of_publication=Monetary%20policy%20account",
  `User-Agent` = "Mozilla/5.0"
)

# function to fetch chunk files and extract meetings
# IMPORTANT: part within /publications.en/.../data/ changes after every reload -> adjust manually
# For that: Load page, go to inspector, copy from one of the chunk URLs, insert to glue()
# Open URL: https://www.ecb.europa.eu/press/pubbydate/html/index.en.html?name_of_publication=Monetary%20policy%20account

parse_chunk_accounts_full <- function(chunk_num) {
  url <- glue::glue("https://www.ecb.europa.eu/foedb/dbs/foedb/publications.en/1794060019/WOgWaVVM/data/0/chunk_{chunk_num}.json")
  
  res <- try(GET(url, headers), silent = TRUE)
  if (inherits(res, "try-error") || status_code(res) != 200) return(NULL)
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  items <- fromJSON(txt, simplifyVector = FALSE)
  
  out <- list()
  last_url <- NULL
  
  for (i in seq_along(items)) {
    item <- items[[i]]
    
    # If item is a URL starting with /press/accounts/ (filter all publications for policy accounts)
    if (is.list(item) && length(item) > 0 && is.character(item[[1]]) &&
        str_starts(item[[1]], "/press/accounts/")) {
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

# Looping through all chunks (upper bound with tolerance in case number of chunks increases)
all_meetings <- map_dfr(0:80, parse_chunk_accounts_full)

# Remove duplicates
all_meetings <- distinct(all_meetings)


# Add date column to meetings ----------------------------------------------------------------------
all_meetings_date <- all_meetings |>
  mutate(
    # Extract the 6 digits following 'mg' in the URL
    date_code = str_extract(url, "(?<=([/.])mg)\\d{6}"),
    
    # Convert YYMMDD to Date
    date = as.Date(date_code, format = "%y%m%d")
  ) |>
  select(title, url, date) |>
  arrange(desc(date))

# Extract Texts ------------------------------------------------------------------------------------

scrape_account_text <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Grab all div.section nodes
    sections <- page |> html_nodes("div.section")
    
    text <- NA_character_
    
    # Try section 3 if it exists
    if (length(sections) >= 3) {
      text <- sections[[3]] |> html_text(trim = TRUE) |> str_squish()
    }
    
    # If section 3 is too short (<1000 chars), try section 4
    if (is.na(text) || nchar(text) < 1000) {
      if (length(sections) >= 4) {
        text <- sections[[4]] |> html_text(trim = TRUE) |> str_squish()
      }
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


all_meetings_text <- all_meetings_date |>
  select(url, title, date) |>
  mutate(
    scraped = map(url, function(u) {
      scrape_account_text(u) |> rename(scraped_text = text)
    })
  ) |>
  unnest(scraped) |>
  select(-url_text)

# Faulty Meetings (no/too little text):
#   2023-01-19 (No text) -> need section 4
#   2017-04-06 (only infoon place and time) -> need section 4 as well for all these
#   until 2016-02-18


# Clean text ---------------------------------------------------------------------------------------

clean_ecb_text <- function(text) {
  
  text |>
    # Normalize line breaks & whitespace
    str_replace_all("[\r\n\t]+", " ") |>
    str_replace_all("\\s{2,}", " ") |>
    
    # Remove separators / artefacts
    str_replace_all("\\*\\*\\*", " ") |>
    str_replace_all("–|—", "-") |>   # normalize dashes
    
    # Remove non-breaking spaces
    str_replace_all("\u00A0", " ") |>
    
    str_trim()
}

all_meetings_text <- all_meetings_text |>
  mutate(scraped_text = clean_ecb_text(scraped_text))


# Save data ----------------------------------------------------------------------------------------

saveRDS(all_meetings_text, here("data", "scraped", "raw_ECB_policy_accounts.rds"))

tail(all_meetings_text)
