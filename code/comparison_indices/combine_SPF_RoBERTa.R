# Match ECB monetary policy meetings to SPF survey rounds
# Rule: each meeting is assigned to the closest SPF deadline strictly AFTER the meeting,
#       i.e. the first survey round whose forecasters could have seen that statement.

rm(list = ls())

librarian::shelf(dplyr, readr, here)


# Load data ----------------------------------------------------------------------------------------

# ECB statements with uncertainty score (one row per meeting)
statements <- read_csv(here("data", "RoBERTa", "aggregate_RoBERTa_uncertainy_index.csv"),
                       show_col_types = FALSE) |>
  transmute(meeting_date = as.Date(date),
            u            = unc_mean_prob) |>   # chosen uncertainty measure
  arrange(meeting_date)

# SPF forecast dispersion per survey round
spf <- read_csv(here("data", "SPF", "SPF_aggregate_quarterly.csv"), show_col_types = FALSE) |>
  rename(quarter = survey_qtr, n_forecasters = n) |>
  mutate(quarter = gsub(" ", "", quarter))


# SPF response deadlines ---------------------------------------------------------------------------
# One deadline per round, 1999Q1-2025Q4, in chronological order.
# Quarters are assigned by position, so the checks below must pass.

spf_deadlines <- as.Date(c(
  "1999-02-12", "1999-05-03", "1999-08-06", "1999-11-03",
  "2000-02-09", "2000-05-05", "2000-08-04", "2000-11-02",
  "2001-02-01", "2001-05-03", "2001-08-03", "2001-10-29",
  "2002-02-04", "2002-04-22", "2002-07-22", "2002-10-23",
  "2003-01-27", "2003-04-24", "2003-07-23", "2003-10-28",
  "2004-01-28", "2004-04-26", "2004-07-26", "2004-10-21",
  "2005-01-26", "2005-04-22", "2005-07-22", "2005-10-24",
  "2006-01-23", "2006-04-26", "2006-07-21", "2006-10-20",
  "2007-01-24", "2007-04-23", "2007-07-18", "2007-10-18",
  "2008-01-18", "2008-04-18", "2008-07-18", "2008-10-17",
  "2009-01-20", "2009-04-20", "2009-07-17", "2009-10-19",
  "2010-01-19", "2010-04-20", "2010-07-19", "2010-10-19",
  "2011-01-18", "2011-04-19", "2011-07-19", "2011-10-18",
  "2012-01-20", "2012-04-19", "2012-07-19", "2012-10-22",
  "2013-01-22", "2013-04-19", "2013-07-19", "2013-10-22",
  "2014-01-24", "2014-04-25", "2014-07-24", "2014-10-23",
  "2015-01-13", "2015-04-07", "2015-07-06", "2015-10-06",
  "2016-01-11", "2016-04-06", "2016-07-06", "2016-10-06",
  "2017-01-10", "2017-04-07", "2017-07-07", "2017-10-06",
  "2018-01-11", "2018-04-10", "2018-07-06", "2018-10-05",
  "2019-01-11", "2019-03-22", "2019-07-05", "2019-10-07",
  "2020-01-13", "2020-04-07", "2020-07-06", "2020-10-09",
  "2021-01-11", "2021-04-12", "2021-07-05", "2021-10-11",
  "2022-01-13", "2022-04-04", "2022-07-05", "2022-10-06",
  "2023-01-11", "2023-04-07", "2023-07-05", "2023-10-05",
  "2024-01-10", "2024-03-21", "2024-07-04", "2024-10-03",
  "2025-01-09", "2025-04-04", "2025-07-03", "2025-10-07"
))

spf_quarters <- paste0(rep(1999:2025, each = 4), "Q", 1:4)

stopifnot(length(spf_deadlines) == length(spf_quarters),
          !is.unsorted(spf_deadlines, strictly = TRUE))

spf_calendar <- tibble(quarter = spf_quarters, spf_deadline = spf_deadlines) |>
  left_join(spf, by = "quarter")


# Match meetings to the next SPF deadline ----------------------------------------------------------
# closest(meeting_date < spf_deadline): the earliest deadline strictly after the meeting.
# Meetings after the last deadline in the calendar get NA.

meetings_matched <- statements |>
  left_join(spf_calendar, by = join_by(closest(meeting_date < spf_deadline)))

stopifnot(all(meetings_matched$meeting_date < meetings_matched$spf_deadline, na.rm = TRUE))


# Save ---------------------------------------------------------------------------------------------

write_csv(meetings_matched, here("data", "SPF", "RoBERTa_SPF_matched.csv"))
