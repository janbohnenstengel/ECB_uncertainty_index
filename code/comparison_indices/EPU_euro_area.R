# Setup --------------------------------------------------------------------------------------------
rm(list = ls())


librarian::shelf(readxl,
                 dplyr,
                 lubridate,
                 ggplot2,
                 here,
                 showtext,
                 sysfonts)

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# Load Data ----------------------------------------------------------------------------------------
EPU <- read_excel(here("data", "EPU", "Europe_Policy_Uncertainty_Data.xlsx")) |>
  select(-c(European_News_Index, UK_News_Index)) |>
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month)) |>
  filter(Year >= 1998)


# Create and Select Relevant Variables -------------------------------------------------------------
EPU$date <- make_date(
  year = EPU$Year,
  month = EPU$Month,
  day = 1
)

EPU <- EPU |>
  mutate(
    euro_epu = rowMeans(
      select(EPU, Germany_News_Index, France_News_Index,
                 Italy_News_Index, Spain_News_Index),
      na.rm = TRUE
    )) |>
  select(-Month) |>
  relocate(c(date, euro_epu), .after = Year)

write.csv(EPU, here("data", "EPU", "EPU_DE_FR_IT_ES.csv"))

# EPU Plot -----------------------------------------------------------------------------------------
EPU_plot <- EPU |>
    ggplot(aes(x = as.Date(date))) +
    geom_line(aes(y = euro_epu), alpha = 0.6) +
    labs(title = "Euro Area Economic Policy Uncertainty (Baker et al. 2016)",
         y = "EPU Index") +
    theme_bw(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(size=60),
          axis.title.y = element_text(size = 44),
          axis.text = element_text(size = 38),
          axis.title.x = element_blank())

ggsave(here("plots", "EPU.png"), EPU_plot, bg = "white", dpi = 320, height = 5, width = 8)
