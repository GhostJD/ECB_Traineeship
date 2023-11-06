rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)

# evaluation period je Q1 2018 - Q2 2023 (22 čtvrtletí)
# vytváříme tedy list s 22 datasety

transformations <- read_excel("Variables_table.xlsx", sheet = "transformations") %>% 
  tibble()

# raw data
df <- read_tsv("data_agg/wholeTSV.tsv") %>% 
  select(date, ticker, value) %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(transformations, by = c("ticker" = "ticker")) %>% 
  distinct() %>% 
  filter(frequency == 12 | ticker == "gdp") %>% # dočasně odstranit quarterly proměnné
  select(-c(filt, season)) %>%
  filter(date >= as.Date("1998-10-01")) %>%
  filter(date < as.Date("2023-07-01")) # konec evaluation period

#GDP for OOS RMSE
gdp_test <- df %>%
  filter(ticker == "gdp") %>%
  mutate(value = 100 * (log(value) - log(lag(value)))) %>%
  select(date, value) %>% 
  rename(gdp = value)

tic()

#--------------------------------------------------------------#
# (I) - 1 monthly variables available for the nowcasted period #
#--------------------------------------------------------------#

end_dates_1 <- seq(as.Date("2018-01-01"), by = "quarter", length.out = 22)

#main function
create22_baseline <- function(end_date) {
  # GDP
  gdp_data <- df %>%
    filter(ticker == "gdp", date <= end_date) %>%
    mutate(
      value = case_when(
        logarithm == 1 & difference == 1 ~ 100 * (log(value) - log(lag(value))),
        logarithm == 1 ~ log(value),
        difference == 1 ~ value - lag(value),
        TRUE ~ value
      )
    ) %>%
    select(date, ticker, value)
  
  # monthly
  df_temp <- df %>%
    filter(date <= end_date, frequency == 12) %>%
    arrange(date, ticker) %>%
    group_by(ticker, year = lubridate::year(date), quarter = lubridate::quarter(date), logarithm, difference) %>%
    summarise(
      date = if (date == end_date) end_date else min(date),
      value = if (date == end_date) first(value) else mean(value)
    ) %>%
    ungroup() %>%
    mutate(
      value = case_when(
        logarithm == 1 & difference == 1 ~ 100 * (log(value) - log(lag(value))),
        logarithm == 1 ~ log(value),
        difference == 1 ~ value - lag(value),
        TRUE ~ value
      )
    ) %>%
    select(date, ticker, value)
  
  df_combined <- bind_rows(gdp_data, df_temp) %>%
    arrange(date, ticker)
  
  # wide format
  df_wide <- df_combined %>%
    pivot_wider(names_from = ticker, values_from = value) %>% 
    mutate(gdp = if_else(row_number() == n(), NA_real_, gdp)) %>%
    slice(-1) %>% 
    dplyr::select_if(~ !is.na(.[1]))
  
  return(df_wide)
}

dfs22_t1_baseline <- map(end_dates_1, create22_baseline)

# tibble
tibble22_t1_baseline <- tibble(end_date = end_dates_1, data = dfs22_t1_baseline)


#---------------------------------------------------------------#
# (II) - 2 monthly variables available for the nowcasted period #
#---------------------------------------------------------------#

end_dates <- seq(as.Date("2018-02-01"), by = "quarter", length.out = 22)

dfs22_t2_baseline <- map(end_dates, create22_baseline)

#tibble
tibble22_t2_baseline <- tibble(end_date = end_dates_1, data = dfs22_t2_baseline)

#----------------------------------------------------------------#
# (III) - 3 monthly variables available for the nowcasted period #
#----------------------------------------------------------------#

end_dates <- seq(as.Date("2018-03-01"), by = "quarter", length.out = 22)

dfs22_t3_baseline <- map(end_dates, create22_baseline)

#tibble
tibble22_t3_baseline <- tibble(end_date = end_dates_1, data = dfs22_t3_baseline)

toc()

#save, protože se to vytváří dlouho

save(tibble22_t1_baseline, file = "oos_t1_baseline")
save(tibble22_t2_baseline, file = "oos_t2_baseline")
save(tibble22_t3_baseline, file = "oos_t3_baseline")


