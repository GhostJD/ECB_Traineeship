rm(list = ls())

library(tidyverse)
library(readxl)
#library(seasonal)
#library(seastests)
library(lubridate)
#library(tseries)

transformations <- read_excel("Variables_table.xlsx", sheet = "transformations") %>% 
  tibble()

#--------------------#
# Databáze z kroku 1 #
#--------------------#

df <- read_tsv("data_agg/wholeTSV.tsv") %>% 
  select(date, ticker, value) %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(transformations, by = c("ticker" = "ticker")) %>% 
  distinct()

#-------------------------------------------------------------------------------------#
# Sezónní očištění NSA monthly dat - není potřeba, protože NSA data zatím nepoužíváme #
#-------------------------------------------------------------------------------------#

#seasonal adjustment funkce
#source(here::here("functions", "functions_sa.R"))

#všechny NSA variables naráz
#dfwideSA <- df %>% 
#  select(date, ticker, value, season) %>%
#  filter(season == "NSA") %>%
#  arrange(date) %>% 
#  pivot_wider(names_from = ticker, values_from = value) %>%
#  select(-season) %>% 
#  seasonal_adjustment()

#~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.#
#                                #
# For cyklus na každou proměnnou #
#                                #
#~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.#

#defaultní nastavení - w/o MA filter, m -> q průměry 3 měsíců, log, difference

#----------------------------#
# Transformace měsíčních dat #
#----------------------------#

#seznam tickerů měsíčních proměnných
tickers_distinct_m <- df %>% 
  filter(frequency == 12) %>% 
  distinct(ticker)

df_out_m <- data.frame(date = as.Date(character()), ticker = character(), value = numeric(), stringsAsFactors = FALSE)

for (var in pull(tickers_distinct_m, ticker)) {
  message(paste0("Transforming ",var))
  
  dftemp <- df %>% 
    
    #čtvrtletní průměr
    filter(ticker == as.character(var)) %>% 
    arrange(date) %>% 
    mutate(year = lubridate::year(date), quarter = lubridate::quarter(date)) %>%
    group_by(year, quarter, logarithm, difference, filt, season, frequency, ticker) %>%
    summarise(date = min(date), value = mean(value)) %>% 
    select(date, ticker, value, logarithm, difference, filt, season, frequency) %>% 
    ungroup() %>% 
    select(-c(year, quarter)) %>% 
      
    #MA filter - první hodnota = průměr prvních dvou hodnot, poslední = průměr posledních dvou
    #mutate(filt = if_else(row_number() == 1, "1start", as.character(filt))) %>% 
    #mutate(filt = if_else(row_number() == 2, "2start", as.character(filt))) %>% 
    #mutate(value = if_else(filt == "1start", value, value)) %>%
    #mutate(value = if_else(filt == "2start", (value + lag(value, n = 1))/2, value)) %>%
    #mutate(value = if_else(filt == 3, (lag(value, n = 2) + lag(value) + value)/3, value)) %>% 
    
    #logarithm
    mutate(value = if_else(logarithm == 1, log(value), value)) %>% 
    
    #difference
    mutate(value = if_else(logarithm == 1 & difference == 1, 100*(value - lag(value)), value)) %>% 
    mutate(value = if_else(logarithm == 0 & difference == 1, value - lag(value), value)) %>%
    select(date, ticker, value)
    
  df_out_m <- bind_rows(df_out_m, dftemp)
}

#-------------------------------#
# Transformace čtvrtletních dat #
#-------------------------------#

tickers_distinct_q <- df %>% 
  filter(frequency == 4) %>% 
  distinct(ticker)

df_out_q <- data.frame(date = as.Date(character()), ticker = character(), value = numeric(), stringsAsFactors = FALSE)

for (var in pull(tickers_distinct_q, ticker)) {
  message(paste0("Transforming ",var))
  
  dftemp <- df %>% 
    filter(ticker == as.character(var)) %>% 
    arrange(date) %>% 
    
    #logarithm
    mutate(value = if_else(logarithm == 1, log(value), value)) %>% 
    
    #difference
    mutate(value = if_else(logarithm == 1 & difference == 1, 100*(value - lag(value)), value)) %>% 
    mutate(value = if_else(logarithm == 0 & difference == 1, value - lag(value), value)) %>%
    select(date, ticker, value)
  
  df_out_q <- bind_rows(df_out_q, dftemp)
}

data_final <- bind_rows(df_out_m, df_out_q) %>% 
  drop_na()

#seznam proměnných v modelu
variables <- data_final %>% 
  distinct(ticker)

write_tsv(data_final, file = "data_agg/finalTSV.tsv", append = FALSE)

# kontrola že chybí pouze 1st u diferencovaných řad
#a <- df_out %>% 
#filter(is.na(value))

#--------------#
# Stacionarita #
#--------------#

for (var in pull(tickers_distinct, ticker)) {
  df_adf <- data_final %>% 
    filter(ticker == as.character(var)) %>% 
    arrange(date) %>% 
    select(value)
  
  x <- as.vector(df_adf$value)
  
  result <- sprintf("%s: %s", as.character(var), adf.test(x)$p.value)
  print(result)
}

data_non_station <- data_final %>% 
  filter(ticker %in% c("cpi_all", "cpi_energy", "ppi_all_excl_energy", "us_umcsent", "us_oecd_consop", "vacancies", "ESI_retail", "ESI_services", "ESI_construction"))
ggplot(data_non_station, aes(x = date, y = value)) + geom_line(aes(color = ticker))

#---------------------------------------------------------------------------------#
# Kontrola - pozor na řady z data request, které nejsou v sheetu s transformacemi #
#---------------------------------------------------------------------------------#

#Sedí to!

#for (var in pull(tickers_distinct, ticker)) {
#  x <- df %>% 
#    filter(ticker == as.character(var))
#  result <- sprintf("%s: %s", as.character(var), as.character(nrow(x)))
#  print(result)
#}

#for (var in pull(tickers_distinct, ticker)) {
#  x <- a %>% 
#    filter(ticker == as.character(var))
#  result2 <- sprintf("%s: %s", as.character(var), as.character(nrow(x)))
#  print(result2)
#}
