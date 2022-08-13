options(digits = 3)
library(tidyverse)
library(dslabs)
tc <- temp_carbon
gh <- greenhouse_gases
hc <- historic_co2

data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

first_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()

last_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

ratio <- last_year/first_year
ratio

tc$carbon_emissions[tc$year==last_year]/tc$carbon_emissions[tc$year==first_year]
tc %>% filter(year %in% c(first_year, last_year)) %>% summarise(ratio = carbon_emissions[1]/carbon_emissions[2])

head(tc)
