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
first_year <- tc %>% filter(!is.na(temp_anomaly)) %>% pull(year) %>% min()
last_year <- tc %>% filter(!is.na(temp_anomaly)) %>% pull(year) %>% max()
tc$temp_anomaly[tc$year==last_year] - tc$temp_anomaly[tc$year==first_year]

p <- tc %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) + geom_line(col='red')
p + geom_hline(yintercept = 0, col='blue')

p + labs(y='Temperature anomaly (degrees C)', title='Temperature anomaly relative to 20th century mean, 1880-2018') + 
  geom_text(aes(x=2000, y=0.05),label='20th century mean', col='blue') + geom_hline(yintercept = 0, col='blue')

p2 <- tc %>% filter(!is.na(temp_anomaly) & !is.na(land_anomaly) & !is.na(ocean_anomaly)) %>% ggplot() + 
  geom_line(size=1, aes(year, temp_anomaly), col='black') + geom_line(size=1, aes(year, land_anomaly), col='red') + 
  geom_line(size=1, aes(year, ocean_anomaly), col='blue')
p2 + labs(y='Temperature anomaly (degrees C)', title='Temperature anomaly relative to 20th century mean, 1880-2018') +
  geom_hline(yintercept = 0, col='green') + geom_text(aes(x=2005, y=0.05),label='20th century mean', col='darkgrey') +
  geom_text(aes(x=2020, y=1.4),label='Land', col='red') + geom_text(aes(x=2020, y=0.9),label='Global', col='black') +
  geom_text(aes(x=2020, y=0.7),label='Ocean', col='blue')
  theme(legend.position = 'right')
p2 + theme_void()

  
#about the greenhouse gases
head(gh)
summary(as.factor(gh$gas))

gh %>%
  ggplot(aes(year, concentration, col=gas)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

tc %>% filter(!is.na(carbon_emissions)) %>% ggplot(aes(year, carbon_emissions)) + geom_line() + geom_vline(xintercept = 1850, col='red') +
  geom_vline(xintercept = 1960, col='green') + geom_vline(xintercept = 2010, col='blue')

#about the global co2
head(hc)
summary(hc)
summary(as.factor(hc$source))

hc %>% ggplot(aes(year, co2, col=source)) + geom_line()
hc %>% ggplot(aes(year, co2, col=source)) + geom_line() + scale_x_continuous(trans = 'sqrt')
co2_time <- hc %>% ggplot(aes(year, co2, col=source)) + geom_line()

co2_time + xlim(-800000, -775000)
co2_time + xlim(-375000, -330000)
co2_time + xlim(-140000, -120000)
co2_time + xlim(-3000, 2018)
