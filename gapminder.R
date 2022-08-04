library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
head(gapminder)
g <- gapminder

g %>% filter(year==2015 & country %in% c('Sri Lanka', 'Turkey')) %>% select(c('country', 'infant_mortality'))
summary(g['year'])

ds_theme_set()
g %>% filter(year==2015) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point()

g %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(.~year)

g %>% filter(year %in% c(1962, 1970, 1980, 1990, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_wrap(.~year, ncol = 2)

g %>% filter(year %in% c(1962, 1970, 1980, 1990, 2012) & continent %in% c('Europe', 'Asia')) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() + facet_wrap(.~year, ncol = 2)

g %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(continent~year)

summary(g['country'], maxsum = 1000)
countries <- c('South Korea', 'Germany')
g %>% filter(country %in% countries) %>% ggplot(aes(year, life_expectancy, col=country)) + geom_line()

labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
g %>% filter(country %in% countries) %>% ggplot(aes(year, life_expectancy, color=country)) + geom_line() + 
  geom_text(data = labels, aes(x, y, label=country)) + theme(legend.position = 'none')

g <- g %>% mutate(dpd = gdp/population/365)
g <- g %>% mutate(log2_dpd = log2(dpd))
head(g, 10)
g %>% filter(year==1970 & !is.na(log2_dpd)) %>% ggplot(aes(log2_dpd)) + geom_histogram

g %>% filter(year==1970 & !is.na(dpd)) %>% ggplot(aes(dpd)) + geom_histogram(binwidth = 1, color = 'black') + 
  scale_x_continuous(trans = 'log2')

